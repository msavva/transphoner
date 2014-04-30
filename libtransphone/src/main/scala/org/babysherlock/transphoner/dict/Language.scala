package org.babysherlock.transphoner.dict

import org.babysherlock.transphoner.phonetics.{IPA, StressType, Tone, Phone}
import org.babysherlock.transphoner.{Constants, TrieHelper}
import org.babysherlock.nlp.StanfordCorenlpWrapper
import org.babysherlock.util.{LRUCache, CSVFile, IOUtils}
import edu.stanford.nlp.tokensregex.matcher.TrieMap
import scala.collection.mutable
import scala.ref.SoftReference

/**
 * Language containing an inventory of phones and words (with glosses and pronunciations)
 * @author Manolis Savva
 */
trait Language {
  def id: String
  def uwnCode: String = null
  def name: String
  def phoneInventory: Set[Phone]
  def words: Set[Word]
  def wordsMostFrequent: IndexedSeq[Word]
  def gloss: Word => String
  def pronunciation: Word => Seq[Phone]
  def wordFreq: Word => Double
  def wordCount: Word => Double
  def wordRank: Word => Int = word => 0
  // lookup word using orthography
  def lookup(str: String) = toWord(str)
  def toWord(str: String): Word
  def toWords: String => Seq[Word] = str => {
    val wordPairs = toWordPairs(str)
    for ((str,word) <- wordPairs) {
      if (word == null) {
        System.err.println("Unknown word: " + str)
      }
    }
    wordPairs.map( p => p._2 ).filter( x => x != null )
  }
  def toWordPairs: String => Seq[(String,Word)] = str => {
    // Simple white string tokenizer
    val strs = str.trim.split("\\s+")
    if (segmenter != null) {
      strs.flatMap( s => segmenter.segment(this,s) )
    } else {
      strs.map(s => (s, toWord(s)))
    }
  }
  def segmenter: Segmenter = null
  val tones: Map[String, Tone] = Map()
  def hasTones: Boolean = tones.nonEmpty
  lazy val toneLookupByIpa = tones.values.map( t => t.ipa -> t ).toMap
  def lookupTone(ipa: String): Tone = toneLookupByIpa.getOrElse(ipa, null)

  def charTrie: TrieMap[Char,Set[Word]] = {
    if (cachedCharTrie == null || cachedCharTrie.get.isEmpty) {
      // re-get trie
      cachedCharTrie = new SoftReference(TrieHelper.toCharTrie( words.toSeq ))
    }
    cachedCharTrie.apply()
  }
  private var cachedCharTrie: SoftReference[TrieMap[Char,Set[Word]]] = null
  override def toString: String = id + ":" + name
  def nonLatinScript: Boolean = false
}

class IPALanguage extends Language {
  val id = "IPA"
  val name = "IPA"
  val phoneInventory = IPA.phones.toSet
  val words = Set[Word]()
  val wordsMostFrequent = IndexedSeq[Word]()
  val gloss: (Word) => String = w => w.nativeWord
  val pronunciation: (Word) => Seq[Phone] = word => IPA.strToPhoneSeq(word.ipa)
  val wordFreq = Map[Word,Double]()
  val wordCount = Map[Word,Double]()
  // TODO: Check str consists only of IPA symbols?
  def toWord(str: String) = new IPAWord(str)
}

object IPALanguage extends IPALanguage {}

class EnhancedDictLanguage(val self: DictLanguage) extends Language {
  def id: String = self.id

  def name: String = self.name

  def phoneInventory: Set[Phone] = self.phoneInventory

  def words: Set[Word] = self.words

  def wordsMostFrequent: IndexedSeq[Word] = self.wordsMostFrequent

  def gloss: (Word) => String = self.gloss

  def pronunciation: (Word) => Seq[Phone] = self.pronunciation

  def wordFreq: (Word) => Double = self.wordFreq

  def wordCount: (Word) => Double = self.wordCount

  def toWord(str: String): Word = {
    var w = self.toWord(str)
    if (id == "EN") {
      if (w == null) {
        // Let's try to lemmatize (only do so for english)
        // TODO: Have different versions of the pipeline that we can use for different language
        // TODO: Maybe this should go into the segmenter...
        val tokens = StanfordCorenlpWrapper.getTokens(str)
        if (tokens.size == 1) {
          val token = tokens.get(0)
          val lemma = token.lemma()
          w = self.toWord(lemma)
        }
      }
      if (w == null) {
        if (str.endsWith("s")) {
          w = self.toWord(str.slice(0, str.length-1))
        }
      }
    }
    w
  }

  override def uwnCode = self.uwnCode
  override def segmenter = self.segmenter
  override def nonLatinScript = self.nonLatinScript
  override def wordRank: (Word => Int) = self.wordRank

  override val tones = self.tones
}

class DictLanguage(val id: String,
                   val name: String,
                   pronunciationFile: String,
                   wordFreqFile: String = null,
                   tonesFile: String = null,
                   glossesFile: String = null,
                   override val uwnCode: String = null,
                   override val segmenter: Segmenter,
                   override val nonLatinScript: Boolean = false,
                   storeGlosses: Int = 0) extends Language {
  // Set of words in this dictionary
  private val idToWords: Map[String,Word] = loadFromTsv(pronunciationFile, storeGlosses, Option(glossesFile))
  override lazy val words = idToWords.values.toSet

  // Lookup of native orthographies to word
  private val nativeToWords: Map[String,Seq[Word]] = {
    val v = words.flatMap( w => (w.orthographies ++ w.orthographies.map( x => x.toLowerCase)).map( orth => (orth,w) ))
      .groupBy(_._1).mapValues( x => x.toSeq.sortBy( p => if (p._2.orthographies.head == p._1) -1 else 1 ).map( p => p._2) )
//    v.filter( p => p._2.size > 1).foreach {
//      x => println(x._1 + " -> " + x._2.mkString(",") )
//    }
    v
  }
  //private val wordPronunciations: Map[String,Seq[String]] = loadFromTsv(pronunciationFile)
  //private val pronunciationToWords: Map[String,Set[String]] = invertMap(wordPronunciations)
  private lazy val wordFreqs: Map[String,Pair[Double,Double]] = {
    if (wordFreqFile!=null) {
      val counts = IOUtils.loadMap(wordFreqFile,' ').mapValues(_.toDouble)
      val total = counts.map(_._2).sum
      counts.map(c => (c._1, (c._2, c._2 / total)) )
    } else Map()
  }

  //val words = nativeToWords.values.toSet
  val phoneInventory = words.flatMap( w => w.pronunciations.flatten )
  val gloss: (Word) => String = w => w.gloss
  val pronunciation: (Word) => Seq[Phone] = word => word.phones

  override val tones: Map[String, Tone] = if (tonesFile != null) loadTones(tonesFile) else Map()

  private def wordFreqLookup(w: Word): (Double,Double) = {
    var res = wordFreqs.get(w.nativeWord)
    if (res.isEmpty) {
      res = wordFreqs.get(w.nativeWord.toLowerCase)
    }
    if (res.isEmpty) {
      w.orthographies.map( x => wordFreqs.getOrElse(x, (0.0, 0.0) ) ).fold( (0.0,0.0))(
        (d1, d2) => (math.max(d1._1,d2._1), math.max(d1._2, d2._2))
      )
    } else {
      res.get
    }
  }
  val wordFreq: (Word => Double) = w => wordFreqLookup(w)._2
  val wordCount: (Word => Double) = w => wordFreqLookup(w)._1
  override val wordRank: (Word => Int) = w => wordsMostFrequent.indexOf( w ) + 1
  override def toWord(str: String) = nativeToWords.getOrElse(str, nativeToWords.getOrElse(str.toLowerCase, Seq())).headOption.getOrElse(null)
  override lazy val wordsMostFrequent = words.toSeq.map(w => (w, w.count)).sortBy(-_._2).map(p => p._1).toIndexedSeq

  def printNonIPAchars() {
    val nonIPAchars: collection.mutable.HashMap[Char,Int] = new collection.mutable.HashMap()
    words.foreach(word => word.fullIpas.foreach( ipa => IPA.strToPhoneSeq( ipa, nonIPAchars) ) )
    println(nonIPAchars.toSeq.sortBy{case(c,i) => i}.mkString("\n"))
  }

  // Load set of words from tsv
  // storeGlosses: maxNumber of glosses to store
  // glossesFile: file from which to fetch the glosses
  private def loadFromTsv(file: String, storeGlosses: Int = 0, glossesFile: Option[String] = None): Map[String,Word] = {
    val tsvFile = new CSVFile(file,separator='\t',quote='\0')
    val allGlosses = if (storeGlosses > 0) glossesFile.map( f => new TsvDictGlosses(f) ) else None
    val myMap = new mutable.HashMap[String, DictWord ]
    //println("read " + tsvFile.size)
    tsvFile.foreach(row => {
      if (row.size > 1) {
        val word = row(0)
        // If word already contains comma, then pronunciation typically replicates it so don't use for splitting
        val ipas = if (word.contains(',')) Array(row(1)) else row(1).split(',')
        // TODO: Handle multiple glosses and POS
        val gloss: String =
          if (storeGlosses > 0) {
            if (row.size > 2) {
              // TODO: cleanup and separate gloss and pos
//              val g = row(2)
//              if (g.startsWith('/) && g.endsWith('/')) {
//                val defs = g.substring(1, g.length()-1).split('/')
//              }
              row(2)
            } else {
              // get glosses from all glosses
              val glosses = allGlosses.map( g => g.getSenses(word) ).getOrElse(Seq())
              if (glosses.nonEmpty) glosses.head.gloss else word
            }
          } else word
        // Note this is the chinese parenthesis
        val parenPos = word.indexOf("（")
        val orthos = if (parenPos > 0) {
          val endParenPos = word.indexOf("）")
          val word1 = word.substring(0, parenPos).trim()
          val word2 = word.substring(parenPos+1, endParenPos).trim()
          //println("Got word1=" + word1 + ", word2=" + word2)
          Set(word1,word2)
        }
        else Set(word)
        if (word.nonEmpty) {
          val entry = myMap.getOrElse(word, null)
          if (entry == null) {
            myMap.put(word,
                      new DictWord(nativeWord = word,
                         lang = this,
                         fullIpas = ipas,
                         orthographies = orthos,
                         gloss = gloss) )
          } else {
            myMap.put(word,
              new DictWord(nativeWord = entry.nativeWord,
                lang = this,
                fullIpas = entry.ipas ++ ipas,
                orthographies = entry.orthographies,
                gloss = entry.gloss) )
          }
        } else {
          println("WARNING: word is empty, ignore " + row(0) + ":" + row(1) + " from " + file)
        }
      }
    })
    println("Loaded " + myMap.size + " words")
    myMap.toMap
  }

  private def loadTones(file: String): Map[String, Tone] = {
    val csvFile = new CSVFile(file)
    csvFile.map(row => {
      val name = row(0).trim()
      val ipa = row(1).trim()
      val desc = row(2).trim()
      val stressType =
        if (row.length > 3) StressType.valueOf( row(3).trim() )
        else StressType.none
      name -> Tone(name, ipa, desc, stressType)
    }).toMap
  }

//  private def invertMap(map: Map[String,Seq[String]]) = {
//    val ipaToWordTuples = map.map{case(word,ipas) => ipas.map(ipa => ipa -> word)}.flatMap(_.iterator)
//    ipaToWordTuples.groupBy(_._1).mapValues(_.map(_._2).toSet).filter{case(k,v) => v.nonEmpty}
//  }
}

object DictLanguage {
  val languages: Set[String] = Constants.LANGS.keySet
  def apply(lang: String): DictLanguage =
    if (Constants.LANGS.contains(lang)) apply(Constants.LANGS(lang)) else null

  def apply(opts: Map[String,String]): DictLanguage = {
    val segmenter = Segmenter(opts)
    new DictLanguage(opts("id"), opts("name"), opts("dict"), opts("freq"), opts.getOrElse("tones", null),
      opts.getOrElse("glosses", null), opts.getOrElse("iso6393", null),
      segmenter, opts.getOrElse("nonLatinScript", "false").toBoolean, Language.getNumberOfGlossesToStore)
  }

  lazy val PHONE_INVENTORY = IOUtils.getLines(Constants.PHONE_INVENTORY_TSV).map(l => {
    val f = l.split('\t')
    f(0) -> f(2).split(',').toSet
  }).toMap
}

object Language {
//  val languages: Set[String] = Set("IPA") ++ DictLanguage.languages
  val languages: Set[String] = DictLanguage.languages
  private val languagesCache = LRUCache[String, Language](10)
  private var storeGlosses: Int = 0

  def setNumberOfGlossesToStore(nGlosses: Int) {
    // Clearing cache
    languagesCache.clear()
    storeGlosses = nGlosses
  }

  def getNumberOfGlossesToStore = storeGlosses

  def name(lang: String): String = {
    if ("IPA" == lang) "IPA"
    else Constants.LANGS.getOrElse(lang, Map())("name")
  }

  def fromIso6393(code: String): Language = {
    val langs = Constants.LANGS.filter( opts => code == opts._2.getOrElse("iso6393", "") )
    if (langs.nonEmpty) apply(langs.keys.head)
    else null
  }

  def apply(lang: String): Language = {
    var res = languagesCache.get(lang)
    if (res.isEmpty) {
      res = Option(createLanguage(lang))
      if (res.isDefined) languagesCache.put( lang, res.get )
    }
    res.getOrElse(null)
  }

  private def createLanguage(lang: String) = {
    if ("IPA" == lang) IPALanguage
    else if ("EN" == lang) new EnhancedDictLanguage(DictLanguage(lang))
    else DictLanguage(lang)
  }

  lazy val EN = Language("EN")
  lazy val FR = Language("FR")
  lazy val FR2 = Language("FR2")
  lazy val ZH = Language("ZH")
  lazy val JA = Language("JA")
  lazy val DE = Language("DE")
}


