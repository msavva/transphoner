package org.babysherlock.nlp

import net.sf.extjwnl.data.list.PointerTargetTreeNode
import net.sf.extjwnl.dictionary.Dictionary

import net.sf.extjwnl.data._
import scala.collection.JavaConversions._
import java.io.{PrintWriter, StringBufferInputStream, FileInputStream}
import edu.stanford.nlp.ling.tokensregex.PhraseTable
import edu.stanford.nlp.ling.tokensregex.PhraseTable.StringList
import edu.stanford.nlp.util.logging.Redwood
import edu.stanford.nlp.stats.{Counter, Counters}
import org.babysherlock.util.IOUtils
import scala.collection.mutable
import scala.util.matching.Regex

/**
 * Wrapper around the JWNL package.
 *
 * @author Manolis Savva
 * @author Angel Chang
 */
class WordNetWrapper(val dictionary: Dictionary) extends WordNetSynsetId {
  val solrIndex: WordNetSolrIndex = new WordNetSolrIndex(dictionary)
  val phraseTables = POS.getAllPOS.map( pos => pos -> new PhraseTable(true,true,true)).toMap

  def this(conffile: String) { this(Dictionary.getInstance(new FileInputStream(conffile))) }

  def this(wordNetConfFile: String, dictFilePath: String) {
    this(
      if (dictFilePath != null) {
        var properties: String = IOUtils.fileSource(wordNetConfFile).mkString
        properties = properties.replace("data/wn31dict", dictFilePath)
        Dictionary.getInstance(new StringBufferInputStream(properties))
      }
      else {
        Dictionary.getInstance(new FileInputStream(wordNetConfFile))
      }
    )
  }

  def initPhraseTable(pos: POS) {
    val phraseTable = phraseTables(pos)
    val words = dictionary.getIndexWordIterator(pos)
    for (word <- words) {
      phraseTable.addPhrase(word.getLemma, word.getLemma)
      if (word.getLemma.contains(' ')) {
        phraseTable.addPhrase(word.getLemma.replaceAll(" ", ""), word.getLemma)
      }
    }
  }

  // Lookup IndexWord with given pos and text in dictionary
  def lookupPhraseExact(pos: POS, text: String): IndexWord = {
    val indexWord = dictionary.lookupIndexWord(pos, text)
    if (indexWord != null) {
      indexWord
    } else {
      val phraseTable = phraseTables(pos)
      val phrase = phraseTable.lookupNormalized(text)
      if (phrase != null) dictionary.getIndexWord(pos, phrase.getTag) else null
    }
  }

  def lookupPhraseExact(text: String): Set[IndexWord] = {
    val indexWordSet = dictionary.lookupAllIndexWords(text)
    if (indexWordSet != null) {
      indexWordSet.getIndexWordCollection.toSet
    } else {
      val matched = POS.getAllPOS.map(
        pos => {
          val phraseTable = phraseTables(pos)
          val phrase = phraseTable.lookupNormalized(text)
          if (phrase != null) dictionary.getIndexWord(pos, phrase.getTag) else null
        })
      matched.filter(w => w != null).toSet
    }
  }

  // Given pos and a string, lookup phrases in wordnet that occurs in the text
  def lookupPhrase(pos: POS, text: String, minTokens: Int = 1): Set[IndexWord] = {
    val phraseTable = phraseTables(pos)
    val matches = phraseTable.findAllMatches(text)
    val nonoverlapping = phraseTable.findNonOverlappingPhrases(matches)
    val filtered = nonoverlapping.filter( p => p.getTokenEnd - p.getTokenBegin >= minTokens)
    filtered.map( p => dictionary.getIndexWord(pos, p.getPhrase.getTag) ).toSet
  }

  // Given pos and pretokenized text, lookup phrases in wordnet that occurs in the text
  def lookupPhraseTokenized(pos: POS, words: Seq[String], minTokens: Int = 1): Set[IndexWord] = {
    val phraseTable = phraseTables(pos)
    val matches = phraseTable.findAllMatches(new StringList(words))
    val nonoverlapping = phraseTable.findNonOverlappingPhrases(matches)
    val filtered = nonoverlapping.filter( p => p.getTokenEnd - p.getTokenBegin >= minTokens)
    filtered.map( p => dictionary.getIndexWord(pos, p.getPhrase.getTag) ).toSet
  }

  def selectSynset(pos: POS, synsets: Seq[Synset], termCounts: Counter[String], useSolrScore: Boolean = false,
                   reorder: Ordering[(Synset,Double)] = null): Synset = {
    if (synsets != null && synsets.isEmpty) null
    else if (synsets != null && synsets.size == 1) synsets.head
    else {
      if (useSolrScore) {
        val synsetsSet = if (synsets != null) synsets.toSet else null
        //val res = solrIndex.queryText(words, synsetsSet)
        //val termCounts = Counters.asCounter(words)
        val res = solrIndex.getOrderedSynsetSimilarity(termCounts, "text", synsets.toSet)

        var filtered = res
        if (synsets != null) filtered = filtered.filter{ case (synset,score) => synsetsSet.contains(synset) }
        if (pos != null) filtered = filtered.filter { case (synset,score) => pos.equals(synset.getPOS) }
        // Re-order results with same score using specified reordering
        val sorted = if (reorder != null) filtered.sorted(reorder) else filtered

        Redwood.log(Redwood.DBG, "query=" + Counters.toBiggestValuesFirstString(termCounts))
        sorted.foreach( s =>
          Redwood.log(Redwood.DBG, s)
        )
        if (sorted.isEmpty) { if (synsets ==null) null else synsets.head }
        else sorted.head._1
      } else {
        if (synsets ==null) null else synsets.head
      }
    }
  }

  def getSynsets(): Iterable[Synset] = {
    (for (pos <- POS.getAllPOS) yield {
      dictionary.getSynsetIterator(pos).toIterable
    }).flatten
  }

  def getSynsets(pos: POS): Iterable[Synset] = {
    dictionary.getSynsetIterator(pos).toIterable
  }

  def getFirstSynset(pos: POS, w: String): Synset = {
    val indexWord = dictionary.lookupIndexWord(pos, w)
    if (indexWord != null) indexWord.getSenses.head else null
  }

  def getDirectHypernyms(synset: Synset): Set[Synset] = {
    val list = PointerUtils.getDirectHypernyms(synset)
    list.map( p => p.getSynset ).toSet
  }

  def isHyponymOf(hypo: Synset, hyper: Synset): Boolean = {
    val tree = PointerUtils.getHypernymTree(hypo)
    // NOTE: findFirst does not work correctly!!!! only goes down first branch of tree....
    //val found = tree.findFirst(hyper)
    //found != null
    val found = tree.findAll(hyper)
    found.nonEmpty
  }

  def isHypernymOf(hyper: Synset, hypo: Synset): Boolean = {
    val tree = PointerUtils.getHypernymTree(hypo)
    // NOTE: findFirst does not work correctly!!!! only goes down first branch of tree....
    //val found = tree.findFirst(hyper)
    //found != null
    val found = tree.findAll(hyper)
    found.nonEmpty
  }

  def isHyponymOf(w: IndexWord, hyper: Synset): Boolean = {
    w.getSenses.exists( s => isHyponymOf(s, hyper) )
  }

  def isHypernymOf(w: IndexWord, hypo: Synset): Boolean = {
    w.getSenses.exists( s => isHypernymOf(s, hypo) )
  }

  def isHyponymOf(pos: POS, text: String, hyper: Synset): Boolean = {
    val w = lookupPhraseExact(pos, text)
    if (w != null) isHyponymOf(w, hyper)
    else false
  }

  def isHypernymOf(pos: POS, text: String, hypo: Synset): Boolean = {
    val w = lookupPhraseExact(pos, text)
    if (w != null) isHypernymOf(w, hypo)
    else false
  }

  def isInstance(s: Synset): Boolean = {
    val ps = s.getPointers
    val notInstance = ps.forall( p => !p.getType.equals(PointerType.INSTANCE_HYPERNYM) )
    !notInstance
  }

  def getAllHyperymSynsets(synset: Synset): Set[Synset] = {
    val tree = PointerUtils.getHypernymTree(synset)
    getSynsets(tree.getRootNode)
  }

  def getAllHypernyms(pos: POS, word: String): Set[String] = {
    var words = Set[String]()
    val indexWord = dictionary.getIndexWord(pos, word)
    if (indexWord != null) {
      val tree = PointerUtils.getHypernymTree(indexWord.getSenses.get(0))
      words = getWords(tree.getRootNode)
    }
    words
  }

  def getAllHypernymsWithDistances(pos: POS, word: String): Map[String,Double] = {
    var words = Map[String,Double]()
    val indexWord = dictionary.getIndexWord(pos, word)
    if (indexWord != null) {
      val tree = PointerUtils.getHypernymTree(indexWord.getSenses.get(0))
      words = getWordsWithDistances(tree.getRootNode)
    }
    words
  }

  def getAllHypernymsWithDistancesSorted(pos: POS, word: String): Seq[(String,Double)] = {
    var words = Seq[(String,Double)]()
    val indexWord = dictionary.getIndexWord(pos, word)
    if (indexWord != null) {
      val tree = PointerUtils.getHypernymTree(indexWord.getSenses.get(0))
      words = getWordsWithDistancesSorted(tree.getRootNode)
    }
    words
  }

  def getAllHyponymSynsets(synset: Synset): Set[Synset] = {
    val tree = PointerUtils.getHyponymTree(synset)
    getSynsets(tree.getRootNode)
  }

  def getAllHyponyms(pos: POS, word: String): Set[String] = {
    var words = Set[String]()
    val indexWord = dictionary.getIndexWord(pos, word)
    if (indexWord != null) {
      val tree = PointerUtils.getHyponymTree(indexWord.getSenses.get(0))
      words = getWords(tree.getRootNode)
    }
    words
  }

  def getAllSimilar(pos: POS, word: String): Set[String] = {
    var words = Set[String]()
    val indexWord = dictionary.getIndexWord(pos, word)
    if (indexWord != null) {
      val tree = PointerUtils.getSynonymTree(indexWord.getSenses.get(0), 1)
      words = getWords(tree.getRootNode)
    }
    words
  }

  private def getWordsWithDistances(treeNode: PointerTargetTreeNode): Map[String,Double] = {
    import collection.mutable
    val res = new mutable.HashMap[String,Double]()

    def addWords(treeNode: PointerTargetTreeNode, dist: Double = 0.0) {
      if (treeNode != null) {
        val words = treeNode.getSynset.getWords.map( w => w.getLemma )
        for (word <- words) {
          if (!res.contains(word)) {
            res.put(word, dist)
          }
        }
        if (treeNode.getChildTreeList != null) {
          for (childNode <- treeNode.getChildTreeList)
            addWords(childNode, dist + 1)
        }
      }
    }

    addWords(treeNode, 1.0)
    res.toMap
  }

  private def getWordsWithDistancesSorted(treeNode: PointerTargetTreeNode): Seq[(String,Double)] = {
    val res = getWordsWithDistances(treeNode)
    res.toSeq.sortBy( x => (x._2, x._1) )
  }

  private def getWords(treeNode: PointerTargetTreeNode): Set[String] = {
    import collection.mutable
    val res = mutable.LinkedHashSet[String]()

    def addWords(treeNode: PointerTargetTreeNode) {
      if (treeNode != null) {
        res ++= treeNode.getSynset.getWords.map( w => w.getLemma )
        if (treeNode.getChildTreeList != null) {
          for (childNode <- treeNode.getChildTreeList)
            addWords(childNode)
        }
      }
    }

    addWords(treeNode)
    res.toSet
  }

  private def getSynsets(treeNode: PointerTargetTreeNode): Set[Synset] = {
    import collection.mutable
    val res = mutable.LinkedHashSet[Synset]()

    def addSynsets(treeNode: PointerTargetTreeNode) {
      if (treeNode != null) {
        res += treeNode.getSynset
        if (treeNode.getChildTreeList != null) {
          for (childNode <- treeNode.getChildTreeList)
            addSynsets(childNode)
        }
      }
    }

    addSynsets(treeNode)
    res.toSet
  }

  def getSynset(synsetId: String): Synset = getSynset(synsetId, dictionary)

  //    private void demonstrateMorphologicalAnalysis(String phrase) throws JWNLException {
  //        // "running-away" is kind of a hard case because it involves
  //        // two words that are joined by a hyphen, and one of the words
  //        // is not stemmed. So we have to both remove the hyphen and stem
  //        // "running" before we get to an entry that is in WordNet
  //        System.out.println("Base form for \"" + phrase + "\": " +
  //                dictionary.lookupIndexWord(POS.VERB, phrase));
  //    }
  //
  //    private void demonstrateListOperation(IndexWord word) throws JWNLException {
  //        // Get all of the hypernyms (parents) of the first sense of <var>word</var>
  //        PointerTargetNodeList hypernyms = PointerUtils.getDirectHypernyms(word.getSenses().get(0));
  //        System.out.println("Direct hypernyms of \"" + word.getLemma() + "\":");
  //        hypernyms.print();
  //    }
  //
  //    private void demonstrateTreeOperation(IndexWord word) throws JWNLException {
  //        // Get all the hyponyms (children) of the first sense of <var>word</var>
  //        PointerTargetTree hyponyms = PointerUtils.getHyponymTree(word.getSenses().get(0));
  //        System.out.println("Hyponyms of \"" + word.getLemma() + "\":");
  //        hyponyms.print();
  //    }
  //
  //    private void demonstrateAsymmetricRelationshipOperation(IndexWord start, IndexWord end) throws JWNLException, CloneNotSupportedException {
  //        // Try to find a relationship between the first sense of <var>start</var> and the first sense of <var>end</var>
  //        RelationshipList list = RelationshipFinder.findRelationships(start.getSenses().get(0), end.getSenses().get(0), PointerType.HYPERNYM);
  //        System.out.println("Hypernym relationship between \"" + start.getLemma() + "\" and \"" + end.getLemma() + "\":");
  //        for (Object aList : list) {
  //            ((Relationship) aList).getNodeList().print();
  //        }
  //        System.out.println("Common Parent Index: " + ((AsymmetricRelationship) list.get(0)).getCommonParentIndex());
  //        System.out.println("Depth: " + list.get(0).getDepth());
  //    }
  //
  //    private void demonstrateSymmetricRelationshipOperation(IndexWord start, IndexWord end) throws JWNLException, CloneNotSupportedException {
  //        // find all synonyms that <var>start</var> and <var>end</var> have in common
  //        RelationshipList list = RelationshipFinder.findRelationships(start.getSenses().get(0), end.getSenses().get(0), PointerType.SIMILAR_TO);
  //        System.out.println("Synonym relationship between \"" + start.getLemma() + "\" and \"" + end.getLemma() + "\":");
  //        for (Object aList : list) {
  //            ((Relationship) aList).getNodeList().print();
  //        }
  //        System.out.println("Depth: " + list.get(0).getDepth());
  //    }

}

object WordNet30 extends WordNetWrapper(WordNetConstants.WORDNET_CONF_FILE, WordNetConstants.WORDNET_DICT30_FILE)
object WordNet31 extends WordNetWrapper(WordNetConstants.WORDNET_CONF_FILE, WordNetConstants.WORDNET_DICT31_FILE)

object WordNetWrapper extends WordNetWrapper(WordNetConstants.WORDNET_CONF_FILE, WordNetConstants.WORDNET_DICT_FILE)

// Mapping from WordNet30 to WordNet31 senses
object WordNet3xSenseMapping {
  lazy val synsets = readMapFile(WordNetConstants.WORDNET_DICT30TO31MAP_FILE)
  lazy val wordnet30To31 = synsets.map( x => x._1 -> x._2 ).toMap
  lazy val wordnet31To30 = synsets.map( x => x._2 -> x._1 ).toMap

  val wordNetSynsetIdRegex = new Regex("([a-z])(\\d+)")
  val uwnWordNetSynsetIdRegex = new Regex("s/([a-z])(\\d+)")
  def formatSynsetId(id: String) = id match {
    case uwnWordNetSynsetIdRegex(pos,offset) => pos + "%d".format(offset.toInt)
    case wordNetSynsetIdRegex(pos,offset) => pos + "%d".format(offset.toInt)
  }
  def wn30To31(id: String) = wordnet30To31.getOrElse(formatSynsetId(id), "")
  def wn31To30(id: String) = wordnet31To30.getOrElse(formatSynsetId(id), "")

  def readMapFile(filename: String): Seq[(String,String)] = {
    val lines = IOUtils.getLines(filename)
    val synsets = lines.map( x => {
      val fields = x.split("\t")
      (fields(0), fields(1))
    }).filter( x => x._1 != "???" && x._2 != "???")
    synsets.toSeq
  }
}

object WordNet3xSenseMappingCreator extends App {

  createMapFile(WordNet30,WordNet31,WordNetConstants.WORDNET_DICT30TO31MAP_FILE )

  def createMapFile(w1: WordNetWrapper, w2: WordNetWrapper, filename: String = null) {
    val output = if (filename != null) IOUtils.filePrintWriter(filename) else new PrintWriter(System.out)
    def toWordsStr(s: Synset) = {
      if (s != null) s.getWords.map( w => w.getLemma).mkString(",") else ""
    }

    def printMatch(s1: Synset, s2: Synset) {
      val gloss = if (s2 != null) s2.getGloss else s1.getGloss
      val id1 = if (s1 != null) w1.getSynsetId(s1) else "???"
      val id2 = if (s2 != null) w2.getSynsetId(s2) else "???"
      val wd1 = toWordsStr(s1)
      val wd2 = toWordsStr(s2)
      output.println(Seq(id1,id2,wd1,wd2,gloss).mkString("\t"))
    }

    def matches(s1: Synset, s2: Synset) = {
      if (s1 == null || s2 == null) false
      else if (s1.getGloss == s2.getGloss || toWordsStr(s1) == toWordsStr(s2)) true
      else {
        // Just check the first part of the gloss
        val g1 = s1.getGloss.split(";").head
        val g2 = s2.getGloss.split(";").head
        g1 == g2
      }
    }

    for (pos <- POS.getAllPOS) {
      val unmatchedSynsets1 = new mutable.ListBuffer[Synset]()
      val unmatchedSynsets2 = new mutable.ListBuffer[Synset]()

      // Assume the synsets are essentially in order
      val ss1 = w1.getSynsets(pos).iterator
      val ss2 = w2.getSynsets(pos).iterator
      while (ss1.nonEmpty || ss2.nonEmpty) {
        val s1 = if (unmatchedSynsets1.nonEmpty && unmatchedSynsets2.isEmpty && ss2.nonEmpty) unmatchedSynsets1.remove(0)
                  else if (ss1.nonEmpty) ss1.next() else null
        val s2 = if (unmatchedSynsets2.nonEmpty && unmatchedSynsets1.isEmpty && ss1.nonEmpty) unmatchedSynsets2.remove(0)
                  else if (ss2.nonEmpty) ss2.next() else null

        if (matches(s1,s2)) {
          printMatch(s1,s2)
        } else {
          // Try to match any unmatched synsets
          if (s2 != null) {
            val mu1  = unmatchedSynsets1.zipWithIndex.find( s => matches(s._1,s2) )
            if (mu1.nonEmpty) {
              // Matched s2 to mu1
              printMatch(mu1.get._1,s2)
              unmatchedSynsets1.remove(mu1.get._2)
            } else {
              unmatchedSynsets2.append(s2)
            }
          }
          if (s1 != null) {
            val mu2  = unmatchedSynsets2.zipWithIndex.find( s => matches(s1,s._1) )
            if (mu2.nonEmpty) {
              // Matched s1 to mu2
              printMatch(s1,mu2.get._1)
              unmatchedSynsets2.remove(mu2.get._2)
            } else {
              unmatchedSynsets1.append(s1)
            }
          }
        }
      }
      for (s <- unmatchedSynsets1) {
        printMatch(s,null)
      }
      for (s <- unmatchedSynsets2) {
        printMatch(null,s)
      }
      println("Unmatched " + pos + ": " + unmatchedSynsets1.size + ", " + unmatchedSynsets2.size)
    }

    output.flush()
    if (filename != null) output.close()
  }
}

