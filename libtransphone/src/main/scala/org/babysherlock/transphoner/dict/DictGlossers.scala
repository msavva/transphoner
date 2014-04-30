package org.babysherlock.transphoner.dict

import org.babysherlock.transphoner.Constants
import org.babysherlock.util.{CSVRow, CSVFile, IOUtils}
import java.io.IOException
import scala.collection.{mutable, GenMap}
import scala.util.matching.Regex

/**
 * Add glosses and word class information to our dictionaries
 * @author Angel Chang
 */
object DictGlosser extends App {
  writeGlosses(Constants.TRANSPHONER_DATA_DIR + "de-freedict-ipa.tsv.bz2",
    Constants.TRANSPHONER_DATA_DIR + "de-freedict-ipa2.tsv.bz2",
    Constants.TRANSPHONER_DATA_DIR + "de-freedict-glosses.tsv.bz2",
    GermanFreeDictGlossLookup)
  writeGlosses(Constants.TRANSPHONER_DATA_DIR + "fr-freedict-ipa.tsv.bz2",
    Constants.TRANSPHONER_DATA_DIR + "fr-freedict-ipa2.tsv.bz2",
    Constants.TRANSPHONER_DATA_DIR + "fr-freedict-glosses.tsv.bz2",
    FrenchFreeDictGlossLookup)


//  addGlosses(Constants.TRANSPHONER_DATA_DIR + "de-freedict-ipa.tsv.bz2", Constants.TRANSPHONER_DATA_DIR + "de-freedict-ipa-glosses.tsv.bz2", GermanFreeDictGlossLookup)
//  addGlosses(Constants.TRANSPHONER_DATA_DIR + "fr-freedict-ipa.tsv.bz2", Constants.TRANSPHONER_DATA_DIR + "fr-freedict-ipa-glosses.tsv.bz2",  FrenchFreeDictGlossLookup)
//  addGlosses(Constants.TRANSPHONER_DATA_DIR + "fr-combined-ipa.tsv.bz2", Constants.TRANSPHONER_DATA_DIR + "fr-combined-ipa-glosses.tsv.bz2",  FrenchFreeDictGlossLookup)

  // Create single file with word, IPA, and glosses
  def addGlosses(input: String, output: String, glosser: DictGlossLookup) {
    val outputWriter = IOUtils.filePrintWriter(output)
    val tsvFile = new CSVFile(input,separator='\t',quote='\0')
    tsvFile.foreach(row => {
      if (row.size > 1) {
        val word = row(0)
        val ipas = row(1)
        val lookup = glosser.getWordIpaGloss(word, ipas)
        if (lookup.gloss != null) {
          outputWriter.println(lookup.word + "\t" + lookup.ipa + "\t" + lookup.gloss)
        } else {
          println("No gloss for " + word)
          outputWriter.println(word + "\t" + ipas)
        }
      }
    })
    outputWriter.close()
  }

  // Write two files - one with pronunciation and one with glosses
  def writeGlosses(input: String, ipaFile: String, glossesFile: String, glosser: DictGlossLookup) {
    val ipaWriter = IOUtils.filePrintWriter(ipaFile)
    val glossesWriter = IOUtils.filePrintWriter(glossesFile)
    glossesWriter.println("word\tgloss")
    val tsvFile = new CSVFile(input,separator='\t',quote='\0')
    tsvFile.foreach(row => {
      if (row.size > 1) {
        val word = row(0)
        val ipas = row(1)
        val lookup = glosser.getWordIpaGloss(word, ipas)
        if (lookup.gloss != null) {
          ipaWriter.println(lookup.word + "\t" + lookup.ipa)
          glossesWriter.println(lookup.word + "\t" + lookup.gloss)
        } else {
          println("No gloss for " + word)
          ipaWriter.println(word + "\t" + ipas)
        }
      }
    })
    glossesWriter.close()
    ipaWriter.close()
  }
}

// Glosses from TSV files
class TsvDictGlosses(val dictfile: String) extends DictGlossLookup {
  lazy val entries = initEntries

  def initEntries: GenMap[String, Seq[WordSense]] = glossesFromTsvAsMap(dictfile)

  override def getSenses(word: String): Seq[WordSense] = {
    val w = word.replaceAll("\\s+", " ")
    var entry = entries.get(w)
    if (entry.isEmpty) {
      // Try lowercasing
      entry = entries.get(w.toLowerCase)
    }
    entry.getOrElse(Seq())
  }

  override def getWordGloss(word: String): WordGloss = {
    val senses = getSenses(word)
    if (senses.nonEmpty) {
      val h = senses.head
      WordGloss(h.word, h.gloss)
    } else {
      WordGloss(word,null)
    }
  }

  private def glossesFromTsvAsMap(file: String): GenMap[String, Seq[WordSense]] = {
    glossesFromTsv(file).groupBy( g => g.word )
  }

  private def glossesFromTsv(file: String): Seq[WordSense] = {
    val tsvFile = new CSVFile(file,separator='\t',quote='\0',includesHeader=true)
    //println("read " + tsvFile.size)
    tsvFile.toSeq.map(row => {
      val r = new CSVRow(row, tsvFile)
      WordSense(r("word"),
        r("gloss"),
        r.getOrElse("pos", null),
        r.getOrElse("source",null),
        r.get("senseIndex").map( x => x.toInt ).getOrElse(-1))
    })
  }
}

// Glosses from FreeDict

// Map r to ʁ (strictly speaking French has only uvular fricative)
object GermanFreeDictGlossLookup extends FreeDictGlossLookup("deu", Constants.TRANSPHONER_DATA_DIR + "freedict-deu-eng.dict", true) {
  val mathAndSuchRegex = new Regex("(.+?)\\s+(?:(?:f|m|n|pl)\\s+)?(math|biol|bot|econ|elec|med|zoo|arch|chem|phys|tech|gastr|umg)")
  val genderRegex = new Regex("(.+?)\\s+(?:f|m|n|pl)")

  override def getWordIpaGloss(word: String, ipa: String): WordIpaGloss = {
    val g = getWordGloss(word)
    if (g.gloss != null) {
      val ws = word.split("\\s+")
      val ipas = ipa.split("\\s+")
      val newws = g.word.split("\\s+")
      val newipa = if (ws.size == ipas.size && newws.size < ws.size) {
        ipas.take(newws.size).mkString(" ")
      } else ipa
      WordIpaGloss(g.word, newipa, g.gloss)
    } else {
      WordIpaGloss(g.word, ipa, g.gloss)
    }
  }

  override def getWordGloss(word: String): WordGloss = {
    val w = word match {
      case mathAndSuchRegex(x,d) => {
//        println("Convert " + word + " to " + x + ", ipa is " + ipas)
        x.trim()
      }
      case genderRegex(x) => {
//        println("Convert " + word + " to " + x + ", ipa is " + ipas)
        x.trim()
      }
      case _ => word
    }
    super.getWordGloss(w)
  }
}

object FrenchFreeDictGlossLookup extends FreeDictGlossLookup("fra", Constants.TRANSPHONER_DATA_DIR + "freedict-fra-eng.dict") {
}

trait DictGlossLookup {
  case class WordGloss(word: String, gloss: String)
  case class WordIpaGloss(word: String, ipa: String, gloss: String)
  case class WordSense(word: String, gloss: String, pos: String = null, source: String = null, senseIndex: Int = -1)

  def getSenses(word: String) = {
    val g = getWordGloss(word)
    if (g.gloss != null)
      Seq(WordSense(g.word, g.gloss))
    else Seq()
  }

  def getWordGloss(word: String) = WordGloss(word,null)

  def getGloss(word: String) = getWordGloss(word).gloss
  def getWordIpaGloss(word: String, ipa: String): WordIpaGloss = {
    val g = getWordGloss(word)
    WordIpaGloss(g.word, ipa, g.gloss)
  }
}

class FreeDictGlossLookup(val lang: String, val dictfile: String, val removePunct: Boolean = false) extends DictGlossLookup {
  lazy val entries = initEntries

  def initEntries: GenMap[String, FreeDictReader.Entry] = FreeDictReader.entries(lang, dictfile, removePunct)

  override def getWordGloss(word: String): WordGloss = {
    val w = word.replaceAll("\\s+", " ")
    var entry = entries.get(w)
    if (entry.isEmpty) {
        // Try lowercasing
        entry = entries.get(w.toLowerCase)
    }
    if (entry.isDefined) {
      WordGloss(entry.get.word,entry.get.gloss)
    } else {
      WordGloss(word,null)
    }
  }
}

object FreeDictReader {

  case class Entry(word: String,
                   pronunciation: String,
                   pos: Pos.Value,
                   number: Number.Value,
                   gender: Gender.Value,
                   gloss: String)

  val dictEntryFirstLineRegex1 = new Regex("(.+)\\[(.*)\\]\\s*\\[\\](.+)?")
  val dictEntryFirstLineRegex2 = new Regex("(.+)\\[(.*)\\](.+)?")
  val dictEntryFirstLineRegex3 = new Regex("(.+)(\\(n\\).*)")
  def toEntry(lang: String, firstLine: String, otherLines: String, removePunct: Boolean): Entry = {
    firstLine match {
      case dictEntryFirstLineRegex1(word, discipline, posgender) => {
        toEntry(lang, word.trim(), null, posgender, otherLines, removePunct)
      }
      case dictEntryFirstLineRegex2(word, pron, posgender) => {
        toEntry(lang, word.trim(), pron, posgender, otherLines, removePunct)
      }
      case dictEntryFirstLineRegex3(word, posgender) => {
        toEntry(lang, word.trim(), null, posgender, otherLines, removePunct)
      }
      case x if x.startsWith("00") => {
        println("Skipping entry: " + firstLine)
        null
      }
      case _ => {
        toEntry(lang, firstLine, null, null, otherLines, removePunct)
      }
    }
  }

  val wordRegex = new Regex("(.+)\\s+\\{([a-z]+)\\W*")
  def toEntry(lang: String, word: String, pron: String, posgender: String, gloss: String, removePunct: Boolean): Entry = {
    val (cleanedWord1,genderinfo1) = word match {
      case wordRegex(w, g) => (w, g)
      case _ => (word, null)
    }
    val cleanedWord =
      if (removePunct) cleanedWord1.replaceAll("\\p{Punct}","").replaceAll("\\s+"," ")
      else cleanedWord1.replaceAll("\\s+"," ")
    val posgenderfields = if (posgender != null && posgender.nonEmpty) posgender.split("\\s*,\\s*") else null
    val (posinfo,number,gender) =
      if (posgenderfields != null) {
        posgenderfields(1) match {
          case "s.(f )" => ("(n)", Number.SINGLE, Gender.FEM)
          case "s.(m )" => ("(n)", Number.SINGLE, Gender.MASC)
          case "s.(n )" => ("(n)", Number.SINGLE,Gender.NEUT)
          case "pl." => ("(n)", Number.PLURAL, null)
          case _ => (posgenderfields(0),null,null)
        }
      }
      else genderinfo1 match {
        case "f" => ("(n)", Number.SINGLE, Gender.FEM)
        case "m" => ("(n)", Number.SINGLE, Gender.MASC)
        case "n" => ("(n)", Number.SINGLE, Gender.NEUT)
        case "pl" => ("(n)", Number.PLURAL, null)
        case _ => (null,null,null)
      }
    val pos =
      if (posinfo == "(n)") Pos.Noun
      else if (posinfo == "(v)") Pos.Verb
      else if (gloss != null && gloss.startsWith("to ")) Pos.Verb
      else if (lang == "deu" && word.head.isUpper) Pos.Noun
      else null
    new Entry(cleanedWord, pron, pos, number, gender, gloss)
  }

  def entries(lang: String, filename: String, removePunct: Boolean = false): GenMap[String, Entry] = {
    var lineNo = 0
    var entryFirstLine: String = null
    var entryOtherLines: String = null
    val myMap = new mutable.LinkedHashMap[String, Entry ]


    // helper function for addition a new entry
    def addEntry() {
      if (entryFirstLine != null) {
        val entry = toEntry(lang, entryFirstLine, entryOtherLines, removePunct)
        if (entry != null) {
          myMap.put(entry.word, entry)
          if (!myMap.contains(entry.word.toLowerCase))
            myMap.put(entry.word.toLowerCase, entry)
        }
      }
    }

    for (line <- IOUtils.getLines(filename)) {
      lineNo = lineNo+1
      val trimmed = line.trim
      if (trimmed.nonEmpty) {
        if (entryFirstLine == null) {
          entryFirstLine = trimmed
          // Check if this line starts with a space or not
          if (line.head == ' ') {
            // hmm, this is unexpected
            throw new IOException("Unexpected line (" + filename + ":" + lineNo + "): " + line)
          }
        } else {
          entryOtherLines = if (entryOtherLines != null) entryOtherLines + " " + trimmed else trimmed
        }
      } else {
        // Empty line - going on to next entry
        addEntry()
        entryFirstLine = null
        entryOtherLines = null
      }
    }
    addEntry()
    myMap
  }

}

object FreeDictReaderTest extends App {
  //  val dict = Constants.TRANSPHONER_DATA_DIR + "freedict-deu-eng.dict"
  //  val entries = FreeDictReader.entries("deu",dict)
  //  entries.take(100).foreach( e => println(e) )
  val dict = Constants.TRANSPHONER_DATA_DIR + "freedict-fra-eng.dict"
  val entries = FreeDictReader.entries("fra",dict)
  entries.take(100).foreach( e => println(e) )
}