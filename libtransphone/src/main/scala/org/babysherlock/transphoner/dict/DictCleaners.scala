package org.babysherlock.transphoner.dict

import org.babysherlock.transphoner.{Constants, WeightedLevenshteinSimilarity}
import org.babysherlock.transphoner.phonetics.{IPA, PhoneSimilarity}
import org.babysherlock.util.{IOUtils, CSVFile}
import java.util.regex.Pattern

/**
 * Utilities for cleaning our dictionaries
 * @author Angel Chang
 */
object DictCleaners extends App {
  val englishCleaner = new EnglishDictCleaner()
  //clean(Constants.TRANSPHONER_DATA_DIR + "twl.all.ipa.tsv.bz2", Constants.TRANSPHONER_DATA_DIR + "twl.ipa.clean.tsv", englishCleaner)

  val chineseCleaner = new ChineseDictCleaner()
  //clean(Constants.TRANSPHONER_DATA_DIR + "zh-cedict-full.tsv.bz2", Constants.TRANSPHONER_DATA_DIR + "zh-cedict-full-clean.tsv.bz2", chineseCleaner)

  val frenchCleaner = new FrenchDictCleaner()
  //clean(Constants.TRANSPHONER_DATA_DIR + "fr-combined-ipa.tsv.bz2", Constants.TRANSPHONER_DATA_DIR + "fr-combined-ipa-clean.tsv", frenchCleaner)

  def clean(input: String, output: String, cleaner: DictCleaner) {
    val outputWriter = IOUtils.filePrintWriter(output)
    val tsvFile = new CSVFile(input,separator='\t',quote='\0')
    var lastWord: String = null
    var lastFullPron: String = null
    // Assumes file is sorted by words
    tsvFile.foreach(row => {
      if (row.size > 1) {
        val word = cleaner.cleanWord(row(0))
        var ipas = cleaner.getIPAs(word, row(1))
        ipas = ipas.map( ipa => cleaner.cleanIPA(ipa.trim()) ).filter(ipa => ipa.nonEmpty)

        val cleaned = {
          for (ipa <- ipas) yield {
            var res = ipa
            if (word == lastWord && lastFullPron != null) {
              res = cleaner.cleanPartial(lastFullPron, ipa)
            }
            if (res == ipa) {
              lastFullPron = ipa
            }
            lastWord = word
            res
          }
        }.filter( ipa => !cleaner.isPartial(ipa))

        val syllabified = cleaned.map( s => cleaner.syllabify(s) )

        val rest = if (row.length > 2) "\t" + row.drop(2).mkString("\t") else ""
        for (ipa <- syllabified) {
          outputWriter.println( word + "\t" + ipa + rest)
        }
      }
    })
    outputWriter.close()
  }

}

class DictCleaner(val syllabifier: Syllabifier = null) {
  // Cleans IPA pronunciations
  def cleanWord(word: String) = word
  def cleanIPA(ipa: String) = ipa
  def cleanPartial(main: String, partial: String) = partial
  def isPartial(s: String) = false
  def syllabify(s: String) = if (syllabifier != null) syllabifier.syllabify(s) else s
  def getIPAs(word: String, s: String): Seq[String] = Seq(s)
}

class IPARegexCleaner(val patterns: Seq[(Pattern, String)],
                      syllabifier: Syllabifier) extends DictCleaner(syllabifier) {
  val phoneSim = PhoneSimilarity.AlineSimilarity
  val phoneSeqSim = WeightedLevenshteinSimilarity(phoneSim)

  def this(regexes: Map[String,String], syllabifier: Syllabifier = null) =
    this(regexes.toSeq.map( p => ( Pattern.compile(p._1), p._2) ), syllabifier)

  override def cleanIPA(ipa: String) = {
    var res = ipa
    for ( (pattern, replacement) <- patterns) {
      res = pattern.matcher(res).replaceAll(replacement)
    }
    res
  }

  override def cleanPartial(main: String, partial: String) = completeIPA(main, partial)

  def ipaDistance(ipa1: String, ipa2: String): Double = {
    val phones1 = IPA.strToPhoneSeq(ipa1)
    val phones2 = IPA.strToPhoneSeq(ipa2)
    if (phones1.isEmpty || phones2.isEmpty) Double.MaxValue
    else {
      phoneSeqSim.distance(phones1, phones2)
      // Weight first/last more
      //+ 5*phoneSim.distance(phones1.head, phones2.head)
      //+ 5*phoneSim.distance(phones1.last, phones2.last)
    }
  }

  override def isPartial(pr: String) = _incompleteMarkers.contains(pr.head) || _incompleteMarkers.contains(pr.last)

  private val _incompleteMarkers = Set('‐','-')
  // Adapted and simplified from fuzzybox DictionaryComScraper
  private def completeIPA(main: String, pr: String): String = {
    var pronunciation: String = pr
    val incompleteHead = _incompleteMarkers.contains(pr.head)
    val incompleteLast = _incompleteMarkers.contains(pr.last)
    if (incompleteHead && incompleteLast) {
      val part: String = pr.substring(1, pr.length - 1)
      var startIndex: Int = -1
      var endIndex: Int = -1
      var selectedDist: Double = Double.MaxValue
      for (s <- 1 until main.length-1) {
        for (e <- s+1 until main.length-1) {
          val substring: String = main.substring(s, e + 1)
          val dist: Double = ipaDistance(substring, part)
          if (dist < selectedDist) {
            selectedDist = dist
            startIndex = s
            endIndex = e
          }
        }
      }
      if (startIndex >= 0 && endIndex >= 0) {
        pronunciation = main.substring(0, startIndex) + part + main.substring(endIndex + 1)
      }
    }
    else if (incompleteHead) {
      val part: String = pr.substring(1)
      //val startChar: Char = part(0)
      var selectedDist: Double = Double.MaxValue
      var selected: String = ""
      var selectedIndex: Int = -1
      for (i <- main.length-1 to 1 by -1) {
        val temp: String = main(i) + selected
        val dist: Double = ipaDistance(temp, part)
        if (dist <= selectedDist) {
          selected = temp
          selectedDist = dist
          selectedIndex = i
        }
      }
      if (selectedIndex >= 0) {
        pronunciation = main.substring(0, selectedIndex) + part
      }
    }
    else if (incompleteLast) {
      val part: String = pr.substring(0, pr.length - 1)
      //val endChar: Char = part(part.length - 1)
      var selectedDist: Double = Double.MaxValue
      var selected: String = ""
      var selectedIndex: Int = -1
      for (i <- 0 until main.length-1) {
        val temp: String = selected + main(i)
        val dist: Double = ipaDistance(temp, part)
        if (dist <= selectedDist) {
          selected = temp
          selectedDist = dist
          selectedIndex = i
        }
      }
      if (selectedIndex >= 0) {
        pronunciation = part + main.substring(selectedIndex + 1)
      }
    }
    pronunciation
  }
}

// Some English dictionaries (like dictionary.com)
//  uses ʰw with should be mapped to ʍ or hw (see http://en.wikipedia.org/wiki/English_phonology)
// For simplicity, we can also map those to w
// Maybe they use the phonemic alphabet
//        http://englishwithdouglas.com/phonemic_alphabet.htm
//        http://englishwithdouglas.com/miscellaneous/phonemic_chart.pdf
// We also map r (trill) to ɹ (flap) since that's more common
class EnglishDictCleaner extends IPARegexCleaner(Map("ʰw" -> "ʍ","r" -> "ɹ"), new PhonotacticSyllabifier(Constants.LANGS("EN")("syllabify"))) {
  override def cleanWord(word: String): String = {
    var res = word
    if (word.contains(',')) {
      val parts = word.split("\\s*,\\s*", 2)
      val w = parts(0).toLowerCase
      if (w.equalsIgnoreCase(parts(1))) {
        res = w
      } else {
        println("WARNING: Word has comma: " + word)
      }
    }
    res
  }

  override def getIPAs(word: String, s: String): Seq[String] = {
    // Special dictionary.com list cleaning (probably not appropriate for other lists)
    if (word == "baccarat") {
      Seq(s.replaceAll("\\s+", ""))
    } else {
      s.split("\\s+").toSeq
    }
  }
}

// Map spaces to syllable separators since CEDICT output uses spacing between characters
class ChineseDictCleaner extends IPARegexCleaner(Map(" " -> IPA.syllableSep.ipa))

// Map r to ʁ (strictly speaking French has only uvular fricative)
class FrenchDictCleaner extends IPARegexCleaner(Map(" " -> IPA.syllableSep.ipa, "r" -> "ʁ"), new PhonotacticSyllabifier(Constants.LANGS("FR")("syllabify"))) {
  override def syllabify(s: String): String = {
    val i = s.lastIndexOf(".")
    if (i > 0 && !s.contains("ˈ")) s.take(i+1) ++ "ˈ" + s.substring(i+1) else s
  }
}