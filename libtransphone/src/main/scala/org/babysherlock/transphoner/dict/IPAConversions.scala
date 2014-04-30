package org.babysherlock.transphoner.dict

import org.babysherlock.transphoner.phonetics.IPA
import org.babysherlock.transphoner.Constants
import org.babysherlock.util.{IOUtils, CSVFile}
import com.ibm.icu.text.Transliterator

/**
 * @author Manolis Savva
 */
object IPAConversions extends App {
  val trans = Transliterator.getInstance("Hiragana-Katakana")
  val kata2ipa = IOUtils.loadMap(Constants.KATAKANA_IPA_CSV,',')
  val nonSyllableTargets = Set("Q", "N", "ː", "")

  //println(availableTransliteratorIDs())
  //japaneseTsvToRomaji(Constants.DATA_DIR + "ja-edict-full.tsv.bz2", Constants.DATA_DIR + "ja-edict-latin.tsv.bz2")

  def availableTransliteratorIDs(): List[String] = {
    val en = Transliterator.getAvailableIDs
    var arr = List[String]()
    while (en.hasMoreElements) arr = arr :+ en.nextElement()
    arr
  }

  def katakana2ipa(s: String): String = {
    var temp: String = s
    val digraphs = kata2ipa.filterKeys(_.size > 1)
    val unigraphs = kata2ipa.filterKeys(!digraphs.keySet.contains(_))
    digraphs.foreach(k => temp = temp.replace(k._1, k._2 + IPA.syllableSep))
    unigraphs.foreach(k => {
      val tgt = if (nonSyllableTargets.contains(k._2)) k._2 else k._2 + IPA.syllableSep
      temp = temp.replace(k._1, tgt)
    })
    temp = correctLongMark(temp)
    temp = moraicNasalToIPA(temp)
    temp = sokuonToIPA(temp)
    if (temp.last == '.') temp = temp.dropRight(1)
    temp = finalSokuonToStop(temp)
    println(s + "->" + temp)
    temp
  }

  // Handle moraic nasal conversion to IPA
  private def moraicNasalToIPA(s: String): String = {
    s.replaceAll("\\.N([pbm])","m.$1")
     .replaceAll("\\.N([dtn])","n.$1")
     .replaceAll("\\.N([kg])","ŋ.$1")
     .replaceAll("\\.N","ɴ.")
     .replaceAll("N","ɴ.")
  }

  // Handle germination due to sokuon
  private def sokuonToIPA(s: String): String = s.replaceAll("\\.Q([kɡtdpbszʃmnɕɸhɾ])","$1.$1")

  // Make sure length mark is not separated from previous syllable by break
  private def correctLongMark(s: String): String = s.replaceAll("\\.ː","ː.")

  private def finalSokuonToStop(s: String): String = {
    if (s.endsWith(".Q")) s.replaceAll("\\.Q","ʔ") else s
  }

  def japaneseTsvToIPA(file: String, out: String) {
    val pw = IOUtils.filePrintWriter(out)
    val tsvFile = new CSVFile(file,separator='\t',quote='\0')
    tsvFile.foreach(row => {
      val word = row(0)
      val kana = trans.transliterate(row(1))
      val gloss = row(2)
      val ipa = katakana2ipa(kana)
      pw.println(word + "\t" + ipa + "\t" + gloss)
    })
    pw.close()
  }

  def edictToRomaji(file: String, out: String) {
    val pw = IOUtils.filePrintWriter(out)
    val tsvFile = new CSVFile(file,separator='\t',quote='\0')
    val trans = Transliterator.getInstance("Any-Latin")
    tsvFile.foreach(row => {
      val word = row(0)
      val romaji = trans.transliterate(row(1))
      pw.println(word + "\t" + romaji)
    })
    pw.close()
  }

  def cedictToNumericPinyin(file: String, out: String) {
    val pw = IOUtils.filePrintWriter(out)
    val tsvFile = new CSVFile(file,separator=' ',quote='\0')
    val trans = Transliterator.getInstance("Latin-NumericPinyin")
    tsvFile.foreach(row => {
      pw.println(row.head + "\t" + trans.transliterate(row.tail.mkString(" ")))
      println(row.head)
      println(row.tail.mkString(" "))
    })
    pw.close()
  }
}
