package org.babysherlock.transphoner

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}
import scala.collection.GenSeq
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.reflect.io.File
import org.babysherlock.transphoner.phonetics._
import org.babysherlock.transphoner.phonetics.Phone
import org.babysherlock.transphoner.dict._
import org.babysherlock.transphoner.phonetics.Phone
import org.babysherlock.transphoner.dict.Word

/**
 * Test cases for Phone class
 * @author Manolis Savva
 */
@RunWith(classOf[JUnitRunner])
class PhoneTest extends FunSuite with BeforeAndAfter {

  lazy val en = Language.EN
  lazy val fr = Language.FR
  lazy val fr2 = Language.FR2
  lazy val zh = Language.ZH
  lazy val ja = Language.JA
  lazy val de = Language.DE

  lazy val enWords = en.words.toList.filter(w => w.phones.size > 0 && w.phones.size < 11).sortBy(-_.phones.size)
  lazy val frWords = fr.words.toList.filter(w => w.phones.size > 0 && w.phones.size < 11).sortBy(-_.phones.size)
  lazy val fr2Words = fr2.words.toList.filter(w => w.phones.size > 0 && w.phones.size < 11).sortBy(-_.phones.size)
  lazy val zhWords = zh.words.toList.filter(w => w.phones.size > 0 && w.phones.size < 11).sortBy(-_.phones.size)
  lazy val jaWords = ja.words.toList.filter(w => w.phones.size > 0 && w.phones.size < 11).sortBy(-_.phones.size)
  lazy val deWords = de.words.toList.filter(w => w.phones.size > 0 && w.phones.size < 11).sortBy(-_.phones.size)

  val hovercraft = Seq(new IPAWord("maɪhəvərkræftsfʊləvilz"))
  val word = Seq(new IPAWord("dɪsˈɡrʌntəɫd"))

  val threads = 6
  val N = 100
  lazy val testWords = frWords.take(N)
  var matches: GenSeq[(Word,(Seq[Word],Double))] = Seq()

  val phoneCatSim = PhoneticApproximatorTestHelper.similarityPhoneticApproximation(PhoneSimilarity.AlineSimilarity, 0, 0)_

  private def transPhone(src: Iterable[Word], tgt: Seq[Word],
                         method: (Word,Seq[Word])=> (Seq[Word],Double)): Iterable[(Word,(Seq[Word],Double))] = {
    var count = 0
    val par = src.par
    par.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(threads))
    val matches = par.map(w => {
      count += 1
      val res = w -> method(w, tgt)
      println(count + ":" + res)
      res
    }).seq.toSeq.sortBy(_._2._2)
    println(matches.mkString("\n"))
    matches
  }

  private def averageMatchDistance(l1: List[Word], l2: List[Word], N: Int): Double = {
    val s = l1.take(N)
    val t = l2.take(N)
    val sim = PhoneticApproximatorTestHelper.similarityPhoneticApproximation()_
    val av = s.par.map(w => {
      val res = w -> sim(w, t.diff(List(w)))
      println(res)
      res
    }).toSeq.map(_._2._2).sum / N.toDouble
    println(av)
    av
  }

  test("word-de") {
    transPhone(word, deWords, phoneCatSim)
  }

  test("print phonetic inventories") {
    val en = DictLanguage.PHONE_INVENTORY("English (American)").toList.sorted
    val fr = DictLanguage.PHONE_INVENTORY("French").toList.sorted
    val ja = DictLanguage.PHONE_INVENTORY("Japanese").toList.sorted
    val zh = DictLanguage.PHONE_INVENTORY("Mandarin").toList.sorted
    println(en.mkString(", "))
    println(fr.mkString(", "))
    println(ja.mkString(", "))
    println(zh.mkString(", "))
    println(en.size)
    println(fr.size)
    println(ja.size)
    println(zh.size)
  }

  test("phonetic inventory overlap") {
    def setOverlap[T](a: Set[T], b: Set[T]) = a.intersect(b).size.toDouble / math.min(a.size, b.size)

    val en = Language.EN.phoneInventory//DictLanguage.PHONE_INVENTORY("English (American)")
    val fr = Language.FR.phoneInventory//PHONE_INVENTORY("French")
    val ja = Language.JA.phoneInventory//PHONE_INVENTORY("Japanese")
    val zh = Language.ZH.phoneInventory//PHONE_INVENTORY("Mandarin")

    println("")
    println(en.mkString(","))
    println(fr.mkString(","))
    println(ja.mkString(","))
    println(zh.mkString(","))

    val en2en = 1 - setOverlap(en, en)
    val en2fr = 1 - setOverlap(en, fr)
    val en2ja = 1 - setOverlap(en, ja)
    val en2zh = 1 - setOverlap(en, zh)

    val fr2en = 1 - setOverlap(fr, en)
    val fr2fr = 1 - setOverlap(fr, fr)
    val fr2ja = 1 - setOverlap(fr, ja)
    val fr2zh = 1 - setOverlap(fr, zh)

    val ja2en = 1 - setOverlap(ja, en)
    val ja2fr = 1 - setOverlap(ja, fr)
    val ja2ja = 1 - setOverlap(ja, ja)
    val ja2zh = 1 - setOverlap(ja, zh)

    val zh2en = 1 - setOverlap(zh, en)
    val zh2fr = 1 - setOverlap(zh, fr)
    val zh2ja = 1 - setOverlap(zh, ja)
    val zh2zh = 1 - setOverlap(zh, zh)

    println(List(en2en, en2fr, en2ja, en2zh).mkString(","))
    println(List(fr2en, fr2fr, fr2ja, fr2zh).mkString(","))
    println(List(ja2en, ja2fr, ja2ja, ja2zh).mkString(","))
    println(List(zh2en, zh2fr, zh2ja, zh2zh).mkString(","))
  }

  test("hovercraft") {
    transPhone(hovercraft, jaWords, phoneCatSim)
  }

  test("en2zh") {
    transPhone(enWords.take(N), zhWords, phoneCatSim)
  }

  test("Print all Phone mappings") {
    println(en.words.map(w => (w.nativeWord,en.pronunciation(w)) -> IPA.strToPhoneSeq(w.ipa)).toMap.mkString("\n"))
    println(fr.words.map(w => (w.nativeWord,fr.pronunciation(w)) -> IPA.strToPhoneSeq(w.ipa)).toMap.mkString("\n"))
  }

  test("Print all non-IPA Chars") {
    en.asInstanceOf[DictLanguage].printNonIPAchars()
    fr.asInstanceOf[DictLanguage].printNonIPAchars()
  }

  test("Print PhoneSimilarity Distance Matrix") {
    println(PhoneSimilarity.ASJP_DIST)
  }

  test("Sort Words by frequency") {
    val sorted = en.words.toList.filter(_.freq > 0).sortBy(-_.freq)
    println(sorted)
    println("Frequencies for " + sorted.size + "/" + en.words.size)
  }

  test("Simple Greedy Matching") {
    matches = testWords.map(w => {
      val res = w -> PhoneticApproximatorTestHelper.greedyPhoneticApproximation(w, enWords)
      println(res)
      println(res)
      res
    })
  }

  test("Simple Similarity Matching") {
    val N = 10000
    averageMatchDistance(enWords, enWords, N)
    averageMatchDistance(enWords, frWords, N)
    averageMatchDistance(enWords, jaWords, N)
    averageMatchDistance(enWords, zhWords, N)

    averageMatchDistance(frWords, enWords, N)
    averageMatchDistance(frWords, frWords, N)
    averageMatchDistance(frWords, jaWords, N)
    averageMatchDistance(frWords, zhWords, N)

    averageMatchDistance(jaWords, enWords, N)
    averageMatchDistance(jaWords, frWords, N)
    averageMatchDistance(jaWords, jaWords, N)
    averageMatchDistance(jaWords, zhWords, N)

    averageMatchDistance(zhWords, enWords, N)
    averageMatchDistance(zhWords, frWords, N)
    averageMatchDistance(zhWords, jaWords, N)
    averageMatchDistance(zhWords, zhWords, N)

  }

  test("Exact Similarity Matching") {
    matches = testWords.par.map(w => {
      w -> PhoneticApproximatorTestHelper.similarityPhoneticApproximation(PhoneSimilarity.ExactMatchPhoneSimilarity)(w, enWords)
    })
    println(matches.mkString("\n"))
  }

  test("ASJP Similarity Matching") {
    matches = testWords.par.map(w => {
      w -> PhoneticApproximatorTestHelper.similarityPhoneticApproximation(PhoneSimilarity.ASJPPhonePatternSimilarity)(w, enWords)
    })
  }

  test("Mielke Weighted Similarity Matching") {
    matches = testWords.par.map(w => {
      w -> PhoneticApproximatorTestHelper.similarityPhoneticApproximation(PhoneSimilarity.MielkePhonePatternSimilarity, 0, 0)(w, enWords)
    })
  }

  test("Category Similarity Matching") {
    matches = testWords.par.map(w => {
      val res = w -> PhoneticApproximatorTestHelper.similarityPhoneticApproximation(PhoneSimilarity.BasicPhoneCategorySimilarity, 0, 0)(w, enWords)
      res
    })
    println(matches.mkString("\n"))
  }

  test("Simple Similarity Matching with Trie") {
    val approximator = new SimilarityBasedTriePhoneticApproximator(
      new SimilarityWithNull(PhoneSimilarity.ExactMatchPhoneSimilarity), enWords)
    matches = testWords.par.map(w => {
      val res = approximator(Seq(w)).toPair
      w -> res
    })
    println(matches.mkString("\n"))
  }
  test("Mielke Weighted Similarity Matching with Trie") {
    val approximator = new SimilarityBasedTriePhoneticApproximator(
      new SimilarityWithNull(PhoneSimilarity.MielkePhonePatternSimilarity), enWords)
    matches = testWords.par.map(w => {
      val res = approximator(Seq(w)).toPair
      w -> res
    })
    println(matches.mkString("\n"))
  }
  test("Mielke Weighted Similarity Matching with Trie with penalties") {
    val wordPenalty = PhoneticApproximator.wordSeqCostFunc(0.3, 0.3)
    val approximator = new SimilarityBasedTriePhoneticApproximator(
      new SimilarityWithNull(PhoneSimilarity.MielkePhonePatternSimilarity), enWords, wordPenalty)
    matches = testWords.par.map(w => {
      val res = approximator(Seq(w)).toPair
      w -> res
    })
    println(matches.mkString("\n"))
  }
  test("Print zh dictionary") {
    println(zh.words.map(w => (w.nativeWord,zh.pronunciation(w),w.orthographies.mkString(",")) -> IPA.strToPhoneSeq(w.ipa)).take(100).toMap.mkString("\n"))
    println(zh.words.size)
  }

  test("read mielke acoustic distance matrix") {
    val m = PhoneSimilarity.readDistMatrix(Constants.MIELKE_IPA_ACOUSTIC_DIST_MATRIX)
    println(m.toList.sortBy(_._2).mkString("\n"))
  }

  test("make features") {
    val tempFile = File.makeTemp("ipaFeatures")
    val tempFile2 = File.makeTemp("ipaFeatures2")
    PhoneFeatures.makeFeaturesFile(tempFile.path, IPA.phones)
    val map = PhoneFeatures.readFeaturesFile(tempFile.path)
    PhoneFeatures.makeFeaturesFile(tempFile2.path, IPA.phones, map)
    val str1 = tempFile.safeSlurp()
    val str2 = tempFile2.safeSlurp()
    assert(str1 === str2)
    tempFile.delete()
    tempFile2.delete()
  }

  test("make aline similarities") {
    PhoneSimilarity.printPhoneSimilarities(Constants.TRANSPHONER_DATA_DIR + "aline.sim.csv", PhoneSimilarity.AlineSimilarity)
  }

  test("Japanese to IPA") {
    val tests = Seq(
      ("サブキャラクタ","sa.bu.kja.ɾa.ku.ta")
    )

    tests.foreach(t => assert(IPAConversions.katakana2ipa(t._1) === t._2))

    IPAConversions.japaneseTsvToIPA(
      Constants.TRANSPHONER_DATA_DIR + "ja-edict-full.tsv.bz2",
      Constants.TRANSPHONER_DATA_DIR + "ja-edict-full-ipa.tsv.bz2")
  }

}

object PhoneticApproximatorTestHelper {
  def greedyPhoneticApproximation(w: Word, xs: Seq[Word]): (Seq[Word], Double) = {
    val approximator = new GreedyPhoneticApproximator(xs)
    approximator.approximate(Seq(w)).toPair
  }

  def similarityPhoneticApproximation
  (phoneSim: Similarity[Phone] = PhoneSimilarity.ExactMatchPhoneSimilarity,
   wordPenalty: Double = 0, // per word penalty
   infreqPenalty: Double = 0, // per word penalty
   greedy: Boolean = true)
  (w: Word, xs: Seq[Word]): (Seq[Word], Double) = {
    val phoneSeqSim = WeightedLevenshteinSimilarity[Phone](phoneSim)
    val wordPenaltyFunc = PhoneticApproximator.wordSeqCostFunc(wordPenalty, infreqPenalty)
    val approximator = new SimilarityBasedSlowPhoneticApproximator(phoneSeqSim, xs, wordPenaltyFunc, realign = !greedy)
    approximator.approximate(Seq(w)).toPair
  }

  def similarityTriePhoneticApproximation
  (phoneSim: Similarity[Phone] = PhoneSimilarity.ExactMatchPhoneSimilarity,
   wordPenalty: Double = 0, // per word penalty
   infreqPenalty: Double = 0 // per word penalty
    )
  (w: Word, xs: Seq[Word]): (Seq[Word], Double) = {
    val wordPenaltyFunc = PhoneticApproximator.wordSeqCostFunc(wordPenalty, infreqPenalty)
    val approximator = new SimilarityBasedTriePhoneticApproximator(phoneSim, xs, wordPenaltyFunc)
    approximator.approximate(Seq(w)).toPair
  }
}
