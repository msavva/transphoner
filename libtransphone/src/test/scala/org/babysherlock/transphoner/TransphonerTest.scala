package org.babysherlock.transphoner

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.babysherlock.transphoner.phonetics.PhoneSimilarity
import org.babysherlock.transphoner.dict.Language

/**
 * Basic tests for transphoner
 * Update as transphoner is improved
 * When updating make sure that stress/tone is still retained
 * @author Angel Chang
 */
@RunWith(classOf[JUnitRunner])
class TransphonerTest  extends FunSuite with BeforeAndAfter {

  def transphoneAndCheck(options: TransPhonerOptions,
                         n: Int,
                         toCheck: Seq[(String,Seq[String])]) {
    val transphoner = new TransPhoner(options)
    for ( (input, expected) <- toCheck) {
      val results = transphoner.transphone(input, n)
      val wordIpas = results.map( r => {
        // Words
        val word = r.target.map(w => w.nativeWord).mkString(" ")
        // IPA with stress marks
        val ipa = r.alignment.syllableAlignment.target.mkString
        word + ":/" + ipa + "/" + " " + r.score
      } )
      println(wordIpas.mkString("\n"))
      // TODO: enable asserts
      //assert(wordIpas === expected)
    }
  }

  def transphoneAndCheck(src: String,
                         tgt: String,
                         n: Int,
                         toCheck: Seq[(String,Seq[String])]) {
    val srcLang = Language(src)
    val tgtLang = Language(tgt)
    val options = TransPhonerOptions(
      source = srcLang,
      target = tgtLang,
      unweightedPhoneSim = PhoneSimilarity.AlineExactSimilarity,
      phoneApproxName = "SimTrie",
      wordPenalty = 0, infreqPenalty = 0, syllabify = true, ignoreTones = true, searchBeamSize = 2*n
    )
    transphoneAndCheck(options, n, toCheck)
  }

  def transphoneAndCheckWithLM(src: String,
                               tgt: String,
                               n: Int,
                               toCheck: Seq[(String,Seq[String])]) {
    val srcLang = Language(src)
    val tgtLang = Language(tgt)
    val options = TransPhonerOptions(
      source = srcLang,
      target = tgtLang,
      unweightedPhoneSim = PhoneSimilarity.AlineExactSimilarity,
      phoneApproxName = "SimTrie",
      languageModelWeight = 1.0,
      wordPenalty = 0, infreqPenalty = 0, syllabify = true, ignoreTones = true, searchBeamSize = 2*n
    )
    transphoneAndCheck(options, n, toCheck)
  }

  test("test EN-DE") {
    transphoneAndCheck("EN", "DE", 5, Seq(
      ("hovercraft", Seq("hebelkraft:/ˈhɛbəlˌkʀaft/",
        "he wohl kneift:/ˈheːvoːlˌknaɪft/",
        "hosen klappt:/ˈhoːzənˌklapt/",
        "hosen knecht:/ˈhoːzənˌknɛçt/",
        "hosen kläfft:/ˈhoːzənˌklɛfft/"))
    ))
  }

  test("test EN-FR") {
    transphoneAndCheck("EN", "FR", 5, Seq(
      ("hovercraft", Seq("rond vent huit coin fête:/ˈʁovɥiˌkwɛˌfɛt/",
        "révolte classe t':/ˈʁevɔltˌklasˌt/",
        "rond vent huit quoi stand:/ˈʁovɥiˌkwaˌsta/",
        "rond vent huit quoi Styx:/ˈʁovɥiˌkwaˌsti/",
        "rond vent huit coin Styx:/ˈʁovɥiˌkwɛˌsti/"))
    ))
  }

  test("test EN-JA") {
    transphoneAndCheck("EN", "JA", 5, Seq(
      ("hovercraft", Seq("ホ ず や キャー 府:/ˈhozujaˌkjaːˌɸu/",
        "ヘザー や キャー 府:/ˈhezaːjaˌkjaːˌɸu/",
        "ヘザー 湯 Ｑ 府:/ˈhezaːjuˌkjuːˌɸu/",
        "ヘザー 湯 虚 府:/ˈhezaːjuˌkjoˌɸu/",
        "ヘザー 湯 っきゃ 府:/ˈhezaːjuˌkjaˌɸu/"))
    ))
  }

  test("test EN-ZH") {
    transphoneAndCheck("EN", "ZH", 5, Seq(
      ("hovercraft", Seq("龢（和） 死 二杆子:/ˈxɤ˧˥s˨˩˦ər˥˩ˌkan˥˥ˌts/",
        "嚇（吓） 分工 子:/ˈxɤ˥˩fən˥˥ˌkuŋ˥˥ˌts˨˩˦/",
        "嚇（吓） 分工 自:/ˈxɤ˥˩fən˥˥ˌkuŋ˥˥ˌts˥˩/",
        "嚇（吓） 分工 資（资）:/ˈxɤ˥˩fən˥˥ˌkuŋ˥˥ˌts˥˥/",
        "嚇（吓） 分工 詞（词）:/ˈxɤ˥˩fən˥˥ˌkuŋ˥˥ˌts˧˥/"))
    ))
  }

  test("test EN-ZH with LM") {
    transphoneAndCheckWithLM("EN", "ZH", 5, Seq(
      ("hovercraft", Seq("龢（和） 死 二杆子:/ˈxɤ˧˥s˨˩˦ər˥˩ˌkan˥˥ˌts/",
        "嚇（吓） 分工 子:/ˈxɤ˥˩fən˥˥ˌkuŋ˥˥ˌts˨˩˦/",
        "嚇（吓） 分工 自:/ˈxɤ˥˩fən˥˥ˌkuŋ˥˥ˌts˥˩/",
        "嚇（吓） 分工 資（资）:/ˈxɤ˥˩fən˥˥ˌkuŋ˥˥ˌts˥˥/",
        "嚇（吓） 分工 詞（词）:/ˈxɤ˥˩fən˥˥ˌkuŋ˥˥ˌts˧˥/")),
      ("water balloon", Seq()),
      ("pest", Seq()),
      ("ratatouille", Seq())
    ))
  }

  test("test ZH-EN") {
    transphoneAndCheck("ZH", "EN", 5, Seq(
      ("阿爸父", Seq("Ain pie fou:/ɛˈpaɪˈfu/",
        "Ain pie foh:/ɛˈpaɪˈfɔ/",
        "Ain pie Sue:/ɛˈpaɪˈsu/",
        "Ain pie thaw:/ɛˈpaɪˈθɔ/",
        "ow pow fou:/aʊˈpaʊˈfu/"))
    ))
  }

  test("check transphone score") {
    val transphoner = TransPhoner(TransPhonerOptions(Language("EN"), Language("EN")))
    var score = transphoner.score("brook", "brick")
    println(score.score)
    println(score.scoreBreakdown.mkString("\n"))

    // TODO: Unknown word - should print something sensible
    score = transphoner.score("brook", "briok")
    println(score.score)
    println(score.scoreBreakdown.mkString("\n"))

    score = transphoner.score("watermelon", "apple")
    println(score.score)
    println(score.scoreBreakdown.mkString("\n"))
  }

  test("check transphone score filterSourceWord") {
    val transphoner = TransPhoner(TransPhonerOptions(Language("EN"), Language("EN"), filterSourceWord = true))
    val score = transphoner.score("rabbit", "rabbit")
    println(score.score)
  }
}
