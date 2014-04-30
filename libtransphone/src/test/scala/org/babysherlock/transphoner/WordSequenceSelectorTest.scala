package org.babysherlock.transphoner

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import org.babysherlock.transphoner.dict.Language

/**
 * Tests the word sequence selector
 * @author Angel Chang
 */
@RunWith(classOf[JUnitRunner])
class WordSequenceSelectorTest extends FunSuite with BeforeAndAfter with ShouldMatchers {
  case class TestEntry(words: String, score: Double = Double.NaN)

  test("test chinese lm")  {
    val lang = Language.ZH
    val wordSeqScorer = WordSequenceSelector.lmSequenceScorer(lang)
    val testPhrases = Seq(
      TestEntry("不太好"),
      TestEntry("不纳好")
    )
    for (phrase <- testPhrases) {
      val words = lang.toWords(phrase.words)
      val score = wordSeqScorer(words, null)
      println(phrase.words + ": " + score)
    }

  }

}
