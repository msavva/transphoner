package org.babysherlock.transphoner

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.babysherlock.transphoner.imageability.AoAImageability
import org.babysherlock.transphoner.dict.Language

/**
 * Tests for imageability estimation
 * @author Angel Chang
 */
@RunWith(classOf[JUnitRunner])
class ImageabilityTest extends FunSuite with BeforeAndAfter {
  case class TestWord(word: String, langCode: String, imageability: Double = Double.NaN)

  test("test AoA imageability")  {
    val imageability = new AoAImageability(true)
    val words = Seq(
      TestWord("a", "eng"),
      TestWord("s", "eng"),
      TestWord("t", "eng"),
      TestWord("decadent", "eng"),
      TestWord("orange", "eng"),
      TestWord("apple", "eng"),
      TestWord("peach", "eng"),
      TestWord("water", "eng"),
      TestWord("fuzzy", "eng"),
      TestWord("eau", "fra"),
      TestWord("arroser", "fra"),
      TestWord("不太好", "cmn"),
      TestWord("水", "cmn"),
      TestWord("纳", "cmn"),
      TestWord("納", "cmn"),
      TestWord("水", "jpn"),
      TestWord("水をかける", "jpn"),
      TestWord("Wasser", "deu")
    )
    for (w <- words) {
      val i = imageability.imageability(w.word, w.langCode)
      println( "(" + w.word + "," + w.langCode + ")" + " -> " + i)
      if (!w.imageability.isNaN ) {
        assert( w.imageability === i )
      }
      // Make sure that if we look up the word from the dictionary, and retrieve the imageability with it,
      // we get the same value
      val lang = Language.fromIso6393(w.langCode)
      if (lang != null) {
        val word = lang.lookup(w.word)
        if (word != null) {
          val idict = imageability(word)
          assert( idict === i, "imagineability for " + w + " does not match imagineability for " + word )
        } else {
          println("Cannot find " + w.word + " in " + lang.id)
        }
      } else {
        println("Cannot find language from iso6393 " + w.langCode)
      }
    }
  }

}
