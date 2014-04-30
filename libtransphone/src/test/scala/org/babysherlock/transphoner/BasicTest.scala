package org.babysherlock.transphoner

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import org.babysherlock.transphoner.dict._
import org.babysherlock.transphoner.phonetics.{IPA, Phone}
import org.babysherlock.test._
import org.babysherlock.transphoner.dict.Word
import org.babysherlock.transphoner.phonetics.Phone

/**
 * Basic tests for transphoner subcomponents
 * These tests should not take very long and should all pass
 * @author Angel Chang
 */
@RunWith(classOf[JUnitRunner])
class BasicTest extends FunSuite with BeforeAndAfter with ShouldMatchers {

  test("test IPA String to Seq[Phone] matching") {
    val ipaStringTests = Seq(
      ("ɑts", Seq(Phone("ɑ"), Phone("t"), Phone("s"))),
      ("ɑt͡ʃ", Seq(Phone("ɑ"), Phone("t͡ʃ"))),
      ("ɑt͡ʃZɑ", Seq(Phone("ɑ"), Phone("t͡ʃ"), Phone("ɑ"))),
      ("Z", Seq())
    )

    ipaStringTests.foreach{
      case(string,phones) => {
        val res = IPA.strToPhoneSeq(string)
        //println("Test: " + string + " Expect: " + phones.mkString(",") + " Result: " + res.mkString(","))
        assert (res === phones)
      }
    }
  }

  test("test english cleaner") {
    val englishCleaner = new EnglishDictCleaner()
    englishCleaner.ipaDistance("sərə", "mɛn") should be (-.190 plusOrMinus 0.001)
    englishCleaner.ipaDistance("sərə", "ʃəˈre") should be (-.760 plusOrMinus 0.001)
    englishCleaner.ipaDistance("sərə", "ʃəre") should be (-.860 plusOrMinus 0.001)
    assert(englishCleaner.cleanPartial("ˈmɛnʃərəbəl","-sərə-") === "ˈmɛnsərəbəl")
    assert(englishCleaner.cleanPartial("nɒˈstældʒɪk","nə‐") === "nəˈstældʒɪk")
    println(Language.EN.toWordPairs("thebedisniceandsoftbuttherockisveryveryhardindeedorsoithink").mkString("\n"))
  }

  test("test segmentation") {
    assert(Language.ZH.toWordPairs("我不知道") ===
      Seq(
        ("我", Language.ZH.lookup("我")),
        ("不", Language.ZH.lookup("不")),
        ("知道", Language.ZH.lookup("知道"))
      )
    )
    assert(Language.ZH.toWordPairs("浸沒") ===
      Seq(
        ("浸沒", Language.ZH.lookup("浸沒"))
      )
    )
  }

  test("test zh word set same tones") {
    val w1 = Language.ZH.lookup("毐")
    val w2 = Language.ZH.lookup("欸")
    assert(w1.ipa === w2.ipa)
    assert(w1.phones === w2.phones)
    val set = Set(w1)
    val set2 = set + w2
    assert(set2 === Set(w1, w2) )
    val trie = TrieHelper.toPhoneTrie(set2, new WordConverter(false, false))
    assert(TrieHelper.trieGet(trie, w1.phones) === set2)
    println(trie)
  }

  test("test zh word set different tones") {
    val w1 = Language.ZH.lookup("毐")
    val w2 = Language.ZH.lookup("艾")
    assert(w1.ipa != w2.ipa)
    assert(w1.phones != w2.phones)
    val wordConverter = new WordConverter(false, true)
    val p1 = wordConverter.toPhonesToMatch(w1)
    val p2 = wordConverter.toPhonesToMatch(w2)
    assert(p1 === p2)
    val set = Set(w1)
    val set2 = set + w2
    assert(set2 === Set(w1, w2) )
    val trie = TrieHelper.toPhoneTrie(set2, wordConverter)
    assert(TrieHelper.trieGet(trie, p1) === set2)
    println(trie)
  }

  test("test get all") {
    val all = TrieHelper.getAll(Seq(Set("one", "1", "a"), Set("two", "2", "b"), Set("three", "3", "c")))
    println(all)
  }

  test("test word lookup") {
    assert(Language.EN.toWordPairs("salt water") ===
      Seq(
        ("salt", Language.EN.lookup("salt")),
        ("water", Language.EN.lookup("water"))
      )
    )
    assert(Language.EN.toWordPairs("salty water") ===
      Seq(
        ("salty", Language.EN.lookup("salty")),
        ("water", Language.EN.lookup("water"))
      )
    )

  }

  test("test word seq selector") {
    val set1 = Set( Language.EN.lookup("salt"), Language.EN.lookup("salty"), Language.EN.lookup("silky") )
    val set2 = Set( Language.EN.lookup("water"), Language.EN.lookup("ocean"), Language.EN.lookup("river") )
    val set3 = Set( Language.EN.lookup("blue"), Language.EN.lookup("red"), Language.EN.lookup("green") )

    val seqScorer = (ws: Seq[Word]) => ws.map( w => w.freq ).sum
    val sel = new SimpleWordSequenceSelector(seqScorer)
    val selected = sel.select(Seq(set1, set2, set3), 2)
    println("Selected:")
    println(selected.mkString("\n"))
    val ordered = sel.order(Seq(set1, set2, set3))
    println("Ordered:")
    println(ordered.mkString("\n"))
    assert(selected.size === 2)
    assert(ordered.size === 27)
    assert(ordered.take(2) === selected)
  }

  test("test English syllabifier") {
    val syllabifier = new PhonotacticSyllabifier(Constants.LANGS("EN")("syllabify"))
    def syllabify(w: String, stripSyllableMarks: Boolean = false): Seq[String] = {
      Language.EN.lookup(w).fullIpas.map( s => {
        val ipa = if (stripSyllableMarks) {
          s.replace(IPA.syllableSep.ipa, "")
        } else s
        syllabifier.syllabify(ipa)
      })
    }

    assert( syllabify("salty", false) === Seq("ˈsɔl.ti") )
    assert( syllabify("machete", false) === Seq("mə.ˈʃɛ.ti", "mə.ˈtʃɛ.ti") )
    assert( syllabify("salty", true) === Seq("ˈsɔl.ti") )
    assert( syllabify("machete", true) === Seq("mə.ˈʃɛ.ti", "mə.ˈtʃɛ.ti") )

//    for (word <- Language.EN.words.take(100)) {
//      val s = syllabifier.syllabify(word.fullIpas.head)
//      println(word + " -> " + s)
//    }
  }

  test("test alignment phrase") {
    val sim = WeightedLevenshteinSimilarity[Char]()
    val w1 = "I am a water balloon"
    val w2 = "You are a water basket"
    val (score, alignment) = sim.align(w1, w2)
    println("Aligned '" + w1 + "' to '" + w2 + "' with a score of " + score)
    println(alignment.mkString(w1,w2))
  }

  test("test alignment word") {
    val sim = WeightedLevenshteinSimilarity[Char]()
    val w1 = "brik"
    val w2 = "brok"
    val (score, alignment) = sim.align(w1, w2)
    println("Aligned '" + w1 + "' to '" + w2 + "' with a score of " + score)
    println(alignment.mkString(w1,w2))
  }

  test("test stress marking") {
    val words = Language.EN.toWords("salt water balloon are strange")
    val wordConverter = new WordConverter()
    val syllables = wordConverter.toSyllables(words)
    val syllableStrings = syllables.map( s => s.map( t => t.toString() ))
    val expectedSyllableStrings = Seq( Seq("sɔlt"), Seq("ˈwɔ", "təɹ"), Seq("bə","ˈlun"), Seq("ɑɹ"), Seq("stɹeɪndʒ"))
    checkSequence("stress marking", expectedSyllableStrings, syllableStrings )
  }

  test("test Chinese tones") {
    def checkTones(str: String, ipa: String, expectedSyllableIpas: Seq[String], expectedTones: Seq[Seq[String]]) {
      val w = Language.ZH.lookup(str)
      assert( w.orthographies.exists( x => x == str) )
      assert( w.ipa === ipa )
      val wordConverter = new WordConverter()
      val syllables = wordConverter.toSyllables(w)

      val syllableStrings = syllables.map( t => t.toString() )
      val tones = syllables.map( x => x.tones.map( t => t.name ) )
      assert( expectedSyllableIpas === syllableStrings )
      assert( expectedTones ===  tones)

      //println(syllables.map( x => x.tones ))

    }

    checkTones("阿爸父", "a˥˥.pa˥˩.fu˥˩", Seq( "a˥˥", "pa˥˩", "fu˥˩"), Seq( Seq("1"), Seq("4"), Seq("4")))
    checkTones("白水县", "pai˧˥.ʂuei˨˩˦.ɕiɛn˥˩", Seq( "pai˧˥", "ʂuei˨˩˦", "ɕiɛn˥˩"), Seq( Seq("2"), Seq("3"), Seq("4")))
  }

  test("word equality") {
    val rabbitSeq = Language.EN.toWords("rabbit")
    val rabbitWord = Language.EN.lookup("rabbit")
    assert( rabbitWord == rabbitSeq.head )
    assert( rabbitSeq.contains(rabbitWord) )
  }
}
