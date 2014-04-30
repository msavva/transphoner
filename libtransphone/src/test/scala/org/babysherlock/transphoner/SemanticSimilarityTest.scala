package org.babysherlock.transphoner

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import org.babysherlock.transphoner.dict.Language
import org.babysherlock.transphoner.semantics.SemanticSimilarity

/**
  * Tests for semantic similarity
  * @author Angel Chang
  */
@RunWith(classOf[JUnitRunner])
class SemanticSimilarityTest extends FunSuite with BeforeAndAfter with ShouldMatchers {
   case class TestEntry(phrase1: String, lang1: String, phrase2: String, lang2: String,
                        similarity: Map[String,Double] = Map()) {
     def toShortString() = phrase1 + "(" + lang1 + ")" + "-" + phrase2 + "(" + lang2 + ")"
   }

   test("test basic semantic similarity")  {
     val simTypes = Seq("jaccard", "bow", "wordnetJCSJaccard", "wordnetJCS", "distCosine", "tfidfBow", "conceptnet")
     val testEntries = Seq(
       TestEntry("water balloon", "EN", "water balloon", "EN",
         Map("jaccard" -> 1.0, "bow" -> 1.0, "wordnetJCSJaccard" -> 1.0, "wordnetJCS" -> 1.0, "distCosine" -> 0.8943988049396899, "conceptnet" -> 0.99777132058096 )),
       TestEntry("water", "EN", "eau", "FR",
         Map("jaccard" -> 0.2727272727272727, "bow" -> 0.6323755137004572,
           "wordnetJCSJaccard" -> 0.2727272727272727, "wordnetJCS" -> 0.751558979035945, "distCosine" -> 0.7996741883438755, "conceptnet" -> 0.9315365535175193 )),
       TestEntry("water balloon", "EN", "fire ball", "EN",
         Map("jaccard" -> 0.0, "bow" -> 0.13027647565660216,
           "wordnetJCSJaccard" -> 0.20946924348756285, "wordnetJCS" -> 0.5222967636337118, "distCosine" -> 0.4416119730681969, "conceptnet" -> 0.6256887334168736)),
       // TODO: Improve lookup of water balloon in wordnet (also, sense similarity should be improved)
       TestEntry("water balloon", "EN", "水气球", "ZH",
         Map("jaccard" -> 0.0, "bow" -> 0.2959181776442077,
           "wordnetJCSJaccard" -> 0.06456855690573995, "wordnetJCS" -> 0.5139176051527778, "distCosine" -> 0.4410419460648979, "conceptnet" -> 0.002363900535273373)),
       TestEntry("water balloon", "EN", "水 气球", "ZH",
         Map("jaccard" -> 0.3888888888888889, "bow" -> 0.8835219412725585,
           "wordnetJCSJaccard" -> 0.5022980427609459, "wordnetJCS" -> 0.8173220144090982, "distCosine" ->  0.785537880260186, "conceptnet" -> 0.0021830816403705924)),
       TestEntry("water", "EN", "Wasser", "DE",
         Map("jaccard" -> 0.09090909090909091, "bow" -> 0.5081209799713002,
           "wordnetJCSJaccard" -> 0.09090909090909091, "wordnetJCS" -> 0.6070678932665038, "distCosine" ->  0.7464957527020936, "conceptnet" -> 0.9702436629963569)),
       TestEntry("balloon", "EN", "Ballon", "DE", // Ballon not DE
         Map("jaccard" -> 0.4, "bow" -> 0.8260098415097766,
           "wordnetJCSJaccard" -> 0.4, "wordnetJCS" -> 0.7965820080219319, "distCosine" ->  0.7988474879890902, "conceptnet" -> 0.0)),
       TestEntry("balloon", "EN", "ballon", "FR",
         Map("jaccard" -> 0.0, "bow" -> 0.030098468581458364,
           "wordnetJCSJaccard" -> 0.40954997533931614, "wordnetJCS" -> 0.5138448465033005, "distCosine" ->  0.2603234616202819, "conceptnet" -> 0.5694011775907418)),
       TestEntry("brücke", "DE", "brook", "EN",
         Map("jaccard" -> 0.0, "bow" -> 0.12179187300135745,
           "wordnetJCSJaccard" -> 0.25734842030888383, "wordnetJCS" -> 0.5142277912289915, "distCosine" ->  0.2834506422836999, "conceptnet" -> 0.8155041490886571)),
        TestEntry("sperre", "DE", "Sherry", "EN",
         Map("jaccard" -> 0.0, "bow" -> 0.0,
           "wordnetJCSJaccard" -> 0.509875344293141, "wordnetJCS" -> 0.509875344293141, "distCosine" ->  0.0048199755561771146, "conceptnet" -> 0.0)),
       TestEntry("haben", "DE", "Hagen", "EN",
         Map("jaccard" -> 0.0, "bow" -> 0.0,
           "wordnetJCSJaccard" -> 0.0, "wordnetJCS" -> 0.0, "distCosine" ->  0.0, "conceptnet" -> 0.0))
     )
     for (simType <- simTypes) {
       val semanticSimilarity = SemanticSimilarity(simType)
       for (t <- testEntries) {
         val lang1 = Language(t.lang1)
         val lang2 = Language(t.lang2)
         val p1 = lang1.toWords(t.phrase1)
         val p2 = lang2.toWords(t.phrase2)
         val s = semanticSimilarity.similarity(p1,p2)
         println( simType + ": " + t.toShortString + " -> " + s)
         val expectedSim = t.similarity.getOrElse(simType, Double.NaN)
         if (!expectedSim.isNaN ) {
           withClue("Testing " + simType + " " + t.toShortString + ": ")(s should be (expectedSim plusOrMinus 0.001))
         }
       }
     }
   }

  test("test semantic similarity ordering")  {
    val simType = "wordnetJCS"
    val semanticSimilarity = SemanticSimilarity(simType)
    val lang1 = Language("DE")
    val w1 = lang1.lookup("brücke")
    val lang2 = Language("EN")
    val lang2Words = lang2.words //Set(lang2.lookup("brook"), lang2.lookup("bridge"))
    val n = 10
    println("Considering " + lang2Words.size + " options")
    val topa = semanticSimilarity.mostSimilar(w1, lang2Words, n, useSim = true)
    println("Most similar " + n + " words for " + w1 + " (using sim)")
    println(topa.mkString("\n"))
    val topb = semanticSimilarity.mostSimilar(w1, lang2Words, n, useSim = false)
    println("Most similar " + n + " words for " + w1 + " (using dist)")
    println(topb.mkString("\n"))
    val bottoma = semanticSimilarity.mostDifferent(w1, lang2Words, n, useSim = true)
    println("Most different " + n + " words for " + w1 + " (using sim)")
    println(bottoma.mkString("\n"))
    val bottomb = semanticSimilarity.mostDifferent(w1, lang2Words, n, useSim = false)
    println("Most differnt " + n + " words for " + w1 + " (using dist)")
    println(bottomb.mkString("\n"))

  }
}
