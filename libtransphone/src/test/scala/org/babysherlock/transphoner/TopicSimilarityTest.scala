package org.babysherlock.transphoner

import org.babysherlock.nlp.ConceptNet
import org.babysherlock.transphoner.dict.Language
import org.babysherlock.transphoner.topics.{TopicSimilarityWordsSelector, TopicSimilarity}
import org.babysherlock.test._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.matchers.ShouldMatchers

/**
 * Test topic similarity similarity
 * @author Angel Chang
 */
@RunWith(classOf[JUnitRunner])
class TopicSimilarityTest extends FunSuite with BeforeAndAfter with ShouldMatchers {
  test("topic similarity: Food") {
    val topic = "food"
    val topicLangId = "EN"
    val outputLangId = "EN"

    val topicLang = Language(topicLangId)
    val outputLang = Language(outputLangId)

    val topicSim = TopicSimilarity(topic = topic, topicLang = topicLang, similarityWeight = 2.0, similarityType = null, useHyponyms = true)
    // Check our topic sim
    println("sim to stilton: " + topicSim.similarity(outputLang.toWords("stilton")))
    checkDouble(0.0001)("sim to cheese", topicSim.similarity(outputLang.toWords("cheese")), 2.0)
    checkDouble(0.0001)("sim to brie", topicSim.similarity(outputLang.toWords("brie")), 2.0)
    checkDouble(0.0001)("sim to cheddar", topicSim.similarity(outputLang.toWords("cheddar")), 2.0)

    checkDouble(0.0001)("sim to this", topicSim.similarity(outputLang.toWords("this")), 0.0)
  }

  test("Transphoner score with topic similarity: Food") {
    val topic = "food"
    val inputLangId = "EN"
    val topicLangId = "EN"
    val outputLangId = "EN"

    val topicLang = Language(topicLangId)
    val inputLang = Language(inputLangId)
    val outputLang = Language(outputLangId)

    val transphonerOptions = TransPhonerOptions(
      inputLang, outputLang,
      phoneticWeight = 1.0, imageabilityWeight = 0.0, semanticSimilarityWeight = 0, orthographicSimilarityWeight = 0.0, languageModelWeight = 0.0,
      syllabify = true,
      searchBeamSize = 10
    )
    val topicSim = TopicSimilarity(topic = topic, topicLang = topicLang, similarityWeight = 2.0, similarityType = null, useHyponyms = true)
    transphonerOptions.topics = Seq(topicSim)
    val transphoner = TransPhoner(transphonerOptions)
      println("score between this and cheese: " + transphoner.score("this", "cheese").scoreBreakdown.mkString(","))
      println("score between travel and cheddar: " + transphoner.score("travel", "cheddar").scoreBreakdown.mkString(","))
      println("score between travel and brie: " + transphoner.score("travel", "brie").scoreBreakdown.mkString(","))
      println("score between this and this: " + transphoner.score("this", "this").scoreBreakdown.mkString(","))



  }

  test("Food topic: saffron") {
    val topic = "food"
    val topicLangId = "EN"
    val outputLangId = "EN"

    val topicLang = Language(topicLangId)
    val outputLang = Language(outputLangId)

    val topicSim = TopicSimilarity(topic = topic, topicLang = topicLang, similarityWeight = 2.0, similarityType = null, useHyponyms = true)
    val topicWordsSelector = new TopicSimilarityWordsSelector(topicSim)
    val topicWords = topicWordsSelector.getTopicWords(outputLang).map( x => x.orthographies.head.toLowerCase )
    val topicTerms = topicWordsSelector.getTopicTerms(outputLang)
    assert(topicTerms.contains("saffron"))
    assert(topicWords.contains("saffron"))
  }

  test("ConceptNet topic words") {
    val topic = "food"
    val topicLangId = "EN"
    val outputLangId = "EN"

    val topicLang = Language(topicLangId)
    val outputLang = Language(outputLangId)

    val conceptNet = new ConceptNet()
    val clangid = conceptNet.toLangId(outputLang.id)
    val conceptNetTopicWordsSimilar = conceptNet.getSimilar(conceptNet.toConceptId(topic, topicLang.id), 10, clangid )
    println(conceptNetTopicWordsSimilar.mkString(","))
    val expectedSimilar = Seq(
      ("/c/en/food",0.9996186725655827),
      ("/c/en/soul_food",0.9425460853693911),
      ("/c/en/health_food",0.9414507000386841),
      ("/c/en/sushi",0.9329522054324512),
      ("/c/en/life-form",0.9324405243809015),
      ("/c/en/pabulum",0.9323869772102088),
      ("/c/en/potato_and_chicken",0.9323708796037238),
      ("/c/en/rice_and_beef",0.9323708796037238),
      ("/c/en/micronutrient",0.9323194321452085),
      ("/c/en/chyme",0.9322377405994084)
    )
    checkSequence("ConceptNet words similar to food", conceptNetTopicWordsSimilar, expectedSimilar, checkScored(0.0001)_)

    val conceptNetTopicWordsRelated = conceptNet.getRelated(conceptNet.toConceptId(topic, topicLang.id), 10, clangid )
    println(conceptNetTopicWordsRelated.mkString(","))
    val expectedRelated = Seq(
      ("/c/en/cake",395.4161071777344),
      ("/c/en/plate",188.46868896484375),
      ("/c/en/rice",163.68185424804688),
      ("/c/en/soup",123.40237426757812),
      ("/c/en/bread",115.87687683105469),
      ("/c/en/chicken",94.90338134765625),
      ("/c/en/corn",76.06338500976562),
      ("/c/en/egg",53.244651794433594),
      ("/c/en/eat",38.86443328857422),
      ("/c/en/taste",31.99445343017578)
    )
    checkSequence("ConceptNet words related to food", conceptNetTopicWordsRelated, expectedRelated, checkScored(0.0001)_)

  }

}
