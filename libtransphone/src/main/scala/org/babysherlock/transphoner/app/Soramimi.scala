package org.babysherlock.transphoner.app

import org.babysherlock.transphoner.{Alignment, TransPhonerOptions, TransPhoner}
import org.babysherlock.transphoner.dict.Language
import org.babysherlock.transphoner.topics.{TopicSimilarity,TopicSimilarityWordsSelector}
import edu.stanford.nlp.util.logging.Redwood
import org.babysherlock.transphoner.phonetics.PhoneticWordsMatch
import org.babysherlock.nlp.StanfordCorenlpWrapper

/**
 * Does topic based homophonic translation
 * @author Angel Chang
 */
object SoramimiApp extends App {
  val inputLangId = "EN"
  val outputLangId = "EN"

  val inputText =
    "Sweet dreams are made of this\n" +
    "Who am I to disagree\n" +
    "I travel the world and the seven seas\n" +
    "Everybody is looking for something"
  val reference =
    "Sweet dreams are made of cheese\n" +
    "Who am I to diss a Brie\n" +
    "I cheddar the World and the feta cheese\n" +
    "Everybody is looking for Stilton\n"
  val topic = "food"
  val topicLangId = "EN"
  val soramimi = Soramimi(topic, topicLangId, inputLangId, outputLangId)
  soramimi.transformOne(inputText)
}

object Soramimi {
  def apply(topic: String, topicLang: Language, inputLang: Language, outputLang: Language): Soramimi = {
    val transphonerOptions = TransPhonerOptions(
      inputLang, outputLang,
      phoneticWeight = 1.0, imageabilityWeight = 0.0, semanticSimilarityWeight = 0, orthographicSimilarityWeight = 0.0, languageModelWeight = 0.0,
      syllabify = true,
      searchBeamSize = 10,
      filterSourceWord = true  // Filter source word (important!)
    )
    val topicSim = TopicSimilarity(topic = topic, topicLang = topicLang, similarityWeight = 2.0, similarityType = null, useHyponyms = true)
    transphonerOptions.topics = Seq(topicSim)

    val transphoner = TransPhoner(transphonerOptions)
    val soramimi = new Soramimi(transphoner, topicSim)
    soramimi
  }
  def apply(topic: String, inputLangId: String, outputLangId: String): Soramimi = Soramimi(topic, "EN", inputLangId, outputLangId)
  def apply(topic: String, topicLangId: String, inputLangId: String, outputLangId: String): Soramimi =
    Soramimi(topic, Language(topicLangId), Language(inputLangId), Language(outputLangId))

  case class PartialPhraseTransform(wordIndexStart: Int, wordIndexEnd: Int,
                                    transformation: String,
                                    /*alignment: Alignment, */score: Double)
  case class PhraseTransform(parts: Seq[PartialPhraseTransform], score: Double) {
    def this(partial: PartialPhraseTransform) = this(Seq(partial), partial.score)
  }

  case class PhraseSuggestions(inputPhraseWords: IndexedSeq[String],
                               transformations: IndexedSeq[PhraseTransform] // List of suggestions for the phrase
                              )

  case class Phrases(phrases: IndexedSeq[PhraseSuggestions])
  case class PhoneticPhraseMatch(inputPhrase: String, phoneticMatch: PhoneticWordsMatch)
}

/**
 * Does topic based homophonic transformation (http://en.wikipedia.org/wiki/Homophonic_translation) of a block of text.
 * The term soramimi (http://en.wikipedia.org/wiki/Soramimi) applies when we transform song lyrics.
 * @param transphoner
 * @param topicSim
 */
class Soramimi(val transphoner: TransPhoner,
               val topicSim: TopicSimilarity) {
  private val transphonerOptions = transphoner.options
  private val inputLang = transphoner.sourceLang
  private val outputLang = transphoner.targetLang
  private val topicWordsSelector = new TopicSimilarityWordsSelector(topicSim)
  private val topicTerms: Set[String] = getTopicTerms()

  private def getTopicTerms(): Set[String] = {
    val topicWords = topicWordsSelector.getTopicWords(outputLang).map( x => x.orthographies.head.toLowerCase )
    //println("Got topic words for " + topicSim.topic + ": " + topicWords.size + "\n" + topicWords.mkString("\n"))
    topicWords
  }

  def transform(inputText: String) = transformAll(inputText)

  /**
   * Transforms all words
   * @param inputText
   */
  def transformAll(inputText: String): IndexedSeq[Soramimi.PhoneticPhraseMatch] = {
    val phrases = inputText.split("\\n+")
    for (phrase <- phrases) yield {
      // Do whole phrase
      val output1 = transphoner.transphone(phrase)
      Redwood.Util.debug(output1.target)
      // Do one word at a time
      //    val words = phrase.split("\\s+")
      //    val output = for (w <- words) yield {
      //      val out = transphoner.transphone(w)
      //      out.target
      //    }
      //    println(output.flatten.mkString(" "))
      Soramimi.PhoneticPhraseMatch(phrase, output1)
    }
  }

  /**
   * Transforms one word per phrase
   * @param inputText
   */
  def transformOne(inputText: String, includeTransphonerMultiWordSuggestions: Boolean = false): Soramimi.Phrases = {
    val phrases = inputText.split("\\n+")
    val transformedPhrases = for (phrase <- phrases) yield {
      // For each phrase, choose one word to replace with a topic appropriate word
      val words = phrase.split("\\s+")
      val topicWordReplacements = for ((word,wordIndex) <- words.zipWithIndex) yield {
        transphonerOptions.topics = Seq(topicSim)
        val w = inputLang.toWord(word)
        if (w != null) {
          // Get top ten transphoner options for this word
          val ts = transphoner.transphone(Seq(w), 10)
          //println(t.target.map( x => x.orthographies.head ).mkString(" "))
          val transphonerSuggestions = new scala.collection.mutable.ArrayBuffer[(String,Double)]()
          transphonerOptions.topics = Seq()
          // Only consider replacements that have a topic word
          if (includeTransphonerMultiWordSuggestions) {
            for (t <- ts) {
              if (t.target.size > 1 && t.target.exists( x => topicTerms.contains(x.orthographies.head.toLowerCase) )) {
                val s2 = transphoner.score(Seq(w), t.target)
                val suggestion = (t.target.map( x => x.orthographies.head).mkString(" "), s2.score-(t.target.size-1))
                transphonerSuggestions.append(suggestion)
              }
            }
          }
          // Consider all possible topic terms (scored by transphoner)
          val topicTermScores = (for (c <- topicTerms) yield {
            val s2 = transphoner.score(word, c)
            (c,s2.score)
          }).toSeq
          // Take transphoner suggestions and topic terms and get score
          // (filtering out options that are the same as the original word)
          var scores: Seq[(String,Double)] = (transphonerSuggestions ++ topicTermScores)
            .filter( x => x._1 != w.orthographies.head.toLowerCase )
          scores = scores.map( x => (x._1, x._2/w.phones.size) )
          Redwood.Util.debug(word + ": " + scores.toSeq.sortBy( x => -x._2 ).mkString(" "))
          val best = scores.maxBy( x => x._2 )
          Soramimi.PartialPhraseTransform(wordIndex, wordIndex+1, best._1, best._2)
        } else {
          Soramimi.PartialPhraseTransform(wordIndex, wordIndex+1, word, Double.NegativeInfinity)
        }
      }
      val selected = topicWordReplacements.maxBy( x => x.score )
      val output = words.zipWithIndex.map( x => if (x._2 == selected.wordIndexStart) selected.transformation else x._1 )
      Redwood.Util.debug(output.mkString(" "))
      val transformedPhrase = Soramimi.PhraseSuggestions( words.toIndexedSeq,
        topicWordReplacements.filter( x => !x.score.isNegInfinity)
          .map( x => new Soramimi.PhraseTransform(x) )
          .sortBy( x => -x.score )
      )
      transformedPhrase
    }
    Soramimi.Phrases(transformedPhrases.toIndexedSeq)
  }
}
