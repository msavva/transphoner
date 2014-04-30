package org.babysherlock.transphoner.topics

import org.babysherlock.nlp.ConceptNet
import org.babysherlock.transphoner.dict.{Language, Word}

/**
 * Get list of topic words
 * @author Angel Chang
 */
trait TopicWordsSelector {
  def topic: String
  def topicLang: Language
  // Returns the top topic words with scores
  // TODO: This implementation may return less than the number requested due to filtering of known words...
  def getTopicWordScores(outputLang: Language, limit: Int = -1): Seq[(Word,Double)]
    = getTopicTermScores(outputLang, limit).map( x => (outputLang.toWord(x._1), x._2) ).filter( x => x._1 != null )
  // Returns a set of topic words
  def getTopicWords(outputLang: Language, limit: Int = -1): Set[Word]
    = getTopicWordScores(outputLang, limit).map( x => x._1 ).toSet

  // Returns the top topic terms (strings) with scores
  def getTopicTermScores(outputLang: Language, limit: Int = -1): Seq[(String,Double)]
  // Returns a set of topic terms (strings)
  def getTopicTerms(outputLang: Language, limit: Int = -1): Set[String]
    = getTopicTermScores(outputLang, limit).map( x => x._1 ).toSet
}

class ConceptNetTopicWordsSelector(override val topic: String,
                                   override val topicLang: Language,
                                   val useSimilar: Boolean = true) extends TopicWordsSelector {
  val conceptNet = new ConceptNet()
  override def getTopicTermScores(outputLang: Language, limit: Int = -1) = {
    val clangid = conceptNet.toLangId(outputLang.id)
    val conceptNetTopicTerms =
      if (useSimilar) conceptNet.getSimilar(conceptNet.toConceptId(topic, topicLang.id), limit, clangid)
      else conceptNet.getRelated(conceptNet.toConceptId(topic, topicLang.id), limit, clangid )
    val topicTerms = conceptNetTopicTerms.map( x => (conceptNet.toWord(x._1, clangid), x._2) )
    topicTerms
  }
}

class TopicSimilarityWordsSelector(val topicSim: TopicSimilarity) extends TopicWordsSelector {
  override def topic = topicSim.topic
  override def topicLang = topicSim.topicLang
  override def getTopicTermScores(outputLang: Language, limit: Int = -1) = {
    if (topicSim.useHyponyms) {
      val hyponyms = topicSim.getHyponyms(outputLang)
      // TODO: Improve topic similarity score...
      hyponyms.toSeq.map( x => (x, 1.0))
    } else {
      topicSim.getTopSimilarWords(outputLang.words, limit).map( x => (x._1.orthographies.head, x._2) )
    }
  }
}
