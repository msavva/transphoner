package org.babysherlock.transphoner.topics

import org.babysherlock.transphoner.dict.{Word, Language}
import org.babysherlock.transphoner.semantics.SemanticSimilarity
import org.babysherlock.nlp.{UWN, WordNetWrapper}
import net.sf.extjwnl.data.POS

case class TopicSimilarity(
                            // String to use as a topic
                            topic: String,
                            // Language to use for interpreting the topic string
                            topicLang: Language,
                            // how much should the topic similarity matter
                            similarityWeight: Double = 1.0,
                            // what kind of topic similarity to use
                            similarityType: String = SemanticSimilarity.defaultSemanticSimilarityType,
                            // use wordNet to get a list of hyponyms?
                            useHyponyms: Boolean = false
                            ) {
  // topic as separate words
  lazy val topicWords = topicLang.toWords(topic)
  // similarity function
  lazy val topicSimilarity = if (similarityType != null) SemanticSimilarity(similarityType) else null
  lazy val topicHyponyms = getHyponyms()

  // TODO: We can skip topic similarity for some words
  def similarity(sourceWords: Seq[Word], targetWords: Seq[Word]): Double = {
    similarity(targetWords)
  }
  def similarity(words: Seq[Word]): Double = {
    var parts = 0
    val sim = if (topicSimilarity != null) {
      parts += 1
      topicSimilarity.similarity(topicWords, words)
    } else 0.0
    val hypoSim = if (useHyponyms) {
      parts += 1
      val nHypos = words.map( x => if (topicHyponyms.contains(x.orthographies.head.toLowerCase)) 1.0 else 0.0).sum
      nHypos/words.length
    } else 0.0
    val totalSim = if (parts > 0) (sim + hypoSim)/parts else 0.0
    similarityWeight*totalSim
  }
  def getHyponyms(): Set[String] = {
    getHyponyms(topicLang)
  }
  private def _getWordnetHyponyms(): Set[String] = {
    val wordNet = WordNetWrapper
    // TODO: If topic language is not english - lookup appropriate synset first
    val hyponyms: Set[String] = wordNet.getAllHyponyms(POS.NOUN, topic)
    //println(hyponyms.mkString(","))
    //println("Got " + hyponyms.size)
    hyponyms.map( x => x.toLowerCase() ).filter( x => x.size > 1 )
  }
  def getHyponyms(targetLang: Language): Set[String] = {
    val hsynsets = UWN.getAllHyponymSynsets(topic, topicLang.uwnCode, Option('n'))
    val uwnLex = hsynsets.map( synsetId => UWN.lookupSynsetLexicalizations(synsetId, Set(targetLang.uwnCode))).flatten
    val uwnTerms = uwnLex.map( x => x.getObject.getTermStr ).filter( x => x.size > 1 )
    //println("Got " + uwnTerms.size)
    if (topicLang.id == "EN" && targetLang.id == "EN") {
      val wordnetTerms = _getWordnetHyponyms()
      uwnTerms.map( x => x.toLowerCase ) ++ wordnetTerms
    } else {
      uwnTerms
    }
  }
  def getTopSimilarWords(n: Int): Seq[(Word,Double)] = {
    getTopSimilarWords(topicLang.words, n)
  }
  def getTopSimilarWords(targetWords: Set[Word], n: Int): Seq[(Word,Double)] = {
    if (topicSimilarity != null) {
      // TODO: take all topic words into account
      topicSimilarity.mostSimilar(topicWords.head, targetWords, n, useSim = true)
    } else null
  }
}

