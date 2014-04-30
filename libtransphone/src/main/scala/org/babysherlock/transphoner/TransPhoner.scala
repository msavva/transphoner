package org.babysherlock.transphoner

import org.babysherlock.transphoner.dict.{Language, WordConverter, Word}
import org.babysherlock.transphoner.imageability.Imageability
import org.babysherlock.transphoner.phonetics._
import org.babysherlock.transphoner.semantics.SemanticSimilarity
import org.babysherlock.transphoner.topics.TopicSimilarity
import scala.collection.mutable

/**
 * Main Transphoner App and objects
 *
 * @author Manolis
 * @author Angel
 */
object TransPhoner extends App {
  def apply(options: TransPhonerOptions) = new TransPhoner(options)
}

// Options for creating a transphoner
case class TransPhonerOptions(
  // Source language to transphone from
  source: Language,
  // Target language to transphone to
  target: Language,
  // Individual phone similarity
  unweightedPhoneSim: Similarity[Phone] = PhoneSimilarity.AlineExactSimilarity,
  // phonetic approximation algorithm (really the search algorithm to use)
  phoneApproxName: String = PhoneticApproximator.defaultName,
  // penalty for using more words
  wordPenalty: Double = 0.0,
  // penalty for using infrequent words
  infreqPenalty: Double = 0.0,
  // how much should the phonetics matter
  phoneticWeight: Double = 1.0,
  // how much should imageability matter
  imageabilityWeight: Double = 0.0,
  // how much should semantic similarity matter
  semanticSimilarityWeight: Double = 0.0,
  // what kind of semantic similarity to use
  semanticSimilarityType: String = SemanticSimilarity.defaultSemanticSimilarityType,
  // how much should orthographic similarity matter
  orthographicSimilarityWeight: Double = 0.0,
  // weight for initial phone/char
  initialMatchWeight: Double = 1.0,
  // weight for language model
  languageModelWeight: Double = 0.0,
  // whether we want to match against syllable breaks
  syllabify: Boolean = false,
  // whether we want to group chinese characters with the same tones together
  ignoreTones: Boolean = false,
  // whether we want to filter out words with ambiguous pronunciations
  filterAmbiguousWord: Boolean = false,
  // Threshold at which to filter rare words based on number of times they occur
  filterRareWordCount: Int = -1,
  // Keep only most common words based on rank
  filterRareWordRank: Int = -1,
  // filter on maximum number of syllables per word
  filterMaxSyllablesPerWord: Int = -1,
 // Filter out the source word from the target words
  filterSourceWord: Boolean = false,
  searchBeamSize: Int = 2,
  includeAlignments: Boolean = true) {
  val wordSequenceSelector: String = WordSequenceSelector.defaultName
  val freqWeight = 1 - infreqPenalty
  val phoneSim =
    if (phoneticWeight == 1.0) unweightedPhoneSim
    else if (phoneticWeight == 0) new ConstantSimilarity[Phone](0,0)
    else new InterpolatedSimilarity(unweightedPhoneSim, phoneticWeight)
  // topics to match against
  var topics: Seq[TopicSimilarity] = Seq()
}

case class TransPhoneScore(score: Double,
                           alignment:  PhoneticAlignment,
                           scoreBreakdown: Map[String,Double] = Map())

class TransPhoner(val options: TransPhonerOptions) {
  val sourceLang: Language = options.source
  val targetLang: Language = options.target
  val targetWords = {
    var tw = options.target.words.toSeq.filter(w => w.ipa.nonEmpty)
    if (options.filterRareWordCount > 0) {
      tw = tw.filter( x => x.count > options.filterRareWordCount)
    }
    if (options.filterRareWordRank > 0) {
      val sorted = tw.sortBy( x => -x.freq )
      tw = sorted.take(options.filterRareWordRank)
    }
    if (options.filterAmbiguousWord) {
      tw = tw.filter( x => x.fullIpas.size == 1 )
    }
    if (options.filterMaxSyllablesPerWord > 0) {
      val wordConverter = new WordConverter(true, false)
      tw = tw.filter( x => wordConverter.toSyllables(x).size <= options.filterMaxSyllablesPerWord )
    }
    tw
  }

  val imageability = Imageability.defaultImageability
  val imageabilityScorer = (ws: Seq[Word]) => ws.map( w => imageability(w) ).sum

  // TODO: Change to scriptFamily and only have orthographic similarity weight if scriptFamilty is same
  val orthographicSimilarityWeight = if (sourceLang.nonLatinScript || targetLang.nonLatinScript) 0.0 else options.orthographicSimilarityWeight

  // Phonetic approximator
  val baseWordPenalty = PhoneticApproximator.wordPenaltyFunc(options.wordPenalty, options.infreqPenalty)
  val imageabilityWordPenalty: Word => Double =
    w => if (options.imageabilityWeight > 0) options.imageabilityWeight*(1.0-imageability(w)) else 0
  val perWordPenalty: Word => Double = w => baseWordPenalty(w) + imageabilityWordPenalty(w)
  val approximator = if (options.phoneticWeight > 0 || orthographicSimilarityWeight == 0) {
    PhoneticApproximator(options.phoneSim, targetWords, options.phoneApproxName,
      PhoneticApproximator.wordSeqCostFunc(perWordPenalty), options.initialMatchWeight, options.syllabify, options.ignoreTones,
      options.searchBeamSize, options.includeAlignments)
  } else {
    PhoneticApproximator.orthographicApproximator(new ExactMatchSimilarity[Char](), targetWords, options.phoneApproxName,
      PhoneticApproximator.wordSeqCostFunc(perWordPenalty), options.initialMatchWeight, options.syllabify, options.ignoreTones,
      options.searchBeamSize, options.includeAlignments)
  }

  val freqScorer = (ws: Seq[Word]) => ws.map( w => w.freq ).sum
  val stressAlignmentScorer = (ws: Seq[Word], alignment: PhoneticAlignment) => {
    alignment.syllableStressAlignmentScore( approximator.wordConverter, ws )
  }
  val lmScorer = if (options.languageModelWeight > 0) WordSequenceSelector.lmSequenceScorer(targetLang) else null
  val useFullSeqScorer = true
  val baseWordSequenceScorer = new SequenceScorer() {
    import scala.collection.mutable
    def apply(ws: Seq[Word], alignment: PhoneticAlignment, scoreStats: mutable.Map[String, Double]) = {
      val freqScore = addStat("freqScore", options.freqWeight*freqScorer(ws), scoreStats)
      val imgScore =  addStat("imgScore", options.imageabilityWeight*imageabilityScorer(ws), scoreStats)
      var score = freqScore + imgScore
      if (lmScorer != null) {
        val lmScore = addStat("lmScore", options.languageModelWeight*lmScorer(ws, alignment), scoreStats)
        score = score + lmScore
      }
      if (targetLang.hasTones) {
        val stressAlignmentScore = addStat("stressAlignmentScore", 0.5*stressAlignmentScorer(ws, alignment), scoreStats)
        score = score + stressAlignmentScore
      }
      score
    }
    override def isDeltaScore: Boolean = !useFullSeqScorer
  }
  val semanticSimilarity = SemanticSimilarity(options.semanticSimilarityType)
  val orthographicSimilarity = WordSimilarity.orthographicSimilarity

  // From sets clustered sets, how to select reasonable word sequences
  //  based on word frequencies or phrase semantics
  // TODO: Pass appropriate options into word sequence selector
  val wordSequenceSelector = WordSequenceSelector(baseWordSequenceScorer, options.wordSequenceSelector)
  val seqSelectorBeamSize = options.searchBeamSize
  val seqSelectorPerClusterSize = options.searchBeamSize

  // source specific word penalty
  def wordSeqCostFn(source: Seq[Word]): WordSeqCostFn = new WordSeqCostFn {
    override def deltaCost(w: Word, prev: Seq[Word], stats: mutable.Map[String, Double] = null): Double = {
      val wordPenalty = perWordPenalty(w)
      // TODO: should get previous score, and can compute delta more efficiently
      val orthoPenalty = if (orthographicSimilarityWeight > 0) {
//        val initialMatchScore = if (options.initialMatchWeight != 1.0) {
//          (options.initialMatchWeight-1)*orthographicSimilarity.charSim.similarity(
//            Option(source.head.orthographies.head.head), Option(w.orthographies.head.head)
//          )
//        } else 0.0
        val initialMatchScore = 0.0
        val simScore = orthographicSimilarity.distanceWords(source, Seq(w))
        orthographicSimilarityWeight*(simScore + initialMatchScore)
//        val prevPenalty = orthographicSimilarity.distanceWords(source, prev)
//        val curPenalty = orthographicSimilarity.distanceWords(source, prev :+ w)
//        options.orthographicSimilarityWeight*(curPenalty-prevPenalty)
      } else 0.0
      val semanticPenalty = if (options.semanticSimilarityWeight > 0.0) {
        options.semanticSimilarityWeight*semanticSimilarity.distance(source, Seq(w))
//        val prevPenalty = semanticSimilarity.distance(source, prev)
//        val curPenalty = semanticSimilarity.distance(source, prev :+ w)
//        options.semanticSimilarityWeight*(curPenalty-prevPenalty)
      } else 0.0
      val topicPenalty = topicCost(Seq(w), stats)
      val invalidWord = if (options.filterSourceWord && source.size == 1) w == source.head else false
      if (invalidWord) Double.PositiveInfinity
      else wordPenalty + orthoPenalty + semanticPenalty + topicPenalty
    }
    override def totalCost(seq: Seq[Word], stats: mutable.Map[String, Double] = null): Double = {
      val invalidWord = if (options.filterSourceWord && source.size == 1) seq.contains(source.head) else false
      val invalidWordPenalty =
        if (invalidWord) addStat("invalidWordPenalty", Double.PositiveInfinity, stats) else 0.0
      val wordsPenalty = seq.map( w => perWordPenalty(w)).sum
      val orthoPenalty = if (orthographicSimilarityWeight > 0) {
        orthographicSimilarityWeight*orthographicSimilarity.distanceWords(source, seq)
      } else 0.0
      val semanticPenalty = if (options.semanticSimilarityWeight > 0) {
        options.semanticSimilarityWeight*semanticSimilarity.distance(source, seq)
      } else 0.0
      if (stats != null) {
        addStat("wordsPenalty", wordsPenalty, stats)
        addStat("orthoPenalty", orthoPenalty, stats)
        addStat("semPenalty", semanticPenalty, stats)
        // Break down the word penalty into its pieces
        addStat("numWordPenalty", options.wordPenalty*seq.size, stats)
        addStat("infreqPenalty", options.infreqPenalty*seq.map( w => 1-w.freq ).sum, stats)
        addStat("imgPenalty", seq.map( w => imageabilityWordPenalty(w)).sum, stats)
      }
      val topicPenalty = topicCost(seq, stats)
      invalidWordPenalty + wordsPenalty + orthoPenalty + semanticPenalty + topicPenalty
    }
    private def topicCost(target: Seq[Word], stats: mutable.Map[String, Double] = null) = {
      var score = 0.0
      for (topic <- options.topics) {
        val topicPenalty = -topic.similarity(source, target)
        if (stats != null) {
          addStat("topicPenalty." + topic.topic, topicPenalty, stats)
        }
        score = score + topicPenalty
      }
      score
    }
  }

  // source specific sequence scorer
  def wordSequenceScorer(source: Seq[Word]) = new SequenceScorer() {
    import scala.collection.mutable
    def apply(ws: Seq[Word], alignment: PhoneticAlignment, scoreStats: mutable.Map[String, Double] = null) = {
      var score = baseWordSequenceScorer(ws, alignment, scoreStats)
      if (options.semanticSimilarityWeight > 0) {
        // add semantic similarity weight
        val semSim = addStat("semSimScore", options.semanticSimilarityWeight*semanticSimilarity.similarity(source, ws), scoreStats)
        score = score + semSim
      }
      // Add in topic similarity scores - just add them up!!!
      for (topic <- options.topics) {
        val topicSim = addStat("topicSimScore." + topic.topic, topic.similarity(source, ws), scoreStats)
        score = score + topicSim
      }
      if (useFullSeqScorer) {
        // Also redo phone similarity here and things that are same for a cluster (same phones)
        if (options.phoneticWeight > 0) {
          // phoneSim already has the phoneticWeight in there
          val levSim = WeightedLevenshteinSimilarity(options.phoneSim)
          val phoneScore = levSim.distance( approximator.toPhones(source), approximator.toPhones(ws) )
          score = score + addStat("phoneticScore", -phoneScore, scoreStats)
        }
        val invalidWord = if (options.filterSourceWord && source.size == 1) ws.contains(source.head) else false
        if (invalidWord) {
          score = score + addStat("invalidWordPenalty", -Double.PositiveInfinity, scoreStats)
        }
        if (options.orthographicSimilarityWeight > 0) {
          val orthoPenalty = orthographicSimilarity.distance(approximator.toChars(source), approximator.toChars(ws))
          score = score + addStat("orthoScore", -options.orthographicSimilarityWeight*orthoPenalty, scoreStats)
        }
        val basePenalties = ws.map( w => baseWordPenalty(w)).sum
        score = score + addStat("baseScore", -basePenalties, scoreStats)
      }
      score
    }
  }

  private def getWords(lang: Language, s: String): Seq[Word] = {
    // Convert from string to sequence of words
    lang.toWords(s)
  }

  /** Runs the transphoner
    * @param source sequence words in source language to convert
    * @return An ordered list of pairs of target words with a score of how good the match was
    *         (low is better, high is worse)
    */
  def transphone(source: Seq[Word]): PhoneticWordsMatch = {
    transphone(source, seqSelectorBeamSize).head
  }

  def transphone(source: String): PhoneticWordsMatch = {
    transphone( getWords(sourceLang, source) )
  }

  def transphone(source: Seq[Word], n: Int): Seq[PhoneticWordsMatch] = {
    // Get approximations grouped by approximator
    val grouped = approximator.approximateGrouped(source, n)(wordSeqCostFn(source))
    val betterWordSequenceSelector = wordSequenceSelector.withSeqScorer( wordSequenceScorer(source) )
    // Use word sequence selector to select best
    betterWordSequenceSelector.selectFromMany(grouped, n, seqSelectorPerClusterSize)
  }

  def transphone(source: String, n: Int): Seq[PhoneticWordsMatch] = {
    transphone( getWords(sourceLang, source) , n)
  }

  def transphoneGrouped(source: Seq[Word]): PhoneticClustersMatch = {
    transphoneGrouped(source, seqSelectorBeamSize).head
  }

  def transphoneGrouped(source: String): PhoneticClustersMatch = {
    transphoneGrouped( getWords(sourceLang, source) )
  }

  def transphoneGrouped(source: Seq[Word], n: Int): Seq[PhoneticClustersMatch] = {
    approximator.approximateGrouped(source, n)(wordSeqCostFn(source))
  }

  def transphoneGrouped(source: String, n: Int): Seq[PhoneticClustersMatch] = {
    transphoneGrouped( getWords(sourceLang, source) , n)
  }

  def score(source: String, target: String): TransPhoneScore = {
    val sourceWords = getWords(sourceLang, source)
    val targetWords = getWords(targetLang, target)
    score( sourceWords, targetWords )
  }

  def score(source: Seq[Word], target: Seq[Word]): TransPhoneScore = {
    // Given source and target sequence, compute optimal alignment and score
    val levSim = WeightedLevenshteinSimilarity(options.phoneSim)
    // Phonetic similarity used for aligning source and target
    val (phoneScore, aligned) = levSim.align( approximator.toPhones(source), approximator.toPhones(target) )
    val phoneAlignment = PhoneticAlignment( approximator.wordConverter, source, target, aligned)
    // Per word cost that is also considered when doing the trie search
    val wordSeqCostStats = new mutable.HashMap[String, Double]()
    val wordSeqCost = wordSeqCostFn(source).totalCost(target, wordSeqCostStats)
    // main word sequence score
    val wss = wordSequenceScorer(source)
    val mainWordSeqScoreStats = new mutable.HashMap[String, Double]()
    val mainWordSeqScore = wss(target, phoneAlignment, mainWordSeqScoreStats)
    val score = if (wss.isDeltaScore) -phoneScore - wordSeqCost + mainWordSeqScore else mainWordSeqScore
    val wordSeqCostLabel = if (wss.isDeltaScore) "wordSeqScore1" else "wordSeqScoreInTrie"
    val wordSeqScoreLabel = if (wss.isDeltaScore) "wordSeqScore2" else "wordSeqScore"
    val scoreBreakdown = Map(
     "phoneticScore" -> -phoneScore,
      wordSeqCostLabel  -> -wordSeqCost,
      wordSeqScoreLabel  -> mainWordSeqScore
    ) ++ wordSeqCostStats.map( kv => wordSeqCostLabel + "." + kv._1 -> kv._2).toMap ++
      mainWordSeqScoreStats.map( kv => wordSeqScoreLabel + "." + kv._1 -> kv._2).toMap
    TransPhoneScore(score, phoneAlignment, scoreBreakdown)
  }

  def getPhoneSimilarity: Similarity[Phone] = options.phoneSim

}

