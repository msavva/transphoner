package org.babysherlock.transphoner.semantics

import org.babysherlock.transphoner.dict.{Language, Word}
import org.babysherlock.transphoner.{WeightedUnorderedAlignSimilarity, SimilarityWithCache, Similarity}
import org.babysherlock.nlp._
import org.babysherlock.similarity.SimilarityModels._
import org.babysherlock.util.SoftLRUCache
import org.goobs.sim.{DistSim, Ontology}
import edu.stanford.nlp.stats.{Counters, ClassicCounter, Counter}

/**
 * Gives the semantic similarity of two terms
 * @author Angel Chang
 */
object SemanticSimilarity {
  val defaultSemanticSimilarityType = "jaccard"
  private def sigmoid(x:Double):Double = 1.0 / (1.0 + scala.math.exp(-x))

  object WordnetPathJaccardSimilarity extends WordnetSenseJaccardUWNSemanticSimilarity(_.path)
  object WordnetResnikJaccardSimilarity extends WordnetSenseJaccardUWNSemanticSimilarity(_.resnik)
  object WordnetLinJaccardSimilarity extends WordnetSenseJaccardUWNSemanticSimilarity(_.lin)
  object WordnetJCSJaccardSimilarity extends WordnetSenseJaccardUWNSemanticSimilarity(_.jc)
  object WordnetWuPalmerJaccardSimilarity extends WordnetSenseJaccardUWNSemanticSimilarity(_.wuPalmer)
  object WordnetLeskJaccardSimilarity extends WordnetSenseJaccardUWNSemanticSimilarity(_.approximateLesk)
  /** Careful -- I'm very slow! */
  object WordnetELeskJaccardSimilarity extends WordnetSenseJaccardUWNSemanticSimilarity(_.eLesk)

  object WordnetPathSimilarity extends WordnetSenseUWNSemanticSimilarity(_.path, sigmoid)
  object WordnetResnikSimilarity extends WordnetSenseUWNSemanticSimilarity(_.resnik, sigmoid)
  object WordnetLinSimilarity extends WordnetSenseUWNSemanticSimilarity(_.lin, sigmoid)
  object WordnetJCSSimilarity extends WordnetSenseUWNSemanticSimilarity(_.jc, sigmoid)
  object WordnetWuPalmerSimilarity extends WordnetSenseUWNSemanticSimilarity(_.wuPalmer)
  object WordnetLeskSimilarity extends WordnetSenseUWNSemanticSimilarity(_.approximateLesk)
  /** Careful -- I'm very slow! */
  object WordnetELeskSimilarity extends WordnetSenseUWNSemanticSimilarity(_.eLesk)

  object DistributionalCosSimilarity extends DistributionalUWNSemanticSimilarity(_.cos)
  object DistributionalAngleSimilarity extends DistributionalUWNSemanticSimilarity(_.angle, x => x/math.Pi)
  object DistributionalJensenShannonSimilarity extends DistributionalUWNSemanticSimilarity(_.jensenShannon)
  object DistributionalHellingerSimilarity extends DistributionalUWNSemanticSimilarity(_.hellinger)
  object DistributionalJaccardSimilarity extends DistributionalUWNSemanticSimilarity(_.jaccard)
  object DistributionalDiceSimilarity extends DistributionalUWNSemanticSimilarity(_.dice)

  val jaccard = new JaccardSemanticSimilarity()
  val bow = new BowSenseSemanticSimilarity()
  val tfidfBow = new BowSenseTfidfSemanticSimilarity()
  val conceptnet = new ConceptNetSemanticSimilarity()

  val semanticSimilarityTypes = Map(
    "wordnetPath" -> WordnetPathSimilarity,
    "wordnetResnik" -> WordnetResnikSimilarity,
    "wordnetLin" -> WordnetLinSimilarity,
    "wordnetJCS" -> WordnetJCSSimilarity,
    "wordnetWuPalmer" -> WordnetWuPalmerSimilarity,
    "wordnetLesk" -> WordnetLeskSimilarity,
    "wordnetELesk" -> WordnetELeskSimilarity,
    "wordnetPathJaccard" -> WordnetPathJaccardSimilarity,
    "wordnetResnikJaccard" -> WordnetResnikJaccardSimilarity,
    "wordnetLinJaccard" -> WordnetLinJaccardSimilarity,
    "wordnetJCSJaccard" -> WordnetJCSJaccardSimilarity,
    "wordnetWuPalmerJaccard" -> WordnetWuPalmerJaccardSimilarity,
    "wordnetLeskJaccard" -> WordnetLeskJaccardSimilarity,
    "wordnetELeskJaccard" -> WordnetELeskJaccardSimilarity,
    "distCosine" -> DistributionalCosSimilarity,
    "distAngle" -> DistributionalAngleSimilarity,
    "distJS" -> DistributionalJensenShannonSimilarity,
    "distHellinger" -> DistributionalHellingerSimilarity,
    "distJaccard" -> DistributionalJaccardSimilarity,
    "distDice" -> DistributionalDiceSimilarity,
    "jaccard" -> jaccard,
    "tfidfBow" -> tfidfBow,
    "bow" -> bow,
    "conceptnet" -> conceptnet
  )
  val names = semanticSimilarityTypes.keySet

  // TODO: Have options and construct appropriate similarity
  def apply(semanticSimilarityType: String = defaultSemanticSimilarityType): SemanticSimilarity = {
    val st = semanticSimilarityTypes.getOrElse(semanticSimilarityType, null)
    if (st == null) {
      throw new RuntimeException("Unknown semantic similarity type: " + semanticSimilarityType)
    }
    st
  }
}

trait SemanticSimilarity extends Similarity[Seq[Word]] {
  type Phrase = Seq[Word]
  type WordSimilarity = Similarity[Word]
  type PhraseSimilarity = Similarity[Phrase]

  def uncachedWordSimilarity: WordSimilarity
  def uncachedPhraseSimilarity: PhraseSimilarity

  lazy val cachedWordSimilarity: WordSimilarity = new SimilarityWithCache[Word](uncachedWordSimilarity)
  lazy val cachedPhraseSimilarity: PhraseSimilarity = new SimilarityWithCache[Phrase](uncachedPhraseSimilarity)

  def wordSimilarity = cachedWordSimilarity
  def phraseSimilarity = cachedPhraseSimilarity

  override def distance(t1: Seq[Word], t2: Seq[Word]): Double =  phraseSimilarity.distance(t1,t2)
  override def similarity(t1: Seq[Word], t2: Seq[Word]): Double =  phraseSimilarity.similarity(t1,t2)

  def distance(t1: Word, t2: Word): Double = distance(Seq(t1), Seq(t2))
  def similarity(t1: Word, t2: Word): Double = similarity(Seq(t1), Seq(t2))
  def mostSimilar(t: Word, candidates: Set[Word], n: Int, useSim: Boolean): Seq[(Word,Double)] =
    mostSimilar(Seq(t), candidates.map( x => Seq(x) ), n, useSim).map( x => (x._1.head, x._2) )
  def mostDifferent(t: Word, candidates: Set[Word], n: Int, useSim: Boolean): Seq[(Word,Double)] =
    mostDifferent(Seq(t), candidates.map( x => Seq(x) ), n, useSim).map( x => (x._1.head, x._2) )
}

// Semantic similarity that looks directly at words (not senses)
abstract class WordBasedSemanticSimilarity extends SemanticSimilarity {
  def sim: Similarity[String]
  def getId(w: String, lang: String): String

  protected lazy val aggrSim = new WeightedUnorderedAlignSimilarity(
    new SimilarityWithCache(sim))

  class WordBasedWordSimilarity extends WordSimilarity {
    def distance(t1: Word, t2: Word): Double = -similarity(t1,t2)

    override def similarity(word1: Word, word2: Word): Double = {
      similarity(word1.orthographies.head, word1.lang.id, word2.orthographies.head, word2.lang.id)
    }
    def similarity(w1: String, lang1: Language, w2: String, lang2: Language): Double = similarity(w1, lang1.id, w2, lang2.id)
    def similarity(w1: String, lang1: String, w2: String, lang2: String): Double = {
      // Get similarity between words
      val c1 = getId(w1, lang1)
      val c2 = getId(w2, lang2)
      sim.similarity(c1, c2)
    }
  }

  class WordBasedPhraseSimilarity extends PhraseSimilarity {
    def distance(t1: Phrase, t2: Phrase): Double = -similarity(t1,t2)

    override def similarity(phrase1: Phrase, phrase2: Phrase): Double = {
      val phraseWords1 = phrase1.map( w => getId(w.orthographies.head, w.lang.id) )
      val phraseWords2 = phrase2.map( w => getId(w.orthographies.head, w.lang.id) )
      phraseWordSimilarity(phraseWords1, phraseWords2)
    }
    def similarity(p1: Seq[String], lang1: Language, p2: Seq[String], lang2: Language): Double
    = similarity(p1, lang1.id, p2, lang2.id)
    def similarity(p1: Seq[String], lang1: String, p2: Seq[String], lang2: String): Double = {
      val phraseWords1 = p1.map( w => getId(w, lang1) )
      val phraseWords2 = p2.map( w => getId(w, lang2) )
      phraseWordSimilarity(phraseWords1, phraseWords2)
    }
  }

  override val uncachedWordSimilarity = new WordBasedWordSimilarity
  override val uncachedPhraseSimilarity = new WordBasedPhraseSimilarity

  def phraseWordSimilarity(words1: Seq[String], words2: Seq[String]): Double = {
    aggrSim.similarity(words1.map(x =>(x,1.0)), words2.map(x=>(x,1.0)))
  }
}

class ConceptNetSemanticSimilarity extends WordBasedSemanticSimilarity {
  val conceptNet = new ConceptNet()
  override def getId(w: String, lang: String) = conceptNet.toConceptId(w, lang)
  override val sim = new Similarity[String] {
    override def distance(t1: String, t2: String): Double = 1 - similarity(t1,t2)
    override def similarity(t1: String, t2: String): Double = conceptNet.getSimilarity(t1,t2).getOrElse(0.0)
  }
}

// word level semantic similarity using UWN as mediator between languages
abstract class UWNSemanticSimilarity extends SemanticSimilarity {
  val uwn = UWN

  class UWNWordSimilarity extends WordSimilarity {
    def distance(t1: Word, t2: Word): Double = -similarity(t1,t2)

    override def similarity(word1: Word, word2: Word): Double = {
      similarity(word1.orthographies.head, word1.lang.uwnCode, word2.orthographies.head, word2.lang.uwnCode)
    }
    def similarity(w1: String, lang1: Language, w2: String, lang2: Language): Double = similarity(w1, lang1.uwnCode, w2, lang2.uwnCode)
    def similarity(w1: String, lang1: String, w2: String, lang2: String): Double = {
      // Lookup senses for words and get similarity
      val s1 = lookupSenses_(w1, lang1)
      val s2 = lookupSenses_(w2, lang2)
      senseSimilarity(s1, s2)
    }
  }

  class UWNPhraseSimilarity extends PhraseSimilarity {
    def distance(t1: Phrase, t2: Phrase): Double = -similarity(t1,t2)

    override def similarity(phrase1: Phrase, phrase2: Phrase): Double = {
      val phraseSenses1 = phrase1.map( w => lookupSenses_(w.orthographies.head, w.lang.uwnCode) )
      val phraseSenses2 = phrase2.map( w => lookupSenses_(w.orthographies.head, w.lang.uwnCode) )
      phraseSenseSimilarity(phraseSenses1, phraseSenses2)
    }
    def similarity(p1: Seq[String], lang1: Language, p2: Seq[String], lang2: Language): Double
      = similarity(p1, lang1.uwnCode, p2, lang2.uwnCode)
    def similarity(p1: Seq[String], lang1: String, p2: Seq[String], lang2: String): Double = {
      val phraseSenses1 = p1.map( w => lookupSenses_(w, lang1) )
      val phraseSenses2 = p2.map( w => lookupSenses_(w, lang2) )
      phraseSenseSimilarity(phraseSenses1, phraseSenses2)
    }
  }

  override val uncachedWordSimilarity = new UWNWordSimilarity
  override val uncachedPhraseSimilarity = new UWNPhraseSimilarity

  private def lookupSenses_ (w: String, lang: String): Seq[(String, Double)] = {
    //val synsets = uwn.lookupSynsets(w, lang)
    //synsets.map( s => (s.getObject.getId, s.getWeight.toDouble)).toSeq
    uwn.lookupWeightedSynsetIds(w, lang)
  }

  // Returns the similarity based on set of senses
  def senseSimilarity(senses1: Seq[(String, Double)], senses2: Seq[(String, Double)]): Double = {
    if (senses1.isEmpty || senses2.isEmpty) 0
    else senseSimilarityImpl(senses1, senses2)
  }

  // Returns the similarity based on set of senses
  def phraseSenseSimilarity(senses1: Seq[Seq[(String, Double)]], senses2: Seq[Seq[(String, Double)]]): Double = {
    val ps1 = senses1.flatten
    val ps2 = senses2.flatten
    senseSimilarity(ps1, ps2)
  }

  def senseSimilarity(senseId1: String, senseId2: String): Double =
    senseSimilarityImpl(Seq((senseId1,1)), Seq((senseId2,1)))

  // Implement this
  protected def senseSimilarityImpl(senses1: Seq[(String, Double)], senses2: Seq[(String, Double)]): Double

}

// Some simple semantic similarities
class BowSenseSemanticSimilarity extends UWNSemanticSimilarity {
  lazy val bowCache = SoftLRUCache[String, Counter[String]](200000)
  val useCache = true

  protected def bowOfSynsetImpl(synsetId: String, weight: Double = 1, counter: Counter[String] = new ClassicCounter[String]() ): Counter[String] = {
    val glosses = uwn.lookupGlosses(synsetId).map( s => s.getObject.getTermStr.toLowerCase )
    StanfordCorenlpWrapper.getBowFeatures(glosses, StanfordCorenlpWrapper.isNJRV, weight, counter)
  }

  def bowOfSynset(synsetId: String, weight: Double = 1, counter: Counter[String] = new ClassicCounter[String]() ): Counter[String] = {
    if (useCache) {
      val cached = bowCache.getOrElse(synsetId)(
        bowOfSynsetImpl(synsetId, weight)
      )
      counter.addAll(cached)
      counter
    } else {
      bowOfSynsetImpl(synsetId, weight, counter)
    }
  }

  def bowOfSynsets(senses: Seq[(String, Double)]) = {
    val bow = new ClassicCounter[String]()
    for (sense <- senses) {
      bowOfSynset(sense._1, sense._2, bow)
    }
    bow
  }

  // We do a bow on the the senses of the words and compute the semantic similarity
  override def senseSimilarityImpl(senses1: Seq[(String, Double)], senses2: Seq[(String, Double)]): Double = {
    val bow1 = bowOfSynsets(senses1)
    val bow2 = bowOfSynsets(senses2)
    Counters.cosine(bow1, bow2)
  }
}

class BowSenseTfidfSemanticSimilarity extends BowSenseSemanticSimilarity {
  lazy val wordnet = WordNetWrapper
  lazy val wordnetSynsetMap = WordNet3xSenseMapping

  override protected def bowOfSynsetImpl(synsetId: String, weight: Double = 1, counter: Counter[String] = new ClassicCounter[String]() ): Counter[String] = {
    val wn31id = wordnetSynsetMap.wn30To31(synsetId)
    if (wn31id.nonEmpty) {
      val tfidf = wordnet.solrIndex.getSynsetTfidf(wn31id)
      for ( (s,w) <- tfidf ) {
        counter.incrementCount(s, weight*w)
      }
    }
    counter
  }

}

class JaccardSemanticSimilarity extends UWNSemanticSimilarity {
  override def senseSimilarityImpl(senses1: Seq[(String, Double)], senses2: Seq[(String, Double)]): Double = {
    // number of overlapping senses / number of union senses (jaccard)
    val s1Set = senses1.map( x => x._1 ).toSet
    val s2Set = senses2.map( x => x._1 ).toSet
    val intersect = s1Set.intersect(s2Set)
    val union = s1Set.union(s2Set)
    if (union.size == 0) 0 else intersect.size.toDouble/union.size.toDouble
  }

//  def senseSimilarity(senseId1: String, senseId2: String): Double =
//    if (senseId1 == senseId2) 1 else 0
}

// Semantic similarity is based on wordnet sense
class WordnetSenseJaccardUWNSemanticSimilarity(val simFn: Ontology.Similarity => Double)
                                              (implicit wordnet:()=>Ontology) extends UWNSemanticSimilarity {

  private def convertSenseId(s: String) = uwn.toWordNetSynsetId(s)
  private def toSenseSet(senses: Seq[(String, Double)]): Set[String] = {
    val s = senses.map( x => convertSenseId(x._1) ).toSet
    // Filter out senses not recognizes by the wordnet() (like a01768724)
    s.filter( x => wordnet().id2node.contains(x) )
  }

  override def senseSimilarityImpl(senses1: Seq[(String, Double)], senses2: Seq[(String, Double)]): Double = {
    val s1Set = toSenseSet(senses1)
    val s2Set = toSenseSet(senses2)
    println("Doing synset sim of " + s1Set.mkString(",") + " with " + s2Set.mkString(","))
    //wordnet().synsetSim(s1Set, s2Set).aggregateMax( simFn )
    // TODO: Debug
    wordnet().synsetSim(s1Set, s2Set).aggregateJaccard( simFn )
  }

//  override def senseSimilarity(senseId1: String, senseId2: String): Double = {
//    val s1 = convertSenseId(senseId1)
//    val s2 = convertSenseId(senseId2)
//    simFn(wordnet().synsetSim(s1, s2))
//  }
}

// Similarity between wordnet senses
class WordNetSenseSimilarity(val simFn: Ontology.Similarity => Double,
                             val toWordNetId: String => String = s => s,
                             val normalizeFn: Double => Double = x => x)
                             (implicit wordnet:()=>Ontology) extends Similarity[String]
{
  override def distance(s1: String, s2: String): Double = - similarity(s1,s2)
  override def similarity(s1: String, s2: String): Double = {
    // Filter out senses not recognizes by the wordnet() (like a01768724)
    val ws1 = toWordNetId(s1)
    val ws2 = toWordNetId(s2)
    if (wordnet().id2node.contains(ws1) && wordnet().id2node.contains(ws2)) {
      val rawsim = simFn(wordnet().synsetSim(ws1, ws2))
      normalizeFn(rawsim)
    } else 0.0  // What can we do?
  }
}

abstract class AggregateSenseSemanticSimilarity extends UWNSemanticSimilarity {
  protected def aggrSim: Similarity[Seq[(String, Double)]]
  protected def senseSimilarityImpl(senses1: Seq[(String, Double)], senses2: Seq[(String, Double)]): Double = {
    aggrSim.similarity(senses1, senses2)
  }
}

class WordnetSenseUWNSemanticSimilarity(val simFn: Ontology.Similarity => Double,
                                        val normalizeFn: Double => Double = x => x)
                                       (implicit wordnet:()=>Ontology)
  extends AggregateSenseSemanticSimilarity {
  override protected val aggrSim = new WeightedUnorderedAlignSimilarity(
    new SimilarityWithCache(
      new WordNetSenseSimilarity(simFn, s => uwn.toWordNetSynsetId(s), normalizeFn)(wordnet)))
}


// Semantic similarity is based on words (typically English words)
class WordBasedUWNSemanticSimilarity(val sim:Similarity[String]) extends UWNSemanticSimilarity {
  private def senseWords(s: String) = {
    // Go from senses to words
    val lex = uwn.lookupSynsetLexicalizations(s, Set("eng"))
    lex.map( x => x.getObject.getTermStr )
  }
  private def senseWords(s: Seq[(String,Double)]): Seq[(String,Double)] = {
    s.map( p => {
      val sw = senseWords(p._1)
      sw.map( x => (x, p._2/sw.size))
    }).flatten
  }

  protected def aggrSim: Similarity[Seq[(String, Double)]] = new WeightedUnorderedAlignSimilarity(sim)
  protected def senseSimilarityImpl(senses1: Seq[(String, Double)], senses2: Seq[(String, Double)]): Double = {
    val w1 = senseWords(senses1)
    val w2 = senseWords(senses2)
    aggrSim.similarity(w1, w2)
  }
}

class DistributionalWordSimilarity(simFn: DistSim.Similarity => Double,
                                   val normalizeFn: Double => Double = x => x)
                                  (implicit distsim:()=>DistSim) extends Similarity[String]
{
  override def distance(s1: String, s2: String): Double = 1 - similarity(s1,s2)
  override def similarity(s1: String, s2: String): Double =
    normalizeFn(distsim().sim(s1,s2).map( simFn ).getOrElse(0.0))
}

class DistributionalUWNSemanticSimilarity(simFn: DistSim.Similarity => Double,
                                          val normalizeFn: Double => Double = x => x)
                                         (implicit distsim:()=>DistSim)
  extends WordBasedUWNSemanticSimilarity(new DistributionalWordSimilarity(simFn, normalizeFn)(distsim))




