package org.babysherlock.transphoner

import org.babysherlock.transphoner.dict.{Language, Word}
import org.babysherlock.transphoner.phonetics.{PhoneticClustersMatch, PhoneticWordsMatch, PhoneticAlignment}
import org.babysherlock.util.BoundedPriorityQueue
import scala.collection.JavaConversions._

/**
 * Interfaces for selecting word sequences
 * @author Angel Chang
 */
// Selects word sequence from sequence of clusters/sets of words
trait WordSequenceSelector {
  def select(setSeq: Seq[Set[Word]], n: Integer): Seq[(Seq[Word], Double)] = selectFromOne(setSeq, n)
  def select(setSeqs: Seq[PhoneticClustersMatch], total: Integer, perCluster: Integer): Seq[PhoneticWordsMatch] = selectFromMany(setSeqs, total, perCluster)
  def order(setSeq: Seq[Set[Word]]) = select(setSeq, Integer.MAX_VALUE)

  def selectFromOne(setSeq: Seq[Set[Word]], n: Integer): Seq[(Seq[Word], Double)] = selectFromOne(setSeq, null, n)
  def selectFromOne(setSeq: Seq[Set[Word]], alignment: PhoneticAlignment, n: Integer): Seq[(Seq[Word], Double)]
  def selectFromMany(setSeqs: Seq[PhoneticClustersMatch], total: Integer, perCluster: Integer): Seq[PhoneticWordsMatch]

  def seqScorer: SequenceScorer
  def withSeqScorer(seqScorer: SequenceScorer): WordSequenceSelector
}

import scala.collection.mutable
trait SequenceScorer extends Function3[Seq[Word], PhoneticAlignment, mutable.Map[String, Double], Double] {
  def apply(w: Seq[Word], alignment: PhoneticAlignment): Double = apply(w, alignment, null)
  def isDeltaScore: Boolean = false

  // Internal helper function
  protected def addStat(name: String, value: Double, stats: mutable.Map[String, Double]): Double = {
    if (stats != null) {
      stats.update(name, value)
    }
    value
  }
}

object WordSequenceSelector {
  val defaultName = "MostFreq"

  def apply(wordSequenceScorer: SequenceScorer,
            selectorType: String): WordSequenceSelector = {
//    val wordSelector =
//      selectorType match {
//        case "MostFreq" => MostFreqWordSelect
//        case "Order" => WordOrderWordSelector
//        case _ => null
//      }
    new SimpleWordSequenceSelector(wordSequenceScorer, None)
  }

  // Language model scorer
  def lmSequenceScorer(lang: String): SequenceScorer =
    if (Constants.LANGS(lang).contains("lmfile")) new BerkeleyGoogleNGramSequenceScorer(lang) else null
  def lmSequenceScorer(lang: Language): SequenceScorer =
    if (Constants.LANGS(lang.id).contains("lmfile")) new BerkeleyGoogleNGramSequenceScorer(lang) else null

  class BerkeleyGoogleNGramSequenceScorer(lang: Language, lmFilename: String, vocabFilename: String) extends SequenceScorer {
    def this(lang: Language) = this(lang, Constants.LANGS(lang.id)("lmfile"), Constants.LANGS(lang.id)("lmvocab"))
    def this(lang: String) = this(Language(lang))
    val lm = edu.berkeley.nlp.lm.io.LmReaders.readGoogleLmBinary(lmFilename, vocabFilename)
    val wordIndexer = lm.getWordIndexer
    val unkSymbol =  wordIndexer.getUnkSymbol
    val unkIndex = wordIndexer.getIndexPossiblyUnk(unkSymbol)
    val useProb = false
    def apply(words: Seq[Word], alignment: PhoneticAlignment, scoreStats: mutable.Map[String, Double]): Double = {
      // Get the orthography that comes first (simplified characters comes from traditional)
      var ngram = words.map( w => w.orthographies.toSeq.sorted.head )

      // for chinese language model split unknown words into single character per word...
      if (lang.id == "ZH") {
        ngram = ngram.map( s => if (wordIndexer.getIndexPossiblyUnk(s) != unkIndex) Seq(s) else s.map( c => c.toString ) ).flatten
      }

//      if (ngram.size > lm.getLmOrder) {
//        println("target length larger than " + lm.getLmOrder)
//      }
      //val lp = lm.getLogProb( ngram.toList )
      val lp = scoreWords( ngram.toIndexedSeq )
      if (useProb) math.exp(lp) else lp
    }

    private def scoreWords(s: IndexedSeq[String]) = {
      var sentenceScore = 0.0
      val lmOrder = lm.getLmOrder
      for (i <- 1 until math.min(lmOrder, s.size())) {
        val ngram = s.subList(0, i)
        val scoreNgram = lm.getLogProb(ngram.toList)
        sentenceScore = sentenceScore + scoreNgram
      }
      for (i <- lmOrder to s.size()) {
        val ngram = s.subList(i - lmOrder, i)
        val scoreNgram = lm.getLogProb(ngram.toList)
        sentenceScore = sentenceScore + scoreNgram
      }
      sentenceScore
    }
  }
}

class SimpleWordSequenceSelector(override val seqScorer: SequenceScorer,
                                 val wordSelector: Option[WordSelector]) extends WordSequenceSelector {

  def this(seqScorer: Seq[Word] => Double, wordSelector: Option[WordSelector] = None) =
    this( new SequenceScorer() {
            def apply(x: Seq[Word], alignment: PhoneticAlignment, stats: mutable.Map[String,Double]) = seqScorer(x) },
          wordSelector)

  val ordering: Ordering[(Seq[Word], Double)] = Ordering[Double].on( x => -x._2 )
  val wordsMatchOrdering: Ordering[PhoneticWordsMatch] = Ordering[Double].on( x => -x.score )

  //def seqScorer: (Seq[Word], PhoneticAlignment) => Double = defaultSeqScorer
  def withSeqScorer(newSeqScorer: SequenceScorer) = new SimpleWordSequenceSelector(
    seqScorer = newSeqScorer,
    wordSelector
  )

  override def selectFromOne(setSeq: Seq[Set[Word]], alignment: PhoneticAlignment, n: Integer)
                            : Seq[(Seq[Word], Double)] = {
    // selects up to n from each set and then picks the n best overall scoring ones
    val reducedSetSeq = if (wordSelector.isDefined) {
      setSeq.map( s => wordSelector.get.select(s,n) )
    } else {
      setSeq.map( s => s.zipWithIndex.map( p => (p._1, 1.0/(p._2+1.0) ) ) )
    }
    // Keep simple beam search...
    // TODO: optimize this...
    var best: Seq[(Seq[Word], Double)] = Seq( (Seq(), 0) )
    for (set <- reducedSetSeq) {
      val newBest = new BoundedPriorityQueue[(Seq[Word], Double)]( n )(ordering)
      for (p <- set; prev <- best) {
        val seq = prev._1 :+ p._1
        val score = seqScorer( seq, alignment )
        newBest += ( (seq, score) )
      }
      best = newBest.dequeueAll.toSeq.reverse
    }
    best
  }

  override def selectFromMany(setSeqs: Seq[PhoneticClustersMatch], total: Integer, perCluster: Integer): Seq[PhoneticWordsMatch] = {
    var queue = new BoundedPriorityQueue[PhoneticWordsMatch]( total )(wordsMatchOrdering)
    for (setSeq <- setSeqs) {
      val selected = selectFromOne(setSeq.target, setSeq.alignment, perCluster)
      for (s <- selected) {
        // add score of cluster (setSeq.score) if seqScorer is only a delta score
        val score = if (seqScorer.isDeltaScore) s._2 + setSeq.score else s._2
        queue += PhoneticWordsMatch(s._1, setSeq.alignment, score )
      }
    }
    queue.dequeueAll.toSeq.reverse
  }
}

// Select words from word cluster/sets
trait WordSelector {
  def select(set: Set[Word], n: Integer): Seq[(Word, Double)]
  def order(set: Set[Word]) = select(set, Integer.MAX_VALUE)
}

// Select words by score (top score first)
class ScoreBasedWordSelector(scorer: Word => Double) {
  def select(set: Set[Word], n: Integer): Seq[(Word, Double)] = {
    set.toSeq.map(w => (w, scorer(w))).sortBy( -_._2 ).take(n)
  }
}

// Select words by frequency
object MostFreqWordSelect extends ScoreBasedWordSelector( w => w.freq )

// Select the words that appears first (uses inverse rank as score)
object WordOrderWordSelector extends WordSelector {
  def select(set: Set[Word], n: Integer): Seq[(Word, Double)] = {
    set.toSeq.take(n).zipWithIndex.map( p => (p._1, 1.0/(p._2+1.0) ) )
  }
}


