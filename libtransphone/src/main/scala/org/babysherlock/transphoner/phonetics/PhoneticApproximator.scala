package org.babysherlock.transphoner.phonetics

import org.babysherlock.transphoner._
import org.babysherlock.transphoner.dict._
import org.babysherlock.util.BoundedTreeSet
import edu.stanford.nlp.tokensregex.matcher.{Match, MatchCostFunction, TrieMapMatcher, TrieMap}
import java.util.{List => JList}
import java.util.Comparator
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.Some

/**
 * Find closest phonetic approximation
 *
 * @author Manolis Savva
 * @author Angel Chang
 **/
abstract class PhoneticApproximator(
                                    // cost of selecting this word sequence
                                    val defaultWordSeqCostFn: WordSeqCostFn = null,
                                    val syllabify: Boolean = false,
                                    val ignoreTones: Boolean = false) {
  val wordConverter: WordConverter = new WordConverter(syllabify, ignoreTones)

  def apply(source: Seq[Word]): PhoneticWordsMatch = approximate(source)
  def apply(source: Seq[Word], wordSeqCostFn: WordSeqCostFn): PhoneticWordsMatch = approximate(source, wordSeqCostFn)
  def apply(source: Seq[Word], n: Int)(implicit wordSeqCostFn: WordSeqCostFn = defaultWordSeqCostFn): Seq[PhoneticWordsMatch] = approximate(source, n)

  /**
   * Given a sequence of words in a source language, returns the top n phonetic approximation in the
   *   target language
   * Each phonetic approximation is given as a sequence of words with a distance indicating the phonetic match
   *   (higher means closer match)
   */
  def approximate(source: Seq[Word], n: Int)(implicit wordSeqCostFn: WordSeqCostFn = defaultWordSeqCostFn): Seq[PhoneticWordsMatch]
  def approximate(source: Seq[Word], wordSeqCostFn: WordSeqCostFn): PhoneticWordsMatch = approximate(source, 1)(wordSeqCostFn).head
  def approximate(source: Seq[Word]): PhoneticWordsMatch = approximate(source, defaultWordSeqCostFn)

  /**
   * Given a sequence of words in a source language, returns the top n phonetic approximation in the
   *   target language
   * Each phonetic approximation is given as a sequence of a set of words
   *   (the set of words all have the same basic phones) with a distance indicating the phonetic match
   *   (higher means closer match)
   */
  def approximateGrouped(source: Seq[Word], n: Int)(implicit wordSeqCostFn: WordSeqCostFn = defaultWordSeqCostFn): Seq[PhoneticClustersMatch]
    = approximate(source, n).map( p => PhoneticClustersMatch( p.target.map( x => Set(x)), p.alignment, p.score ) )
  def approximateGrouped(source: Seq[Word], wordSeqCostFn: WordSeqCostFn): PhoneticClustersMatch = approximateGrouped(source, 1)(wordSeqCostFn).head
  def approximateGrouped(source: Seq[Word]): PhoneticClustersMatch = approximateGrouped(source, defaultWordSeqCostFn)

  def toPhones(words: Seq[Word]): Seq[Phone] = wordConverter.toPhonesToMatch(words)
  def toPhones(word: Word): Seq[Phone] = wordConverter.toPhonesToMatch(word)
  def toChars(words: Seq[Word]): Seq[Char] = words.map( w => toChars(w)).mkString("")
  def toChars(word: Word): Seq[Char] = word.orthographies.head
  def toSyllables(words: Seq[Word]) = wordConverter.toSyllables(words)

  lazy val wordDelimiterPhones = if (syllabify) Seq(IPA.syllableSep) else Seq()
}

object PhoneticApproximator {
  val names = Set("Greedy", "SimGreedy", "SimSlow", "SimTrie", "ALINE")
  val defaultName = "SimTrie"
  def wordPenaltyFunc(wordPenalty: Double = 0, infreqPenalty: Double = 0): Word => Double =
    w => wordPenalty + infreqPenalty*(1-w.freq)
  def wordSeqCostFunc(wordPenaltyFunc: Word => Double): WordSeqCostFn = new WordSeqCostFn() {
    override def deltaCost(w: Word, prev: Seq[Word], stats: mutable.Map[String, Double] = null) = wordPenaltyFunc(w)
  }
  def wordSeqCostFunc(wordPenalty: Double = 0, infreqPenalty: Double = 0): WordSeqCostFn =
    wordSeqCostFunc( wordPenaltyFunc(wordPenalty, infreqPenalty) )

  def apply(phoneSim: Similarity[Phone], targetWords: Seq[Word],
            phoneApproxName: String = defaultName,
            wordSeqCostFn: WordSeqCostFn,
            initialMatchWeight: Double = 1.0,
            syllabify: Boolean = false, ignoreTones: Boolean = false,
            beamSize: Int = 2, includeAlignments: Boolean = false): PhoneticApproximator = {
    val phoneSeqSim = WeightedLevenshteinSimilarity[Phone](phoneSim)
    val phoneApprox =
      phoneApproxName match {
        case "Greedy" => new GreedyPhoneticApproximator(targetWords)
        case "SimGreedy" => new SimilarityBasedSlowPhoneticApproximator(
          phoneSeqSim, targetWords, wordSeqCostFn, syllabify, ignoreTones, realign = false, beamSize)
        case "SimSlow" => new SimilarityBasedSlowPhoneticApproximator(
          phoneSeqSim, targetWords, wordSeqCostFn, syllabify, ignoreTones, realign = true, beamSize)
        case "SimTrie" => new SimilarityBasedTriePhoneticApproximator(
          phoneSim, targetWords, wordSeqCostFn, initialMatchWeight, syllabify, ignoreTones, beamSize, includeAlignments)
        case "ALINE" => new SimilarityBasedSlowPhoneticApproximator(
          new AlineAlignerSimilarity[Phone], targetWords, wordSeqCostFn, syllabify, ignoreTones, realign = true, beamSize)
      }
    phoneApprox
  }

  def orthographicApproximator(charSim: Similarity[Char] = new ExactMatchSimilarity[Char](),
            targetWords: Seq[Word],
            phoneApproxName: String = defaultName,
            wordSeqCostFn: WordSeqCostFn,
            initialMatchWeight: Double = 1.0,
            syllabify: Boolean = false, ignoreTones: Boolean = false,
            beamSize: Int = 2, includeAlignments: Boolean = false): PhoneticApproximator = {
    //val phoneSeqSim = WeightedLevenshteinSimilarity[Char](phoneSim)
    val phoneApprox =
      phoneApproxName match {
        case "SimTrie" => new SimilarityBasedTrieOrthographicApproximator(
          charSim, targetWords, wordSeqCostFn, initialMatchWeight, syllabify, ignoreTones, beamSize, includeAlignments)
      }
    phoneApprox
  }
}

class GreedyPhoneticApproximator(words: Seq[Word]) extends PhoneticApproximator() {
  val targets = words.filter( toPhones(_).size > 0)
  override def approximate(source: Seq[Word], wordSeqCostFn: WordSeqCostFn): PhoneticWordsMatch = {
    // Initialize with whole word remaining and empty matched sequence
    var remain = toPhones(source)
    var matched = Seq[Word]()
    // While we still have remainder to match
    while (remain.length > 0) {
      // Look for first prefix in candidate list
      val x = targets.find(t => remain.startsWith( toPhones(t) )).getOrElse(NullWord)
      matched = matched :+ x
      remain = remain.drop( toPhones(x).length )
    }
    // TODO: No alignments/score for this
    PhoneticWordsMatch(matched, null, Double.NaN)
  }
  override def approximate(source: Seq[Word], n: Int)(implicit wordSeqCostFn: WordSeqCostFn): Seq[PhoneticWordsMatch] = {
    Seq(approximate(source))
  }
}

class Candidate(val phones: Seq[Phone],                 // phones to match
                val selectedWords: Seq[Word] = Seq(),   // words that we have selected so far
                val nPhonesMatched: Int = 0,            // number of phones matched
                val score: Double = 0                   // score so far (lower is better)
                 ) {
  lazy val matchedPhones = phones.take(nPhonesMatched)
  lazy val matchedAll: Boolean = nPhonesMatched >= phones.size

  def nextPhones(n: Integer) = phones.drop(nPhonesMatched).take(n)
}

class SimilarityBasedSlowPhoneticApproximator(val sim: Similarity[Seq[Phone]],
                                              val targets: Seq[Word],
                                              wordSeqCostFn: WordSeqCostFn = null,
                                              syllabify: Boolean = false,
                                              ignoreTones: Boolean = false,
                                              val realign: Boolean = true,
                                              // Keep this many best matches at each stage
                                              val beamSize: Int = 2) extends PhoneticApproximator(wordSeqCostFn, syllabify, ignoreTones) {
  val candidateComparator = new Comparator[Candidate]() {
    // high valued items are kept in priority queue (return 1 for things we prefer)
    def compare(o1: Candidate, o2: Candidate): Int =
      if (o1.score == o2.score) {
        if (o1.nPhonesMatched == o2.nPhonesMatched) {
          if (o1.selectedWords.size == o2.selectedWords.size) {
            o1.selectedWords.mkString(",").compare(o2.selectedWords.mkString(","))
          }
          else if (o1.selectedWords.size < o2.selectedWords.size) 1 else -1
        } else if (o1.nPhonesMatched > o2.nPhonesMatched) 1 else -1
      }
      else if (o1.score < o2.score) 1 else -1
  }

  // TODO: Support approximateGroup, alignments
  override def approximate(source: Seq[Word], n: Int)(implicit wordSeqCostFn: WordSeqCostFn): Seq[PhoneticWordsMatch] = {
    // Initialize with whole word remaining and empty matched sequence
    val sourcePhones = toPhones(source)
    // Keep grid of best matches of specified length
    val candidates: Array[BoundedTreeSet[Candidate]] =
      Array.fill(sourcePhones.size + 1)(new BoundedTreeSet[Candidate](beamSize, candidateComparator))
    candidates(0).add( new Candidate( sourcePhones ) )
    var checked: Set[Candidate] = Set()
    var fringe: Set[Candidate] = candidates(0).toSet
    var iters = 0
    val filteredTargets = targets.filter( w => toPhones(w).size > 0 && toPhones(w).size <= sourcePhones.size )
    // While we still have remainder to match
    while (!fringe.isEmpty && iters <= sourcePhones.size) {
      for (candidate <- fringe; target <- filteredTargets) {
        val wordDelimSeq = if (candidate.matchedPhones.nonEmpty) wordDelimiterPhones else Seq()
        val phones = wordDelimSeq ++ toPhones(target)
        val nPhonesMatched = candidate.nPhonesMatched + phones.size
        if (nPhonesMatched <= sourcePhones.size) {
          val words = candidate.selectedWords :+ target
          val score = if (realign) {
            val dist = sim.distance(sourcePhones.take(nPhonesMatched), toPhones(words))
            val wordCost = if (wordSeqCostFn != null) wordSeqCostFn.totalCost(words) else 0
            dist + wordCost
          } else {
            val deltaDist = sim.distance(candidate.nextPhones(phones.size), phones)
            val wordCost = if (wordSeqCostFn != null) wordSeqCostFn.deltaCost(target, candidate.selectedWords) else 0
            candidate.score + deltaDist + wordCost
          }
          val c = new Candidate(sourcePhones, words, nPhonesMatched, score)
          candidates(nPhonesMatched).add(c)
        }
      }
      val candidatesSet = candidates.flatten.toSet
      checked = checked ++ fringe
      val newFringe = candidatesSet.diff(checked)
      fringe = newFringe
      iters = iters+1
    }
    if (!fringe.isEmpty) {
      System.err.println("Terminated SimilarityBasedGreedyPhoneticApproximator ... fringe should have stabilized by now")
    }
    val matched: Seq[Candidate] = candidates.last.toSeq.reverse
    // Negate cost to get score (higher better)
    matched.take(n).map( c => PhoneticWordsMatch(c.selectedWords, null, -c.score) )
  }
}

class SimilarityBasedTriePhoneticApproximator(val sim: Similarity[Phone],
                                              val targets: Seq[Word],
                                              wordSeqCostFn: WordSeqCostFn = null,
                                              val initialMatchWeight: Double = 1.0,
                                              syllabify: Boolean = false,
                                              ignoreTones: Boolean = false,
                                              // Keep this many best matches at each stage
                                              val beamSize: Int = 40,
                                              // Include alignments in results
                                              val includeAlignments: Boolean = true,
                                              val customTargetTrie: TrieMap[Phone, Set[Word]] = null)
  extends PhoneticApproximator(wordSeqCostFn, syllabify, ignoreTones)
{

  lazy val targetTrie: TrieMap[Phone, Set[Word]] =
    if (customTargetTrie != null) customTargetTrie else TrieHelper.toPhoneTrie(targets, wordConverter)

  val simWithNull = if (sim.supportsNull) sim else new SimilarityWithNull[Phone](sim)
  val matcher = new TrieMapMatcher[Phone,Set[Word]](targetTrie, wordDelimiterPhones)

  private def costFunction_(implicit wordSeqCostFn: WordSeqCostFn) = new MatchCostFunction[Phone,Set[Word]]() {
    def cost(p1: Phone, p2: Phone, n: Int): Double = {
      val weight = if (n == 0) initialMatchWeight else 1.0
      weight*simWithNull.distance(p1, p2)
    }
    def multiMatchDeltaCost(phones: JList[Phone], values: Set[Word],
                            prevMatched: JList[Match[Phone, Set[Word]]],
                            curMatched: JList[Match[Phone, Set[Word]]]) = {
      if (wordSeqCostFn != null) {
        val prevSeq = if (prevMatched != null) prevMatched.map( x => x.getCustom.asInstanceOf[Word] ) else Seq()
        val costs = values.map( t => (t,wordSeqCostFn.deltaCost(t, prevSeq)) )
        val minPair = costs.minBy( x => x._2 )
        // Remember the minWord that we used
        curMatched.last.setCustom(minPair._1)
        // Return the minimum score
        minPair._2
      } else 0
    }
  }

  override def approximateGrouped(source: Seq[Word], n: Int)(implicit wordSeqCostFn: WordSeqCostFn): Seq[PhoneticClustersMatch] = {
    val costFunction = costFunction_
    val sourcePhones = toPhones(source)
    val sourceSyllables = toSyllables(source)
    val res = matcher.findClosestMatches(sourcePhones, costFunction, Double.MaxValue, beamSize, true, includeAlignments)
    if (res.nonEmpty) {
      val (bad,okay) = res.partition(c => c.getMultimatched == null)
      if (!bad.isEmpty) {
        println("Matching " + source + ": No multimatched: " + bad.mkString("\n"))
      }
      // Negate cost to get score (higher better)
      okay.take(n).map( c => {
        val phoneAlignments = if (includeAlignments) {
          if (c.getAlignments != null) {
            val alignments = c.getAlignments.toIndexedSeq.map( i => if (i != null) (i.getBegin.intValue(), i.getEnd.intValue()) else null )
  //          PhoneticAlignment(sourcePhones, c.getMultimatched.map(  x => x.toSeq ).toSeq, Alignment.fromSourceMatches(alignments))
            val sampleWords = c.getMultivalues.toSeq.map( s => s.head )
            PhoneticAlignment(wordConverter, source, sourceSyllables, sourcePhones,
              sampleWords, toSyllables(sampleWords), c.getMatched,
              //c.getMultimatched.map(  x => x.toSeq ).toSeq,
              Alignment.fromSourceMatches(alignments)
            )
          } else {
            null
          }
        } else { null }
        PhoneticClustersMatch(c.getMultivalues.toSeq, phoneAlignments, -c.getCost)
      } ).toSeq
    } else {
      println("No closest matches for " + source.mkString(","))
      Seq()
    }
  }

  override def approximate(source: Seq[Word], n: Int)(implicit wordSeqCostFn: WordSeqCostFn): Seq[PhoneticWordsMatch] = {
    // Just take first form each group
    // Up to higher level components (i.e. actual transphoner to see what else is in the set and do something with it
    //   (using WordSequenceSelector)
    // TODO: for words in same set, need to account better for infreq penalty
    val sets = approximateGrouped(source, n)
    sets.map( c => PhoneticWordsMatch( c.target.map( s => s.head ), c.alignment, c.score ) )
  }
}

class SimilarityBasedTrieOrthographicApproximator(val sim: Similarity[Char],
                                                  val targets: Seq[Word],
                                                  wordSeqCostFn: WordSeqCostFn = null,
                                                  val initialMatchWeight: Double = 1.0,
                                                  syllabify: Boolean = false,
                                                  ignoreTones: Boolean = false,
                                                  // Keep this many best matches at each stage
                                                  val beamSize: Int = 40,
                                                  // Include alignments in results
                                                  val includeAlignments: Boolean = true,
                                                  val customTargetTrie: TrieMap[Char, Set[Word]] = null)
  extends PhoneticApproximator(wordSeqCostFn, syllabify, ignoreTones)
{

  lazy val targetTrie: TrieMap[Char, Set[Word]] =
    if (customTargetTrie != null) customTargetTrie else TrieHelper.toCharTrie(targets)

  val simWithNull = if (sim.supportsNull) sim else new SimilarityWithNull[Char](sim)
  val matcher = new TrieMapMatcher[Char,Set[Word]](targetTrie)

  private def costFunction_(implicit wordSeqCostFn: WordSeqCostFn) = new MatchCostFunction[Char,Set[Word]]() {
    def cost(p1: Char, p2: Char, n: Int): Double = {
      val weight = if (n == 0) initialMatchWeight else 1.0
      weight*simWithNull.distance(p1, p2)
    }
    def multiMatchDeltaCost(seq: JList[Char], values: Set[Word],
                            prevMatched: JList[Match[Char, Set[Word]]],
                            curMatched: JList[Match[Char, Set[Word]]]) = {
      if (wordSeqCostFn != null) {
        val prevSeq = if (prevMatched != null) prevMatched.map( x => x.getCustom.asInstanceOf[Word] ) else Seq()
        val costs = values.map( t => (t,wordSeqCostFn.deltaCost(t, prevSeq)) )
        val minPair = costs.minBy( x => x._2 )
        // Remember the minWord that we used
        curMatched.last.setCustom(minPair._1)
        // Return the minimum score
        minPair._2
      } else 0
    }
  }

  override def approximateGrouped(source: Seq[Word], n: Int)(implicit wordSeqCostFn: WordSeqCostFn): Seq[PhoneticClustersMatch] = {
    val costFunction = costFunction_
    val sourceMatchSeq: Seq[Char] = source.map( w => w.orthographies.head ).mkString("")
    val res = matcher.findClosestMatches(sourceMatchSeq, costFunction, Double.MaxValue, beamSize, true, includeAlignments)
    if (res.nonEmpty) {
      val (bad,okay) = res.partition(c => c.getMultimatched == null)
      if (!bad.isEmpty) {
        println("Matching " + source + ": No multimatched: " + bad.mkString("\n"))
      }
      // Negate cost to get score (higher better)
      okay.take(n).map( c => {
        PhoneticClustersMatch(c.getMultivalues.toSeq, null, -c.getCost)
      } ).toSeq
    } else {
      println("No closest matches for " + source.mkString(","))
      Seq()
    }
  }

  override def approximate(source: Seq[Word], n: Int)(implicit wordSeqCostFn: WordSeqCostFn): Seq[PhoneticWordsMatch] = {
    // Just take first form each group
    // Up to higher level components (i.e. actual transphoner to see what else is in the set and do something with it
    //   (using WordSequenceSelector)
    // TODO: for words in same set, need to account better for infreq penalty
    val sets = approximateGrouped(source, n)
    sets.map( c => PhoneticWordsMatch( c.target.map( s => s.head ), c.alignment, c.score ) )
  }
}

class PhoneticAlignment(// Alignment from source phones to target phones
                        val phoneAlignment: AlignmentWithSourceTarget[Phone],
                        // Alignment from source syllables to target syllables
                        val syllableAlignment: AlignmentWithSourceTarget[Syllable],
                        // Alignment from source words to target words
                        val wordAlignment: AlignmentWithSourceTarget[Word],
                        // Word level stresses
                        val wordStresses: Seq[StressType.Value])
{
  def syllableStressAlignmentScore(wordConverter: WordConverter, target: Seq[Word]): Double = {
    // Have score for syllable stress alignment
    val targetSyllables = wordConverter.toSyllables(target).flatten
    val aligned = syllableAlignment.alignment.alignedPartial(
      syllableAlignment.source, targetSyllables, Some(Syllable.SyllableStressMatchSimilarityWithOption)
    )
    -aligned.map( x => x._3 ).sum
  }

}

object PhoneticAlignment {
  def apply(wordConverter: WordConverter, sourceWords: Seq[Word], targetWords: Seq[Word],
            phoneAlignments: Alignment): PhoneticAlignment = {
    apply(wordConverter,
      sourceWords, wordConverter.toSyllables(sourceWords), wordConverter.toPhonesToMatch(sourceWords),
      targetWords, wordConverter.toSyllables(targetWords), wordConverter.toPhonesToMatch(targetWords),
      phoneAlignments
    )
  }

  def apply(wordConverter: WordConverter,
            sourceWords: Seq[Word], sourceSyllables: Seq[Seq[Syllable]], sourcePhones: Seq[Phone],
            targetWords: Seq[Word], targetSyllables: Seq[Seq[Syllable]], targetPhones: Seq[Phone],
            phoneAlignment: Alignment): PhoneticAlignment = {
    // map from phone index to word indices and syllable indices
    //  (word index, syllable index (in word), phone index (in syllable), phone)
    // map syllable break to next syllable, phone -1
    case class WordPhoneIndex(wordIndex: Int, syllableIndex: Int, phoneIndex: Int, phone: Phone)
    case class WordSyllableIndex(wordIndex: Int, syllableIndex: Int)
    def getPhoneToWordSyllableIndexMap(syllables: Seq[Seq[Syllable]], phones: Seq[Phone]): IndexedSeq[WordPhoneIndex] = {
      // syllable phone counts: (word number, syllable number (in word), syllable number (all)
      val syllablePhoneIndices = syllables.zipWithIndex.flatMap(
        w => w._1.zipWithIndex.flatMap( s => s._1.phones.zipWithIndex.map(
            p => WordPhoneIndex( w._2, s._2, p._2, p._1 )
          )
        )
      ).toIndexedSeq
      var i = -1
      val allSyllablePhoneIndices = (for (phone <- phones) yield {
        if (phone.isSyllableSep) {
          // take information from next phone if possible
          if (i < syllablePhoneIndices.size-1) {
            val v = syllablePhoneIndices(i+1)
            WordPhoneIndex( v.wordIndex, v.syllableIndex, -1, phone )
          } else {
            syllablePhoneIndices(i)
          }
        } else {
          i = i + 1
          if (phone.getBasePhone != syllablePhoneIndices(i).phone.getBasePhone) {
            println("Hey phones don't match...")
          }
          syllablePhoneIndices(i)
        }
      }).toIndexedSeq
      allSyllablePhoneIndices
    }
    // Map of word/syllable indices to overall syllable index
    def getSyllableIndexMap( phoneIndices: IndexedSeq[WordPhoneIndex] ): Map[WordSyllableIndex, Int] = {
      phoneIndices.map( x => WordSyllableIndex(x.wordIndex, x.syllableIndex) ).distinct.zipWithIndex.toMap
    }

    // Get mapping of overall phone index to word/syllable/phone index
    val sourceIndices = getPhoneToWordSyllableIndexMap(sourceSyllables, sourcePhones)
    val targetIndices = getPhoneToWordSyllableIndexMap(targetSyllables, targetPhones)

    // Get mapping of (word index, syllable index) to (overall syllable index)
    val sourceSyllIndices = getSyllableIndexMap(sourceIndices)
    val targetSyllIndices = getSyllableIndexMap(targetIndices)
    val targetSyllInvIndices = targetSyllIndices.map( p => p._2 -> p._1 )

    val sourcePhoneSyllIndicies = sourceIndices.map( x => sourceSyllIndices( WordSyllableIndex(x.wordIndex, x.syllableIndex)) )
    val targetPhoneSyllIndicies = targetIndices.map( x => targetSyllIndices( WordSyllableIndex(x.wordIndex, x.syllableIndex)) )

    // Compute word alignments
    val wordAlignmentIndices = phoneAlignment.pairs().map(
      x => (if (x._1 == -1) -1 else sourceIndices(x._1).wordIndex,
            if (x._2 == -1) -1 else targetIndices(x._2).wordIndex)
    ).distinct

    // Compute syllable alignments
    val syllableAlignmentIndices = phoneAlignment.pairs().map(
      x => (if (x._1 == -1) -1 else sourcePhoneSyllIndicies(x._1),
            if (x._2 == -1) -1 else targetPhoneSyllIndicies(x._2))
    ).distinct
    // Flatten syllables
    val sourceSyllablesFlat = sourceSyllables.flatten
    val targetSyllablesFlat = targetSyllables.flatten
    val syllableAlignments = Alignment.fromPairedMatches(syllableAlignmentIndices)

    // Do Stress transfer
    val syllableAlignmentsByTarget = syllableAlignments.spansByTarget()
    val targetSyllablesWithSourceStresses = targetSyllablesFlat.zipWithIndex.map( p => {
      val sourceSpan = syllableAlignmentsByTarget.get(p._2)
      val stressType = if (sourceSpan.isDefined) {
        val s = sourceSyllablesFlat.slice(sourceSpan.get._1, sourceSpan.get._2)
        wordConverter.syllablesToStressType(s)
      } else StressType.none
      Syllable( p._1.phones, stressType, p._1.tones )
    })
    val wordStresses = targetSyllablesWithSourceStresses.zipWithIndex
      .map( p => (targetSyllInvIndices(p._2).wordIndex, p._1) )
      .groupBy( p => p._1 )
      .mapValues( x => wordConverter.syllablesToStressType( x.map( y => y._2) ) )
      .toIndexedSeq.sorted.map( x => x._2 )

    new PhoneticAlignment(
      AlignmentWithSourceTarget( sourcePhones, targetPhones, phoneAlignment),
      AlignmentWithSourceTarget( sourceSyllablesFlat, targetSyllablesWithSourceStresses, syllableAlignments),
      AlignmentWithSourceTarget( sourceWords, targetWords, Alignment.fromPairedMatches(wordAlignmentIndices)),
      wordStresses
    )
  }

}


class PhoneticMatch[T](target: Seq[T],
                       alignment:  PhoneticAlignment,
                       score: Double) {
  def toPair = (target, score)
}

case class PhoneticWordsMatch(target: Seq[Word], alignment:  PhoneticAlignment, score: Double)
  extends PhoneticMatch[Word](target, alignment, score)
case class PhoneticClustersMatch(target: Seq[Set[Word]], alignment: PhoneticAlignment, score: Double)
  extends PhoneticMatch[Set[Word]](target, alignment, score)

trait WordSeqCostFn {
  import scala.collection.mutable
  def deltaCost(w: Word, prevSeq: Seq[Word], stats: mutable.Map[String, Double] = null): Double = 0
  def totalCost(seq: Seq[Word], stats: mutable.Map[String, Double] = null): Double = {
    var prev: Seq[Word] = Seq()
    var sum: Double = 0
    for (w <- seq) {
      sum = sum + deltaCost(w, prev, stats)
      prev = prev :+ w
    }
    sum
  }
  // Internal helper function
  protected def addStat(name: String, value: Double, stats: mutable.Map[String, Double]): Double = {
    if (stats != null) {
      stats.update(name, stats.getOrElse(name, 0.0) + value)
    }
    value
  }
}