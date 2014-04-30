package org.babysherlock.util

import scala.util.Random
import java.util.PriorityQueue
import scala.collection.JavaConversions._
import java.util

/**
 * Given set of elements, samples n elements
 *
 * @author Angel Chang
 */
class BatchSampler {
  def sampleOne[E](samples: IndexedSeq[E], rand: Random = Random): Option[E] = {
    val total = samples.size
    if (total > 0) {
      val i = rand.nextInt(total)
      Some(samples(i))
    } else {
      None
    }
  }

  def sampleOneWeighted[E](samples: Iterable[(E,Double)], rand: Random = Random): Option[(E,Double)] = {
    val iter = sampleWeightedStreamWithoutReplacement(samples, 1, rand)
    iter.headOption
  }

  def sampleIndices(sampleSize: Int, totalSize: Int, shuffleOrder: Boolean = true, rand: Random = Random): IndexedSeq[Int] = {
    if (sampleSize >= totalSize) {
      val samples = (0 until totalSize).toIndexedSeq
      if (shuffleOrder) rand.shuffle(samples)
      else samples
    } else {
      val samples = Array.ofDim[Int](sampleSize)
      var t = 0  // # of total processed
      var m = 0  // # of samples selected
      while (m < sampleSize && t < totalSize) {
        val r = rand.nextFloat()
        if ( (totalSize - t)*r < sampleSize - m) {
          samples(m) = t
          m = m+1
        }
        t = t+1
      }
      if (shuffleOrder)
        rand.shuffle(samples.toIndexedSeq)
      else samples.toIndexedSeq
    }
  }

  def sampleWithoutReplacement[E](allSamples: Iterable[E], nSamples: Int, randOrder: Boolean = true, rand: Random = Random): IndexedSeq[E] = {
    val indexed = allSamples.toIndexedSeq
    if (nSamples < 10 || nSamples < indexed.size/2) {
      val indices = sampleIndices(nSamples, indexed.size, randOrder)
      indices.map( x => indexed(x) )
    } else {
      val permutedList = rand.shuffle(indexed)
      permutedList.slice(0, nSamples)
    }
  }

  def sampleWithReplacement[E](allSamples: Iterable[E], nSamples: Int, rand: Random = Random): IndexedSeq[E] = {
    val ordered = allSamples.toIndexedSeq
    for (i <- 0 until nSamples) yield {
      val r = rand.nextInt(ordered.size)
      ordered(r)
    }
  }

  protected def sampleWeightedAndRemove[E](weightedSamples: Iterable[(E, Double)], nSamples: Int,
                                        totalWeight: Double, rand: Random): Iterable[(E, Double)] = {
    def sampleWeightedAndRemove(weightedSamples: Iterable[(E, Double)], totalWeight: Double, rand: Random):
      ((E, Double), Iterable[(E,Double)]) =
    {
      val r: Double = rand.nextDouble * totalWeight
      var total: Double = 0.0
      var sample: (E, Double) = null
      val remaining = weightedSamples.dropWhile(s => { sample = s; total += s._2; total < r} )
      (sample, remaining)
    }

    def sampleWeightedAndRemove0(accum: Stream[(E, Double)], weightedSamples: Iterable[(E, Double)], nSamples: Int,
                                 totalWeight: Double, rand: Random): Iterable[(E, Double)] = {
      if (weightedSamples.isEmpty || nSamples <= 0) accum
      else {
        val (sampled, remaining) = sampleWeightedAndRemove(weightedSamples, totalWeight, rand)
        sampleWeightedAndRemove0(sampled #:: accum, remaining, nSamples-1, totalWeight - sampled._2, rand)
      }
    }

    if (nSamples > weightedSamples.size) weightedSamples
    else sampleWeightedAndRemove0(Stream(), weightedSamples, nSamples, totalWeight, rand)
  }

  def sampleWeightedWithoutReplacement[E](allSamples: Iterable[(E, Double)], nSamples: Int, rand: Random = Random): Iterable[(E, Double)] = {
    val totalWeight = allSamples.map( s => s._2 ).sum
    sampleWeightedAndRemove(allSamples, nSamples, totalWeight, rand)
  }

  /**
   * Samples a stream uniformly (potential very large) without replacement with one pass
   * Samples are stored and assumed to be relatively small with respect to the number of entries in the stream
   * @param allSamples weighted samples
   * @param nSamples number of samples to draw
   * @param rand random number generator
   * @tparam E
   * @return samples
   */
  def sampleStreamWithoutReplacement[E](allSamples: Iterable[E], nSamples: Int, rand: Random = Random): Iterable[E] = {
    val selected = scala.collection.mutable.ArrayBuffer[E]()
    var n: Int = 0
    for (sample <- allSamples) {
      n += 1
      if (selected.size >= nSamples) {
        val i: Int = rand.nextInt(n)
        if (i < nSamples) {
          selected.updated(i, sample)
        }
      }
      else {
        selected.append(sample)
      }
    }
    selected.toIterable
  }

  /**
   * Samples a weighted stream (potential very large) without replacement with one pass
   * Samples are stored and assumed to be relatively small with respect to the number of entries in the stream
   * @param allSamples weighted samples
   * @param nSamples number of samples to draw
   * @param rand random number generator
   * @tparam E
   * @return samples
   */
  def sampleWeightedStreamWithoutReplacement[E](allSamples: Iterable[(E, Double)], nSamples: Int, rand: Random = new Random): Iterable[(E, Double)] = {
    val ordering: Ordering[(Double, (E, Double))] = Ordering[Double].on( x => -x._1 )
    val selected = new PriorityQueue[(Double, (E, Double))](nSamples, ordering)
    var n: Int = 0
    for (sample <- allSamples) {
      n += 1
      if (sample._2 > 0.0) {
        val r: Double = rand.nextDouble()
        val k: Double = 1.0 / sample._2 * math.log(r)
        if (selected.size >= nSamples) {
          var swap: Boolean = k > selected.peek._1
          if (k == selected.peek._1) {
            swap = rand.nextDouble > 0.5
          }
          if (swap) {
            selected.remove()
            selected.add((k, sample))
          }
        }
        else {
          selected.add((k, sample))
        }
      }
    }
    selected.map( x => x._2 )
  }

/*  def getDistributionWeights(distribution: Distribution[E]): Iterable[(E, Double)] = {
    for (key <- JavaConversions.asScalaSet(distribution.keySet()))
      yield (key, distribution.getCount(key))
  }

  def sampleWeightedWithoutReplacement(distribution: Distribution[E], nSamples: Int, rand: Random = Random): Iterable[(E, Double)] = {
    val weightedSamples = getDistributionWeights(distribution)
    return sampleWeightedAndRemove(weightedSamples, nSamples, 1, rand)
  }

  def sampleWeightedStreamWithoutReplacement(distribution: Distribution[E], nSamples: Int, rand: Random = new Random): Iterable[(E, Double)] = {
    return sampleWeightedStreamWithoutReplacement(getDistributionWeights(distribution), nSamples, rand)
  }                  */

}
