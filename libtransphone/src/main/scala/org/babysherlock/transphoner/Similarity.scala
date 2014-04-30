package org.babysherlock.transphoner

import org.babysherlock.util.{BoundedPriorityQueue, LRUCache}

/**
 * Similarity class for transphoner
 * @author Angel Chang
 */
trait Similarity[T] {
  def apply(t1: T, t2: T) = distance(t1,t2)
  def distance(t1: T, t2: T): Double
  def similarity(t1: T, t2: T): Double = 1/distance(t1,t2)

  // What's the maximum distance this similarity metric is expected to have
  def maxDistance: Double = Double.PositiveInfinity
  def supportsNull: Boolean = false

  val ordering: Ordering[(T, Double)] = Ordering[Double].on( x => -x._2 )
  private def topCandidates(w: T, candidates: Set[T], n: Int, fn: (T,T) => Double): Seq[(T,Double)] = {
    val queue = new BoundedPriorityQueue[(T,Double)]( n )(ordering)
    for (c <- candidates) {
      queue += ((c, fn(w,c)))
    }
    queue.dequeueAll.toSeq.reverse
  }
  def mostSimilar(w: T, candidates: Set[T], n: Int, useSim: Boolean = false): Seq[(T,Double)] = {
    if (useSim) {
      topCandidates(w, candidates, n, similarity)
    } else {
      topCandidates(w, candidates, n, (t1,t2) => -distance(t1,t2)).map( x => (x._1, -x._2) )
    }
  }
  def mostDifferent(w: T, candidates: Set[T], n: Int, useSim: Boolean = false): Seq[(T,Double)] = {
    if (useSim) {
      topCandidates(w, candidates, n, (t1,t2) => -similarity(t1,t2)).map( x => (x._1, -x._2) )
    } else {
      topCandidates(w, candidates, n, distance )
    }
  }

}

class ConstantSimilarity[T](val dist: Double, val sim: Double) extends Similarity[T] {
  override def distance(t1: T, t2: T): Double = dist
  override def similarity(t1: T, t2: T): Double = sim

  override def maxDistance: Double = dist
  override def supportsNull: Boolean = true
}

class SimilarityWithCache[T](val baseSim: Similarity[T], val cacheSize: Int = 10000) extends Similarity[T] {
  private val simcache = LRUCache[(T,T),Double](cacheSize)
  private val distcache = LRUCache[(T,T),Double](cacheSize)

  override def distance(t1: T, t2: T): Double = distcache.getOrElse((t1,t2))(baseSim.distance(t1,t2))
  override def similarity(t1: T, t2: T): Double = simcache.getOrElse((t1,t2))(baseSim.similarity(t1,t2))

  override def maxDistance: Double = baseSim.maxDistance
  override def supportsNull: Boolean = baseSim.supportsNull
}

class SimilarityWithOption[T](val baseSim: Similarity[T],
                              val defaultCost: Double = Double.NaN) extends Similarity[Option[T]] {
  private val cost: Double = if (defaultCost.isNaN) baseSim.maxDistance else defaultCost
  def distance(t1: Option[T], t2: Option[T]): Double = {
    if (baseSim.supportsNull) baseSim.distance(t1.getOrElse(null.asInstanceOf[T]), t2.getOrElse(null.asInstanceOf[T]))
    else if (t1.isDefined && t2.isDefined) {
      baseSim.distance(t1.get, t2.get)
    } else {
      if (!t1.isDefined && !t2.isDefined) 0 else cost
    }
  }
}

class SimilarityWithNull[T](val baseSim: Similarity[T],
                            val defaultCost: Double = Double.NaN) extends Similarity[T] {
  private val cost: Double = if (defaultCost.isNaN) baseSim.maxDistance else defaultCost
  def distance(t1: T, t2: T): Double = {
    if (t1 != null && t2 != null) {
      baseSim.distance(t1, t2)
    } else {
      if (t1 == t2) 0 else cost
    }
  }
  override def supportsNull: Boolean = true
}

class InterpolatedSimilarity[T](val weightedSims: Iterable[(Similarity[T], Double)]) extends Similarity[T] {
  def this(sim: Similarity[T], weight: Double) = this(Seq((sim, weight)))
  override def distance(o1: T, o2: T) = {
    weightedSims.map( sim => sim._1.distance(o1,o2) * sim._2 ).sum
  }
  override def supportsNull: Boolean = {
    weightedSims.forall( sim => sim._1.supportsNull )
  }
  override def maxDistance: Double = {
    weightedSims.map( sim => sim._1.maxDistance * sim._2 ).sum
  }
}

// If two things are the same, the distance is 0 otherwise 1
class ExactMatchSimilarity[T] extends Similarity[T] {
  def distance(t1: T, t2: T): Double = if (t1.equals(t2)) 0 else 1
  override def maxDistance: Double = 1
}
class ExactMatchSimilarityWithOption[T]() extends SimilarityWithOption[T](new ExactMatchSimilarity[T]) {
}

class JaccardSimilarity[T] extends Similarity[Set[T]] {
  override def similarity(t1: Set[T], t2: Set[T]): Double = {
    val intersect = t1.intersect(t2)
    val union = t1.union(t2)
    if (union.size == 0) 0 else intersect.size.toDouble/union.size.toDouble
  }
  override def distance(t1: Set[T], t2: Set[T]): Double = 1 - similarity(t1,t2)
}

// Similarity that uses a map indicating the cost of replacing t1 with t2
class MapBasedSimilarity[T](map: Map[(T,T), Double], defaultCost: Double = 1) extends Similarity[T] {
  private lazy val maxMappedValue = if (map.isEmpty) 0 else map.values.max
  def distance(t1: T, t2: T): Double = map.getOrElse((t1,t2), if (t1.equals(t2)) 0 else defaultCost)
  override def maxDistance: Double = math.max(defaultCost, maxMappedValue)
}

// Similarity that uses a map indicating the cost of replacing t1 with t2
// Lookup reverse if not found
class MapBasedSimilarityWithReverseLookup[T](map: Map[(T,T), Double], defaultCost: Double = 1) extends Similarity[T] {
  private lazy val maxMappedValue = if (map.isEmpty) 0 else map.values.max
  def distance(t1: T, t2: T): Double =
    map.getOrElse((t1,t2), map.getOrElse((t2,t1), if (t1.equals(t2)) 0 else defaultCost))
  override def maxDistance: Double = math.max(defaultCost, maxMappedValue)
}

class WeightedUnorderedAlignSimilarity[T](val sim: Similarity[T]) extends Similarity[Seq[(T,Double)]] {
  private def weightedAve(ts1: Seq[(T,Double)], ts2: Seq[(T,Double)],
                          simfn: (T,T) => Double,
                          aggrfn: (Double,Double) => Double): Double = {
    // Take similarity between each pair, and for each select the other item that gives the largest similarity (or smallest diff)
    val sims = Array.ofDim[Double](ts1.size, ts2.size)
    for ((t1,i) <- ts1.zipWithIndex; (t2,j) <- ts2.zipWithIndex) {
      sims(i)(j) = simfn(t1._1, t2._1)
    }
    val tm1 = sims.map( x => x.reduce( (a,b) => aggrfn(a,b) ) )
      .zip( ts1 )
      .map( x => x._1*x._2._2 )
    val tm1ws = ts1.map( x => x._2 ).sum
    val tm2 = sims.reduce( (a1,a2) => a1.zip(a2).map( x => aggrfn(x._1, x._2) ) )
      .zip( ts2 )
      .map( x => x._1*x._2._2 )
    val tm2ws = ts2.map( x => x._2 ).sum
    val overallSim = (tm1.sum + tm2.sum)/(tm1ws + tm2ws)
    overallSim
  }

  override def similarity(ts1: Seq[(T,Double)], ts2: Seq[(T,Double)]): Double = {
    // Take similarity between each pair, and for each select the other item that gives the largest similarity
    weightedAve(ts1, ts2, sim.similarity, math.max)
  }

  override def distance(ts1: Seq[(T,Double)], ts2: Seq[(T,Double)]): Double = {
    // Take distance between each pair, and for each select the other item that gives the minimum distance
    weightedAve(ts1, ts2, sim.distance, math.min)
  }
}

// Compute Levenshtein similarity between two words where, the cost of insertion depends on the substitution made
// assumes that sim.distance supports comparing against null (no element)
class WeightedLevenshteinSimilarity[T](val sim: Similarity[Option[T]] = new ExactMatchSimilarityWithOption[T]) extends Similarity[Seq[T]] {
  override def similarity(t1: Seq[T], t2: Seq[T]): Double = {
    val diff = distance(t1,t2)
    val maxlen = math.max(t1.size, t2.size)
    (maxlen - diff)/maxlen
  }
  override def distance(t1: Seq[T], t2: Seq[T]): Double = _distance(t1,t2, align = false)._1
  def align(t1: Seq[T], t2: Seq[T]): (Double, Alignment) = _distance(t1,t2, align = true)

  private def _distance(t1: Seq[T], t2: Seq[T], align: Boolean = false): (Double, Alignment) = {
    val m = t1.size
    val n = t2.size
    val none: Option[T] = None
    // for all i and j, d[i,j] will hold the Levenshtein distance between
    // the first i elements of t1 and the first j characters of t2;
    val d: Array[Array[Double]] = Array.ofDim(m+1, n+1)
    // d is initialized to 0
    // backpointers for alignment
    val backpointers: Array[Array[(Int,Int)]] = if (align) Array.ofDim(m+1, n+1) else null
    // initialize base case where one of the strings is empty
    for (i <- 1 to m) {
      d(i)(0) = d(i-1)(0) + sim.distance(Option(t1(i-1)),none)
      if (align) backpointers(i)(0) = (i-1,0)
    }
    for (j <- 1 to n) {
      d(0)(j) = d(0)(j-1) + sim.distance(none,Option(t2(j-1)))
      if (align) backpointers(0)(j) = (0,j-1)
    }
    for ( i <- 1 to m; j <- 1 to n) {
      val a = Option(t1(i-1))
      val b = Option(t2(j-1))
      val dists = Array(
        d(i-1)(j) + sim.distance(a,none), // deletion
        d(i)(j-1) + sim.distance(none,b), // insertion
        d(i-1)(j-1) + sim.distance(a,b)   // substitution
      )
      d(i)(j) = dists.min
      if (align) {
        val selected = dists.indexOf(d(i)(j))
        backpointers(i)(j) = selected match {
          case 0 => (i-1,j) // deletion
          case 1 => (i,j-1) // insertion
          case 2 => (i-1,j-1) // substitution
        }
      }
    }
    val alignments = if (align) {
      val offsets = Array.ofDim[(Int, Int)](math.max(m,n))
      // Trace back and figure out alignment
      var pos = (m,n)
      for (k <- offsets.length-1 to 0 by -1) {
        offsets(k) = (pos._1-1, pos._2-1)
        pos = backpointers(pos._1)(pos._2)
      }
      Alignment.fromPairedMatches(offsets)
    } else null
    (d(m)(n), alignments)
  }
}

object WeightedLevenshteinSimilarity {
  def apply[T](baseSim: Similarity[T] = new ExactMatchSimilarity[T]): WeightedLevenshteinSimilarity[T] =
    new WeightedLevenshteinSimilarity[T](new SimilarityWithOption[T](baseSim))
}

