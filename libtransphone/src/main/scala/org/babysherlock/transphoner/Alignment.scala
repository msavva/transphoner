package org.babysherlock.transphoner

/**
 * Alignment information
 * @author Angel Chang
 */
trait Alignment {
  // Paired offsets
  def pairs(): IndexedSeq[(Int,Int)]
  // Alignments from source to target
  def spansBySource(): Map[Int, (Int,Int)] = pairs().groupBy(_._1).mapValues( v => v.map( x => x._2 ) ).mapValues( v => (v.min, v.max+1) )
  // Alignments from target to source
  def spansByTarget(): Map[Int, (Int,Int)] = pairs().groupBy(_._2).mapValues( v => v.map( x => x._1 ) ).mapValues( v => (v.min, v.max+1) )

  def aligned[T](source: Seq[T], target: Seq[T], scorer: Option[Similarity[Option[T]]] = None): Seq[(Option[T],Option[T], Double)] = {
    // Create aligned sequence of source and target entries
    val s = source.toIndexedSeq
    val t = target.toIndexedSeq
    var last = (-1,-1)
    pairs.map( p => {
      val a: Option[T] = if (p._1 != last._1) Some(s(p._1)) else None
      val b: Option[T] = if (p._2 != last._2) Some(t(p._2)) else None
      last = p
      val score = if (scorer.isDefined) scorer.get.distance(a,b) else Double.NaN
      (a,b,score)
    })
  }
  def alignedPartial[T](source: Seq[T], target: Seq[T], scorer: Option[SimilarityWithOption[T]] = None): Seq[(Option[T],Option[T], Double)] = {
    // Create aligned sequence of source and target entries
    val s = source.toIndexedSeq
    val t = target.toIndexedSeq
    var last = (-1,-1)
    pairs.filter(p => p._1 < s.length && p._2 < t.length).map( p => {
      val a: Option[T] = if (p._1 != last._1) Some(s(p._1)) else None
      val b: Option[T] = if (p._2 != last._2) Some(t(p._2)) else None
      last = p
      val score = if (scorer.isDefined) scorer.get.distance(a,b) else Double.NaN
      (a,b,score)
    })
  }
  def mkStringSimple[T](source: Seq[T], target: Seq[T], scorer: Option[Similarity[Option[T]]] = None, delimiter: String = ","): String = {
    // Create string showing alignment between source and target (for debugging)
    val a = aligned(source, target, scorer)
    mkStringSimple(a, delimiter)
  }
  def mkStringSimple[T](a: Seq[(Option[T],Option[T],Double)], delimiter: String = ","): String = {
    // Create string showing alignment between source and target (for debugging)
    val hasScore = a.exists( !_._3.isNaN )
    if (hasScore) {
      a.map( p => (p._1.getOrElse(""), p._2.getOrElse("")) ).mkString(delimiter)
    } else {
      a.map( p => ( p._1.getOrElse(""), p._2.getOrElse(""), if (p._3.isNaN) "" else p._3.formatted("%0.5f"))).mkString(delimiter)
    }
  }
  def mkString[T](source: Seq[T], target: Seq[T], scorer: Option[Similarity[Option[T]]] = None, prefix: String = "", delimiter: String = " | ", numFormat: Option[String] = None): String = {
    // Create string showing alignment between source and target (for debugging)
    val a = aligned(source, target, scorer)
    mkString(a, prefix, delimiter, numFormat)
  }
  def mkString[T](a: Seq[(Option[T],Option[T],Double)], prefix: String = "", delimiter: String, numFormat: Option[String]): String = {
    // Create string showing alignment between source and target (for debugging)
    val hasScore = a.exists( !_._3.isNaN )
    val alignedAsStrings = a.map( p => (p._1.getOrElse("").toString, p._2.getOrElse("").toString, if (p._3.isNaN) "" else numFormat.getOrElse("%.1f").format(p._3)) )
    val paddedAligned = alignedAsStrings.map( p => {
      var len = math.max(p._1.length, p._2.length)
      if (hasScore) len = math.max(len, p._3.length)
      (p._1.padTo(len, ' '), p._2.padTo(len, ' '), p._3.padTo(len, ' '))
    })
    val r1 = prefix + paddedAligned.map( _._1 ).mkString(delimiter)
    val r2 = prefix + paddedAligned.map( _._2 ).mkString(delimiter)
    if (hasScore) {
      val r3 = prefix + paddedAligned.map( _._3 ).mkString(delimiter)
      r1 + "\n" + r2 + "\n" + r3
    } else {
      r1 + "\n" + r2
    }
  }
}

case class AlignmentWithSourceTarget[T](source: Seq[T],
                                        target: Seq[T],
                                        alignment: Alignment) {
  def aligned(scorer: Option[Similarity[Option[T]]] = None) = alignment.aligned(source, target, scorer)
  def mkStringSimple(scorer: Option[Similarity[Option[T]]] = None, delimiter: String = ",") = alignment.mkStringSimple(source, target, scorer, delimiter)
  def mkString(scorer: Option[Similarity[Option[T]]] = None, prefix: String = "", delimiter: String = " | ") = alignment.mkString(source, target, scorer, prefix, delimiter)
}

object Alignment {
  def fromPairedMatches(matches: IndexedSeq[(Int,Int)]) = AlignmentByPairedMatched(matches.sorted)
  def fromSourceMatches(matches: IndexedSeq[(Int,Int)]) = AlignmentBySource(matches)
  def fromTargetMatches(matches: IndexedSeq[(Int,Int)]) = AlignmentByTarget(matches)

  case class AlignmentByPairedMatched(matches: IndexedSeq[(Int,Int)]) extends Alignment {
    // Paired offsets
    override def pairs(): IndexedSeq[(Int,Int)] = matches
  }
  case class AlignmentBySource(matches: IndexedSeq[(Int,Int)]) extends Alignment {
    // Paired offsets
    override def pairs(): IndexedSeq[(Int,Int)] = spansBySource().toIndexedSeq.flatMap(
      p => Range(p._2._1, p._2._2).map( x => (p._1, x) )
    ).sorted
    // Alignments from source to target
    override def spansBySource(): Map[Int, (Int,Int)] = matches.zipWithIndex.filter( p => p._1 != null).map( p => p._2 -> p._1 ).toMap
  }
  case class AlignmentByTarget(matches: IndexedSeq[(Int,Int)]) extends Alignment {
    // Paired offsets
    override def pairs(): IndexedSeq[(Int,Int)] = spansByTarget().toIndexedSeq.flatMap(
      p => Range(p._2._1, p._2._2).map( x => (x, p._1) )
    ).sorted
    // Alignments from source to target
    override def spansByTarget(): Map[Int, (Int,Int)] = matches.zipWithIndex.filter( p => p._1 != null).map( p => p._2 -> p._1 ).toMap

  }

}

