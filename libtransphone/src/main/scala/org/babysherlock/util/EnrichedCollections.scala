package org.babysherlock.util

import scala.collection.generic.CanBuildFrom

/**
 * Implicit conversions to enriched versions of scala collections with useful functions
 * @author Manolis Savva
 */
object EnrichedCollections {

  class GroupingCollection[A, C, D[C]]
  (ca: C)(implicit c2i: C => Iterable[A],cbf: CanBuildFrom[C,C,D[C]],cbfi: CanBuildFrom[C,A,C]) {

    // TODO: Update to https://github.com/scala/scala/commit/f40e521b8f20bf8285b2f6871554a2bc637fe328
    // TODO: and fix empty list problem
    def splitOn(p: A => Boolean): D[C] = {
      val it = c2i(ca).iterator
      val cca = cbf()
      if (!it.hasNext) cca.result()
      else {
        cca.clear()
        val as = cbfi()
        while (it.hasNext) {
          val a = it.next()
          if (p(a)) as += a
          else { cca += as.result; as.clear(); as += a }
        }
        cca += as.result
      }
      cca.result()
    }
  }

  implicit def collections_have_grouping[A, C[A]](ca: C[A])(
    implicit c2i: C[A] => Iterable[A],
    cbf: CanBuildFrom[C[A],C[A],C[C[A]]],
    cbfi: CanBuildFrom[C[A],A,C[A]]
    ) = {
    new GroupingCollection[A,C[A],C](ca)(c2i, cbf, cbfi)
  }

  val vector_string_builder = (
    new CanBuildFrom[String, String, Vector[String]] {
      def apply() = Vector.newBuilder[String]
      def apply(from: String) = this.apply()
    }
  )

  implicit def strings_have_grouping(s: String)(
    implicit c2i: String => Iterable[Char],
    cbfi: CanBuildFrom[String,Char,String]
    ) = {
    new GroupingCollection[Char,String,Vector](s)(
      c2i, vector_string_builder, cbfi
    )
  }


  // Heterogeneous type cross products
  // http://stackoverflow.com/questions/16219545/scala-cross-cartesian-product-with-multiple-sources-and-heterogeneous-types?lq=1

  trait Crosser[A,B,C] {
    def cross( as: Traversable[A], bs: Traversable[B] ): Traversable[C]
  }

  trait LowPriorityCrosserImplicits {
    private type T[X] = Traversable[X]

    implicit def crosser2[A,B] = new Crosser[A,B,(A,B)] {
      def cross( as: T[A], bs: T[B] ): T[(A,B)] = for { a <- as; b <- bs } yield (a, b)
    }
  }

  object Crosser extends LowPriorityCrosserImplicits {
    private type T[X] = Traversable[X]

    implicit def crosser3[A,B,C] = new Crosser[(A,B),C,(A,B,C)] {
      def cross( abs: T[(A,B)], cs: T[C] ): T[(A,B,C)] = for { (a,b) <- abs; c <- cs } yield (a, b, c)
    }

    implicit def crosser4[A,B,C,D] = new Crosser[(A,B,C),D,(A,B,C,D)] {
      def cross( abcs: T[(A,B,C)], ds: T[D] ): T[(A,B,C,D)] = for { (a,b,c) <- abcs; d <- ds } yield (a, b, c, d)
    }
  }

  implicit class Crossable[A](xs: Traversable[A]) {
    def cross[B,C](ys: Traversable[B])(implicit crosser: Crosser[A,B,C]): Traversable[C] = crosser.cross( xs, ys )
  }

  def split[T](seq: Seq[T], p: T => Boolean): Seq[Seq[T]] = {
    seq.foldLeft(Seq(Seq.empty[T])) {
      (acc, t) =>
        if (p(t)) acc :+ Seq.empty
        else acc.init :+ (acc.last :+ t)
    }
  }

}
