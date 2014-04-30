package org.babysherlock.transphoner

import org.babysherlock.transphoner.dict.Word

/**
 * Word similarities
 * @author Angel Chang
 */
object WordSimilarity {

  val orthographicSimilarity = new OrthographicSimilarity()

  class OrthographicSimilarity(val charSim: Similarity[Option[Char]] = new ExactMatchSimilarityWithOption[Char],
                               val ignoreCase: Boolean = true) extends Similarity[Word] {
    val levSim = new WeightedLevenshteinSimilarity[Char](charSim)
    def toChars(w: Word): String = if (ignoreCase) w.orthographies.head.toLowerCase else w.orthographies.head
    def toChars(ws: Seq[Word]): String = ws.map( w => toChars(w) ).mkString(" ")
    def distance(w1: Word, w2: Word): Double = {
      levSim(toChars(w1), toChars(w2))
    }
    def distanceWords(w1: Seq[Word], w2: Seq[Word]): Double = {
      levSim(toChars(w1), toChars(w2))
    }
    def distance(w1: Seq[Char], w2: Seq[Char]): Double = {
      levSim(w1, w2)
    }
    def distance(w1: String, w2: String): Double = {
      levSim(w1, w2)
    }
  }


  // TODO: add word phone similarities here
}
