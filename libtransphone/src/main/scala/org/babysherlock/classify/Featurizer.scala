package org.babysherlock.classify

import edu.stanford.nlp.stats.Counter
import scala.collection.JavaConversions._

/**
 * Simple featurizer interface
 * @author Angel Chang
 */
trait Featurizer[E] {
  def featurize(features: Set[String])(item: E): Map[String, Double] = {
    import scala.collection.mutable
    val validFeatures = featureMap.keySet.intersect(features)
    val featureCounts = new mutable.HashMap[String,Double]
    for (feature <- validFeatures) {
      val evaluator = featureMap(feature)
      val v = evaluator(item)
      v match {
        case n:Number => featureCounts += (feature -> n.doubleValue())
        case s:Iterable[_] => s.foreach (
          x => featureCounts += (x match {
            case (f:String,score:Double) => feature + "-" + f -> score
            case _ => feature + "-" + x.toString -> 1.0
          })
        )
        case c:Counter[_] => for (f <- c.keySet()) {
          val v = c.getCount(f)
          featureCounts += feature + "-" + f -> v
        }
        case _ => featureCounts += (feature + "-" + v.toString -> 1.0)
      }
    }
    featureCounts.toMap
  }

  def featureValuesAsStrings(features: Seq[String])(item: E): Seq[String] = {
    for (feature <- features) yield {
      featureMap.get(feature).map( x => x(item).toString() ).getOrElse("")
    }
  }

  def featuresCountsToString(featureCounts: Map[String, Double]): String = {
    featureCounts.mkString(",")
  }

  def featureMap: Map[String, (E) => Any]
}
