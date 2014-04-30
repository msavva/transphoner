package org.babysherlock.classify

import edu.stanford.nlp.ling.{RVFDatum, Datum}
import edu.stanford.nlp.classify.RVFDataset
import java.util
import edu.stanford.nlp
import scala.collection.mutable.ArrayBuffer
import edu.stanford.nlp.stats.{ClassicCounter, Counter}

/**
 * Scala interface for JavaNLP datums
 * @author Angel Chang
 */
// Creates a datum for interfacing with JavaNLP classifiers
trait DatumCreator[C,L,F] {
  def createDatum(c: C, label: L): Datum[L,F]
}

case class MappedDatum[L,F](id: String, features: Map[F,Double], label: L)  {
}

class MappedDatumCreator[C <: MappedDatum[L,F],L,F] extends FeatureFactory[C,L,F] with DatumCreator[C,L,F] {
  def createDatum(m: C, label: L): Datum[L,F] = {
    val counter = getFeatures(m)
    new RVFDatum[L,F](counter, label)
  }

  def getFeatures(m: C): Counter[F] = {
    val counter = new ClassicCounter[F]()
    for (p <- m.features) {
      counter.incrementCount(p._1, p._2)
    }
    counter
  }

  def getInstanceHeader(m: C, label: L): String = m.id
}

/**
 * Datum Creator that groups features into feature sets
 */
abstract class FeatureSetDatumCreator[E,L <: Object](val featureSet: String)
  extends FeatureFactory[E,L,String] with DatumCreator[E,L,String] {
  type FeatureExtractor = (E, Counter[String]) => Counter[String]

  def featureSets: Map[String, Seq[String]]
  def getFeatureExtractor(feature: String): FeatureExtractor

  protected def addFeatures(featType: String)(elem: E, counter: Counter[String]): Counter[String] = {
    val feats = featureSets.get(featType)
    if (feats.isDefined) {
      for (f <- feats.get) {
        addFeatures(f)(elem, counter)
      }
    } else {
      val f = getFeatureExtractor(featType)
      if (f != null) {
        f(elem, counter)
      } else {
        throw new RuntimeException("Unknown feature type: " + featType)
      }
    }
    counter
  }

  def createDatum(elem: E, label: L): Datum[L,String] = {
    val counter = getFeatures(elem)
    new RVFDatum[L,String](counter, label)
  }

  def getFeatures(elem: E): Counter[String] = {
    val counter = new ClassicCounter[String]()
    addFeatures(featureSet)(elem, counter)
  }

}

/**
 * Utility functions for adding features
 */
object FeatureExtractorUtils {
  def addFeaturesToCounter(featType: String, feats: Array[Double], counter: Counter[String]): Counter[String] = {
    if (feats != null) {
      for (i <- 0 until feats.length) {
        counter.setCount(featType + "." + i, feats(i))
      }
    }
    counter
  }

  def addFeatureValueToCounter(featType: String, value: Double, counter: Counter[String]): Counter[String] = {
    counter.setCount(featType, value)
    counter
  }

  def addBinaryFeatureToCounter(featType: String, featName: String, counter: Counter[String]): Counter[String] = {
    if (featName != null) {
      counter.setCount(featType + "." + featName, 1)
    }
    counter
  }

  // Extract features from the element and add them
  def addFeatureCounts[E](featType: String, extract: E => Counter[String])(elem: E, counter: Counter[String]): Counter[String] = {
    val feats = extract(elem)
    counter.addAll(feats)
    counter
  }

  def addFeatures[E](featType: String, extract: E => Array[Double])(elem: E, counter: Counter[String]): Counter[String] = {
    val feats = extract(elem)
    addFeaturesToCounter(featType, feats, counter)
  }

  def addFeature[E](featType: String, extract: E => Double)(elem: E, counter: Counter[String]): Counter[String] = {
    val value = extract(elem)
    addFeatureValueToCounter(featType, value, counter)
  }

  def addBinaryFeature[E](featType: String, extract: E => String)(elem: E, counter: Counter[String]): Counter[String] = {
    val feat = extract(elem)
    addBinaryFeatureToCounter(featType, feat, counter)
  }
}

// Dataset
class Dataset[C,L,F](val datumCreator: DatumCreator[C,L,F]) extends DatasetInfo[C,L,F] {
  dataset = new RVFDataset[L,F]()
  items = new util.ArrayList[nlp.util.Pair[C, L]]()

  def add(c: C, label: L) {
    val datum = datumCreator.createDatum(c,label)
    dataset.add(datum)
    items.add(new nlp.util.Pair(c,label))
  }

  def add(c: C, datum: Datum[L,F]) {
    dataset.add(datum)
    items.add(new nlp.util.Pair(c,datum.label()))
  }

  def add(orig: Dataset[C,L,F], indices: Iterable[Int]) {
    for (i <- indices) {
      add(orig.getInstance(i), orig.getDatum(i))
    }
  }

  def getIndicesForLabel(target: L): Iterable[Int] = {
    val indices = new ArrayBuffer[Int]()
    for (i <- 0 until size) {
      val label = getLabel(i)
      if (target.equals(label)) {
        indices.append(i)
      }
    }
    indices
  }

}

object Dataset {
  def apply[C,L,F](items: Seq[(C,L)], datumCreator: DatumCreator[C,L,F]): Dataset[C,L,F] = {
    val dataset = new Dataset[C,L,F](datumCreator)
    for (item <- items) {
      dataset.add(item._1, item._2)
    }
    dataset
  }

  def apply[C <: MappedDatum[L,F],L,F](items: Seq[C]): Dataset[C,L,F] = {
    val datumCreator = new MappedDatumCreator[C,L,F]()
    val dataset = new Dataset[C,L,F](datumCreator)
    for (item <- items) {
      dataset.add(item, item.label)
    }
    dataset
  }
}

