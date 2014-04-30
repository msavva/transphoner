package org.babysherlock.nlp

import edu.stanford.nlp.ling.{CoreLabel, CoreAnnotations}
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.stats.{TwoDimensionalCounter, ClassicCounter, Counter}
import edu.stanford.nlp.util.CoreMap
import java.util.Properties
import java.util.{List => JList}
import scala.collection.JavaConversions._

/**
 *  Wrapper around the Stanford corenlp
 *  @author Angel Chang
 */
object StanfordCorenlpWrapper {
  val basicProperties = new Properties
  basicProperties.setProperty("annotators", "tokenize, ssplit, pos, lemma")
  lazy val basicPipeline = new StanfordCoreNLP(basicProperties)

  def getNumber(t: CoreLabel): Number = {
    val numtype = t.get[String](classOf[CoreAnnotations.NumericCompositeTypeAnnotation])
    if (numtype == "NUMBER") {
      t.get[Number](classOf[CoreAnnotations.NumericCompositeValueAnnotation])
    } else {
      null
    }
  }

  def isNJRV(t: CoreLabel): Boolean = {
    val tag = t.tag()
    tag.startsWith("N") || tag.startsWith("V") || tag.startsWith("J") || tag.startsWith("R")
  }

  def isVerb(t: CoreLabel): Boolean = {
    val tag = t.tag()
    tag.startsWith("V")
  }

  def isNoun(t: CoreLabel): Boolean = {
    val tag = t.tag()
    tag.startsWith("N")
  }

  def isAdjective(t: CoreLabel): Boolean = {
    val tag = t.tag()
    tag.startsWith("J")
  }

  def isAdverb(t: CoreLabel): Boolean = {
    val tag = t.tag()
    tag.startsWith("R")
  }

  def getSentences(text: String): JList[CoreMap] = {
    val annotation = basicPipeline.process(text)
    val sentences = annotation.get[JList[CoreMap]](classOf[CoreAnnotations.SentencesAnnotation])
    sentences
  }

  def getSentences(annotation: Annotation) = {
    annotation.get[JList[CoreMap]](classOf[CoreAnnotations.SentencesAnnotation])
  }

  def getTokens(text: String): JList[CoreLabel] = {
    val annotation = basicPipeline.process(text)
    getTokens(annotation)
  }

  def getTokens(annotation: CoreMap): JList[CoreLabel] = {
    annotation.get[JList[CoreLabel]](classOf[CoreAnnotations.TokensAnnotation])
  }

  def getText(annotation: CoreMap): String = {
    annotation.get[String](classOf[CoreAnnotations.TextAnnotation])
  }

  def getPosBowFeatures0(text: String, filter: CoreLabel => Boolean = { t => true},
                         counter: TwoDimensionalCounter[String, String] = new TwoDimensionalCounter[String, String]()): TwoDimensionalCounter[String, String] = {
    val annotation = basicPipeline.process(text)
    val tokens = annotation.get[JList[CoreLabel]](classOf[CoreAnnotations.TokensAnnotation])
    tokens.filter(filter).foreach(
      t => counter.incrementCount(t.tag(), t.lemma())
    )
    counter
  }

  def getPosBowFeatures(iter: Iterable[String], filter: CoreLabel => Boolean = { t => true},
                        counter: TwoDimensionalCounter[String, String] = new TwoDimensionalCounter[String, String]()): TwoDimensionalCounter[String, String] = {
    iter.foreach(
      text => getPosBowFeatures0(text, filter, counter)
    )
    counter
  }

  def getBowFeatures0(text: String, filter: CoreLabel => Boolean = { t => true}, weight: Double = 1,
                      counter: Counter[String] = new ClassicCounter[String](), prefix: String = null): Counter[String] = {
    val annotation = basicPipeline.process(text)
    val tokens = annotation.get[JList[CoreLabel]](classOf[CoreAnnotations.TokensAnnotation])
    tokens.filter(filter).foreach(
      t => counter.incrementCount( if (prefix != null) prefix + t.lemma() else t.lemma(), weight )
    )
    //println(tokens.map(t => t.word() + " " + t.lemma() + " " + t.tag())mkString("\n"))
    counter
  }

  def getBowFeatures(iter: Iterable[String], filter: CoreLabel => Boolean = { t => true}, weight: Double = 1,
                     counter: Counter[String] = new ClassicCounter[String](), prefix: String = null): Counter[String] = {
    iter.foreach(
      text => getBowFeatures0(text, filter, weight, counter, prefix)
    )
    counter
  }

  def filterCounter[A](counter: Counter[A], filter: (A,Double) => Boolean ): Counter[A] = {
    val res: Counter[A] = counter.getFactory.create()
    for (k <- counter.keySet()) {
      val c = counter.getCount(k)
      if (filter(k, c)) res.setCount(k, c)
    }
    res
  }
}
