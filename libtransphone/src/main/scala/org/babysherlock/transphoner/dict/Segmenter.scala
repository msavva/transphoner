package org.babysherlock.transphoner.dict

import org.babysherlock.transphoner.TrieHelper
import edu.stanford.nlp.tokensregex.matcher.{Match, TrieMapMatcher}
import scala.collection.JavaConversions._
import scala.util.matching.Regex

/**
 * A segmenter segments a string into separate words
 * @author Angel Chang
 */
object Segmenter {
  def apply(opts: Map[String, String]): Segmenter = {
    val segmenterType = opts.get("segmenter")
    if (segmenterType.isDefined) {
      segmenterType.get match {
        case "dict" => new DictSegmenter()
        case "dictfreq" => new DictFreqSegmenter()
        case "char" => new CharSegmenter()
        case "delimited" => new RegexSegmenter(opts.getOrElse("segmenter.delimiter", "\\s+"))
        case _ => throw new RuntimeException("Unsupported segmeter type: " + segmenterType.get)
      }
    } else {
      null
    }
  }
}

trait Segmenter {
  def segment(language: Language, s: String): Seq[(String,Word)]
}

// Regex based segmenter - given regex is used as delimiter
class RegexSegmenter(regex: Regex) extends Segmenter {
  def this(regex: String) = this(new Regex(regex))
  def segment(language: Language, str: String): Seq[(String, Word)] = {
    val strs = regex.split(str)
    strs.map(s => (s, language.toWord(s)))
  }
}

// Single character segmenter - each character is treated as separate word
class CharSegmenter() extends RegexSegmenter("")

// Dictionary based segmenter - segments based on words in the dictionary
class DictSegmenter() extends Segmenter {
  def segment(language: Language, s: String): Seq[(String, Word)] = {
    val matcher = new TrieMapMatcher[Char,Set[Word]](language.charTrie)
    val segments = matcher.segment(s.toSeq)
    segments.map( s => (s.getMatched.mkString, if (s.getValue != null) s.getValue.head else null ) )
  }
}

// Dictionary based segmenter - segements based on word frequency (favors more frequent words)
class DictFreqSegmenter() extends Segmenter {
  def segment(language: Language, s: String): Seq[(String, Word)] = {
    val matcher = new TrieMapMatcher[Char,Set[Word]](language.charTrie)
    val matchScorer: Match[Char,Set[Word]] => Double = x => {
      val score = x.getMatchedLength*(1 + x.getValue.map( w => if (w.rank > 0 && w.rank < 2000) 1.0 else if (w.rank > 0) 1.0/w.rank else 0.0 ).max)
      score
    }
    val segments = matcher.segment(s.toSeq, TrieHelper.toMatchScorer(matchScorer))
    segments.map( s => (s.getMatched.mkString, if (s.getValue != null) s.getValue.head else null ) )
  }
}

