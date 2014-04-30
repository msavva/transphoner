package org.babysherlock.nlp

import org.babysherlock.similarity.SimilarityModels
import org.babysherlock.util.BatchSampler
import org.lexvo.uwn._
import java.io.File
import scala.collection.JavaConversions._
import scala.util.Random
import scala.util.matching.Regex

/**
 * Wrapper for the UWN (Universal wordnet)
 * @author Angel Chang
 */
class UWNWrapper(val pluginsDir: String = UWNConstants.pluginsDir) {
  val uwn = new UWN(new File(pluginsDir))

  def lookup(term: String, lang: String): Iterable[Statement] = {
    val entity = Entity.createTerm(term, lang)
    uwn.get(entity).toIterable
  }

  def lookupGlosses(synsetId: String): Iterable[Statement] = {
    val entity = new Entity(synsetId)
    uwn.getGlosses(entity).toIterable
  }

  def lookupGlosses(term: String, lang: String): Iterable[(String, Iterable[Statement], Float)]  = {
    val synsets = lookupSynsets(term, lang)
    for (synset <- synsets) yield {
      val id = synset.getObject.getId
      (id, uwn.getGlosses(synset.getObject).toIterable, synset.getWeight )
    }
  }

  def lookupLexicalizations(term: String, lang: String, outputLangs: Set[String]): Iterable[(String, Iterable[Statement], Float)] =
    lookupLexicalizations(term, lang, filter = Option( (s:Statement) =>  outputLangs.contains( s.getObject.getTermLanguage )))

  def lookupLexicalizations(term: String, lang: String, filter: Option[Statement => Boolean] = None):
    Iterable[(String, Iterable[Statement], Float)]  =
  {
    val synsets = lookupSynsets(term, lang)
    for (synset <- synsets) yield {
      val id = synset.getObject.getId
      val terms = uwn.getTermEntities(synset.getObject).toIterable
      if (filter.isDefined) {
        (id, terms.filter( filter.get ), synset.getWeight )
      } else {
        (id, terms, synset.getWeight )
      }
    }
  }

  def lookupSynsetLexicalizations(synsetId: String, outputLangs: Set[String]): Iterable[Statement] =
    lookupSynsetLexicalizations(synsetId, filter = Option( (s:Statement) =>  outputLangs.contains( s.getObject.getTermLanguage )))

  def lookupSynsetLexicalizations(synsetId: String, filter: Option[Statement => Boolean] = None): Iterable[Statement] = {
    val entity = new Entity(synsetId)
    val terms = uwn.getTermEntities(entity).toIterable
    if (filter.isDefined) {
      terms.filter( filter.get )
    } else {
      terms
    }
  }

  /** Do random walk in graph and find a related synset **/
  private val sampler = new BatchSampler
  def randomRelatedSynset(term: String, lang: String, steps: Int): Option[String] = {
    val synsets = lookupWeightedSynsetIds(term, lang)
    if (synsets.nonEmpty) {
      // Pick one of the synsets with probability proportional to weight
      val synsetId = sampler.sampleWeightedWithoutReplacement(synsets, 1).head._1
      randomRelatedSynset(synsetId, steps)
    } else None
  }

  def randomRelatedSynset(synsetId: String, steps: Int): Option[String] = {
    if (steps > 0) {
      val rand = Random.nextFloat()
      if (rand > 0.5) {
        val entity = new Entity(synsetId)
        val parents = uwn.getParentClasses(entity)
        val subclasses = uwn.getSubClasses(entity)
        val combined = (parents ++ subclasses).map( s => (s.getObject.getId, s.getWeight.toDouble) )
        if (combined.nonEmpty) {
          val next = sampler.sampleWeightedWithoutReplacement(combined.toIterable, 1).head._1
          randomRelatedSynset(next, steps-1)
        } else {
          None
        }
      } else {
        val randTerm = randomRelatedTerm(synsetId, Set("eng"))
        if (randTerm.isDefined) {
          randomRelatedSynset(randTerm.get._1, randTerm.get._2, steps-1 )
        } else {
          None
        }
      }
    } else Option(synsetId)
  }

  def randomRelatedTerm(synsetId: String, targetLangs: Set[String]): Option[(String,String)] = {
    val terms = lookupSynsetLexicalizations(synsetId, targetLangs).map( s => (s.getObject, s.getWeight.toDouble) )
    if (terms.nonEmpty) {
      val term = sampler.sampleWeightedWithoutReplacement(terms, 1).head._1
      Option((term.getTermStr, term.getTermLanguage))
    } else None
  }

  def randomRelatedTerm(term: String, lang: String, targetLang: String, steps: Int): Option[String] = {
    val synset = randomRelatedSynset(term, lang, steps)
    if (synset.isDefined) {
      randomRelatedTerm( synset.get, Set(targetLang) ).map( a => a._1 )
    } else None
  }

  def randomRelatedTerms(term: String, lang: String, targetLang: String, steps: Int, n: Int): Stream[String] = {
    if (n == 0) Stream.empty
    else {
      val related = randomRelatedTerm(term, lang, targetLang, steps)
      if (related.isEmpty) Stream.empty
      else Stream.cons(
        related.get,
        randomRelatedTerms(term, lang, targetLang, steps, n-1) )
    }
  }

  def lookupSynsets(term: String, lang: String): Iterable[Statement] = {
    var synsets = uwn.getMeaningEntities(term, lang).toIterable
    if (synsets.isEmpty) {
      // Hmm, lets try again with different case
      if (term.charAt(0).isLower) {
        // Try uppercasing
        synsets = uwn.getMeaningEntities(term.capitalize, lang).toIterable
      } else {
        // Try lowercasing
        synsets = uwn.getMeaningEntities(term.toLowerCase, lang).toIterable
      }
    }
    synsets
  }

  def lookupWeightedSynsetIds(term: String, lang: String): Seq[(String,Double)] = {
    val statements = lookupSynsets(term, lang)
    val (senses, nonsenses) = statements.partition( s => s.getObject.getId.startsWith("s/"))
//    if (nonsenses.nonEmpty) {
//      println("Got unexpected senses for " + lang + ":" + term + ": " + nonsenses.mkString(","))
//    }
    sortedWeightedIds(senses)
  }

  def isHyponymOf(hypoSynset: String, hyperSynset: String): Boolean = {
    isHypernymOf(hyperSynset, hypoSynset)
  }

  def isHypernymOf(hyperSynset: String, hypoSynset: String): Boolean = {
    if (hypoSynset == hyperSynset) true
    else {
      val anc = lookupAncestorSynsetsIds(hypoSynset).toSet
      anc.contains(hyperSynset)
    }
  }

  def getAllHyponymSynsets(term: String, lang: String, pos: Option[Char] = None): Set[String] = {
    var synsets = lookupSynsets(term,  lang)
    if (pos.isDefined) {
      synsets = synsets.filter( s => s.getObject.getId.startsWith("s/" + pos.get) )
    }
    if (synsets.nonEmpty) {
      // Just pick first synset
      lookupDescendantSynsetsIds(synsets.head.getObject.getId).toSet
    } else Set()
  }

  def lookupChildSynsets(synsetId: String): Iterable[Statement] = {
    val entity = new Entity(synsetId)
    uwn.getSubClasses(entity).toIterable
  }

  def lookupChildSynsetsIds(synsetId: String): Seq[String] = {
    lookupChildSynsets(synsetId).map( s => s.getObject.getId ).toSeq.distinct
  }

  def lookupChildWeightedSynsetsIds(synsetId: String): Iterable[(String,Double)] = {
    val statements = lookupChildSynsets(synsetId)
    sortedWeightedIds(statements)
  }

  def lookupParentSynsets(synsetId: String): Iterable[Statement] = {
    val entity = new Entity(synsetId)
    uwn.getParentClasses(entity).toIterable
  }

  def lookupParentSynsetsIds(synsetId: String): Seq[String] = {
    lookupParentSynsets(synsetId).map( s => s.getObject.getId ).toSeq.distinct
  }

  def lookupParentWeightedSynsetsIds(synsetId: String): Iterable[(String,Double)] = {
    val statements = lookupParentSynsets(synsetId)
    sortedWeightedIds(statements)
  }

  def lookupDescendants(e: Entity): Iterable[Statement] = {
    def helper(t: Entity, done: Set[Entity]): Iterable[Statement] = {
      if (done.contains(t)) Iterable.empty
      else {
        val children = uwn.getSubClasses(e).toIterable
        var newDone = done ++ Seq(t)
        var res = children
        for (c <- children.map( s => s.getObject )) {
          if (!newDone.contains(c)) {
            newDone = newDone + c
            res = res ++ helper(c, newDone)
          }
        }
        res
      }
    }
    helper(e, Set.empty)
  }

  def lookupDescendantSynsets(synsetId: String): Iterable[Statement] = {
    lookupDescendants(new Entity(synsetId))
  }

  def lookupDescendantSynsetsIds(synsetId: String): Seq[String] = {
    lookupDescendantSynsets(synsetId).map( s => s.getObject.getId ).toSeq.distinct
  }

  def lookupDescendantWeightedSynsetsIds(synsetId: String): Iterable[(String,Double)] = {
    val statements = lookupDescendantSynsets(synsetId)
    sortedWeightedIds(statements)
  }

  def lookupAncestors(e: Entity): Iterable[Statement] = {
    def helper(t: Entity, done: Set[Entity]): Iterable[Statement] = {
      if (done.contains(t)) Iterable.empty
      else {
        val parents = uwn.getParentClasses(e).toIterable
        var newDone = done ++ Seq(t)
        var res = parents
        for (p <- parents.map( s => s.getObject )) {
          if (!newDone.contains(p)) {
            newDone = newDone + p
            res = res ++ helper(p, newDone)
          }
        }
        res
      }
    }
    helper(e, Set.empty)
  }

  def lookupAncestorSynsets(synsetId: String): Iterable[Statement] = {
    lookupAncestors(new Entity(synsetId))
  }

  def lookupAncestorSynsetsIds(synsetId: String): Seq[String] = {
    lookupAncestorSynsets(synsetId).map( s => s.getObject.getId ).toSeq.distinct
  }

  def lookupAncestorWeightedSynsetsIds(synsetId: String): Iterable[(String,Double)] = {
    val statements = lookupAncestorSynsets(synsetId)
    sortedWeightedIds(statements)
  }

  def sortedWeightedIds(statements: Iterable[Statement]): Seq[(String, Double)] = {
    val map = statements.map( s => (s.getObject.getId, s.getWeight) ).groupBy( _._1 )
    val aggregated = map.mapValues( x => x.map( y => y._2.toDouble ).sum /*/x.size*/ ).toSeq
    aggregated.sortBy( -_._2 )
    //    statements.map( s => (s.getObject.getId, s.getWeight.toDouble) ).toSeq
  }

  // Convert UWN sense id to wordnet synset id
  val uwnWordNetSynsetIdRegex = new Regex("s/([a-z])(\\d+)")
  def toWordNetSynsetId(s: String): String = s match {
    case uwnWordNetSynsetIdRegex(pos,offset) => pos + "%08d".format(offset.toInt)
    case wordNetSynsetIdRegex(pos,offset) => pos + "%08d".format(offset.toInt)
  }

  // Convert wordnet synset id to UWN sense id
  val wordNetSynsetIdRegex = new Regex("([a-z])(\\d+)")
  def toUWNSenseId(s: String): String = s match {
    case wordNetSynsetIdRegex(pos,offset) => "s/" + pos + "%d".format(offset.toInt)
    case uwnWordNetSynsetIdRegex(pos,offset) => s
  }
}

object UWNConstants extends org.babysherlock.Constants {
  val uwnDir = EXT_DATA_DIR + "wordnet" + File.separator
  val pluginsDir =  uwnDir + "uwnapi" + File.separator + "plugins" + File.separator
}

object UWN extends UWNWrapper() {
}

object UWNTest extends App {
  val uwn = UWN

  val info = uwn.lookup("water", "eng")
  println(info.mkString("\n"))

  println("Glosses")
  val glosses = uwn.lookupGlosses("water", "eng")
  println(glosses.flatMap( m => m._2 ).mkString("\n"))

  println("Lexicalizations")
  val translations = uwn.lookupLexicalizations("water", "eng", Set("eng", "fra", "deu", "jpn", "cmn"))
  println(translations.flatMap( m => m._2 ).mkString("\n"))

  println("Synsets")
  val synsets = uwn.lookupSynsets("water", "eng")
  println(synsets.mkString("\n"))
  // lookup parents
  for (synset <- synsets) {
    val parents = uwn.lookupParentSynsets(synset.getObject.getId)
    println("synset " + synset + " has parents " + parents.mkString(","))
  }

  val random = uwn.randomRelatedTerms("cook", "eng", "eng", 2, 10)
  println(random.mkString("\n"))

  // Check against wordnet
  val wordnetWrapper = WordNetWrapper
  val wordnet = SimilarityModels.wordnet()
  val synsetGlosses = synsets.foreach(
    s => {
      val glosses = uwn.lookupGlosses(s.getObject.getId)
      val id2 = s.getObject.getId.substring(2)
      val id2padded = id2.take(1) + "%08d".format(id2.drop(1).toInt)
      val simModelKnowsSynset = wordnet.id2node.contains( id2padded )
      val wn31Synset = wordnetWrapper.getSynset( id2 )
      val wn31KnowsSynset = wn31Synset != null
      println(glosses.mkString("\n"))
      val v = if (wn31KnowsSynset) wn31Synset.getGloss else ""
      println("knows synset: " + simModelKnowsSynset + ", " + wn31KnowsSynset + " " + v)
    }
  )
}
