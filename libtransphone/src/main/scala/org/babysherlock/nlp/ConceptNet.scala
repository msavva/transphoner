package org.babysherlock.nlp

import org.apache.solr.client.solrj.{SolrQuery, SolrServer}
import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.babysherlock.Constants
import org.babysherlock.util.{CSVFile, IOUtils, WebUtils, SolrUtils}
import scala.collection.JavaConversions._
import au.com.bytecode.opencsv.CSVWriter
import scala.Array
import org.apache.solr.client.solrj.SolrQuery.ORDER

/**
 * Interface to query concept net
 * @author Angel Chang
 */
class ConceptNet(val solrUrl: String = Constants.CONCEPTNET_SOLR_URL,
                 val apiUrl: String = Constants.CONCEPTNET_API_URL) {
  lazy val conceptNetServer: SolrServer = new HttpSolrServer( solrUrl )

  object ConceptNetSolrFields extends Enumeration {
    val rel, start, startLemmas, end, endLemmas, surfaceText, weight, sources = Value
  }

  def queryCached(cacheFilename: String, queryFunc: () => Seq[ConceptNetRelation]): Seq[ConceptNetRelation] = {
    if (IOUtils.isReadableFileWithData(cacheFilename)) {
      readRelations(cacheFilename)
    } else {
      def rels = queryFunc()
      saveRelations(cacheFilename, rels)
      readRelations(cacheFilename)
    }
  }

  def queryRelationsWithIds(relname: String, start: String, end: String): Seq[ConceptNetRelation] = {
    val solrQueryStrParts = Seq(
      makeFieldQueryString(ConceptNetSolrFields.rel.toString, relname),
      makeFieldQueryString(ConceptNetSolrFields.start.toString, start),
      makeFieldQueryString(ConceptNetSolrFields.end.toString, end)
    )
    val solrQueryStr = solrQueryStrParts.mkString(" AND ")
    query(solrQueryStr)
  }

  def queryRelationsWithLemmas(relname: String, startText: String, endText: String): Seq[ConceptNetRelation] = {
    val solrQueryStrParts = Seq(
      makeFieldQueryString(ConceptNetSolrFields.rel.toString, relname),
      makeFieldQueryString(ConceptNetSolrFields.startLemmas.toString, startText),
      makeFieldQueryString(ConceptNetSolrFields.endLemmas.toString, endText)
    )
    val solrQueryStr = solrQueryStrParts.mkString(" AND ")
    query(solrQueryStr)
  }

  def queryRelations(relname: String, limit: Int = -1): Seq[ConceptNetRelation] = {
    val solrQueryStr = makeFieldQueryString(ConceptNetSolrFields.rel.toString, relname)
    query(solrQueryStr, limit)
  }

  def query(solrQueryStr: String, limit: Int = -1): Seq[ConceptNetRelation] = {
    val solrQuery = new SolrQuery( solrQueryStr )
    solrQuery.setSort("weight", ORDER.desc)
//    val fq = makeFieldQueryString(ConceptNetSolrFields.end.toString, "/c/en/bedroom")
//    solrQuery.setFilterQueries(fq)
//    if (limit > 0) solrQuery.setParam("rows", limit.toString)
//    val response = conceptNetServer.query(solrQuery)
//    val results = response.getResults
    val results = SolrUtils.query(conceptNetServer, solrQuery, totalLimit = limit )
    results.map( x => ConceptNetRelation(
      rel = x.get(ConceptNetSolrFields.rel.toString).asInstanceOf[String],
      start = x.get(ConceptNetSolrFields.start.toString).asInstanceOf[String],
      startLemmas = x.get(ConceptNetSolrFields.startLemmas.toString).asInstanceOf[String],
      end = x.get(ConceptNetSolrFields.end.toString).asInstanceOf[String],
      endLemmas = x.get(ConceptNetSolrFields.endLemmas.toString).asInstanceOf[String],
      surfaceText = x.get(ConceptNetSolrFields.surfaceText.toString).asInstanceOf[String],
      weight = x.get(ConceptNetSolrFields.weight.toString).asInstanceOf[Number].doubleValue()
//      sources = x.get(ConceptNetSolrFields.sources.toString).asInstanceOf[java.util.List[_]].map( x => x.toString ).toSeq
    ))
  }

  def toWord(conceptId: String, langId: String): String = {
    if (conceptId.startsWith(langId + "/")) {
      val w = conceptId.substring(langId.length + 1)
      val i = w.indexOf("/")
      if (i > 0) {
        w.slice(0, i)
      } else if (i == 0) null
      else w
    } else null
  }
  def toLangId(lang: String): String = "/c/" + lang.toLowerCase
  def toConceptId(w: String, lang: String): String = "/c/" + lang.toLowerCase + "/" + w.toLowerCase
  def getSimilarity(c1: String, c2: String): Option[Double] = {
    val path = apiUrl + "assoc" + c1
    val params = Map(
      "filter" -> c2,
      "limit" -> "10"
    )
    val json = WebUtils.loadJson(path, params)
//    println(json)
    val similarScores = json.map(
      x => ConceptNetSimilarityResponse(x)
    )
    if (similarScores.isDefined) {
      val similar = similarScores.get.similar
      // Check search result - make sure that the word is the same
      if (similar.nonEmpty) {
        for ((rc,rs) <- similar) {
          if (rc == c2 || rc.startsWith(c2 + "/")) {
            return Option(rs)
          }
        }
      }
      None
    } else None
  }

  def getRelated(c1: String, n: Int, filter: String = null): Seq[(String,Double)] = {
    def related = queryRelationsWithIds(ConceptNetRelations.RELATED_TO, filter, c1)
    related.map( x => (x.start,x.weight) ).take(n)
  }

  def getSimilar(c1: String, n: Int, filter: String = null): Seq[(String,Double)] = {
    val path = apiUrl + "assoc" + c1
    var params = Map[String,String](
      "limit" -> n.toString
    )
    if (filter != null) {
      params = params ++ Map("filter" -> filter)
    }
    val json = WebUtils.loadJson(path, params)
    println(json)
    val similarScores = json.map(
      x => ConceptNetSimilarityResponse(x)
    )
    if (similarScores.isDefined) {
      val similar = similarScores.get.similar
      similar
    } else Seq()
  }

  private def makeFieldQueryString(fieldName: String, value: String) = SolrUtils.makeFieldQueryString(fieldName, value)
  private def makeFieldQueryString(fieldName: String, parts: Seq[String], conj: String) = SolrUtils.makeFieldQueryString(fieldName, parts, conj)

  def saveRelations(filename: String, rels: => Seq[ConceptNetRelation]) {
    val csvWriter = new CSVWriter(IOUtils.filePrintWriter(filename))
    val header = Array("rel", "weight", "start", "startLemmas", "end", "endLemmas", "surfaceText")
    csvWriter.writeNext(header)
    for (rel <- rels) {
      val row = Array(rel.rel, rel.weight.toString, rel.start, rel.startLemmas, rel.end, rel.endLemmas, rel.surfaceText)
      csvWriter.writeNext(row)
    }
    csvWriter.close()
  }

  def readRelations(filename: String): Seq[ConceptNetRelation] = {
    val csvFile = new CSVFile(filename, includesHeader = true)
    val rels = for (row <- csvFile.rows) yield {
      ConceptNetRelation(
        row("rel"), row("start"), row("startLemmas"),
        row("end"), row("endLemmas"), row("surfaceText"), row("weight").toDouble )
    }
    rels.toSeq
  }
}

private case class ConceptNetSimilarityResponse(
  terms: Seq[(String,Double)],
  similar: Seq[(String,Double)]
)

private object ConceptNetSimilarityResponse {
  private def toScores(obj: Object): Seq[(String,Double)] = {
    val list = obj.asInstanceOf[java.util.List[Object]]
    list.map( x => {
      val s = x.asInstanceOf[java.util.List[Object]]
      assert(s.size == 2)
      (s.get(0).asInstanceOf[String], s.get(1).asInstanceOf[Double])
    }).toSeq
  }
  def apply(json: java.util.Map[String,Object]): ConceptNetSimilarityResponse = {
    new ConceptNetSimilarityResponse(
      terms = toScores(json("terms")),
      similar = toScores(json("similar"))
    )
  }
  def apply(json: Object): ConceptNetSimilarityResponse = apply(json.asInstanceOf[java.util.Map[String,Object]])
}

case class ConceptNetRelation(//id: String,
                              rel: String = null,
                              start: String = null,
                              startLemmas: String = null,
                              end: String = null,
                              endLemmas: String = null,
                              surfaceText: String = null,
                              weight: Double
//                              sources: Seq[String] = null
                              )

object ConceptNet extends ConceptNet(Constants.CONCEPTNET_SOLR_URL, Constants.CONCEPTNET_API_URL) {
}

object ConceptNetRelations {
  // List of concept net relations that we care about
  val AT_LOCATION = "/r/AtLocation"
  val LOCATED_NEAR = "/r/LocatedNear"
  val RELATED_TO = "/r/RelatedTo"
  val PART_OF = "/r/PartOf"
  val HAS_A = "/r/HasA"
  val NOT_HAS_A = "/r/NotHasA"
  val HAS_PROPERTY = "/r/HasProperty"
  val IS_A = "/r/IsA"

  val HAS_PREREQUISITE = "/r/HasPrerequisite"
  val USED_FOR = "/r/UsedFor"
  val CAPABLE_OF = "/r/CapableOf"

  val ANTONYM = "/r/Antonym"

  // Summary of relations and the number of total instances (across all languages)
//    /r/TranslationOf : 2640709
//    /r/PartOf : 513000
//    /r/AtLocation : 355225
//    /r/RelatedTo : 313629
//    /r/DerivedFrom : 235892
//    /r/ConceptuallyRelatedTo : 223270
//    /r/Synonym : 106017
//    /r/HasProperty : 99781
//    /r/Causes : 92110
//    /r/UsedFor : 87327
//    /r/CapableOf : 80955
//    /r/MotivatedByGoal : 79349
//    /r/HasSubevent : 75436
//    /r/Desires : 48285
//    /r/HasPrerequisite : 26853
//    /r/CausesDesire : 26253
//    /r/HasA : 25495
//    /r/MadeOf : 18595
//    /r/Antonym : 18073
//    /r/HasFirstSubevent : 16705
//    /r/DefinedAs : 16514
//    /r/MemberOf : 12290
//    /r/SimilarTo : 11561
//    /r/ReceivesAction : 10741
//    /r/InstanceOf : 10085
//    /r/HasContext : 8952
//    /r/wordnet : 6726
//    /r/SymbolOf : 5929
//    /r/NotDesires : 5847
//    /r/LocatedNear : 5339
//    /r/ObstructedBy : 5172
//    /r/HasLastSubevent : 4354
//    /r/NotIsA : 3837
//    /r/wordnet/adjectivePertainsTo : 3785
//    /r/Derivative : 3594
//    /r/NotUsedFor : 3252
//    /r/NotCapableOf : 2916
//    /r/wordnet/adverbPertainsTo : 2880
//    /r/SimilarSize : 2671
//    /r/DesireOf : 2455
//    /r/NotHasProperty : 1144
//    /r/InheritsFrom : 1002
//    /r/Attribute : 639
//    /r/CreatedBy : 635
//    /r/Entails : 408
//    /r/NotHasA : 407
//    /r/LocationOfAction : 185
//    /r/HasPainIntensity : 74
//    /r/wordnet/participleOf : 61
//    /r/HasPainCharacter : 34
//    /r/NotMadeOf : 24
//    /r/NotCauses : 2

}
