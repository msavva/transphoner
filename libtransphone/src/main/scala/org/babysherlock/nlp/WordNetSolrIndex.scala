package org.babysherlock.nlp

import net.sf.extjwnl.data.{Synset, POS}
import net.sf.extjwnl.dictionary.Dictionary
import scala.collection.JavaConversions._
import java.io.FileInputStream
import org.apache.solr.common.SolrInputDocument
import org.apache.solr.client.solrj.{SolrQuery, SolrServer}
import org.apache.solr.client.solrj.impl.{HttpSolrServer, ConcurrentUpdateSolrServer}
import org.apache.solr.client.solrj.util.ClientUtils
import org.apache.solr.common.util.NamedList
import edu.stanford.nlp.stats.{ClassicCounter, Counters, Counter}
import org.apache.solr.client.solrj.response.QueryResponse

/**
 *  Uses Solr to index word net
 *  @author Angel Chang
 */
class WordNetSolrIndex(val dictionary: Dictionary, val solrUrl: String = WordNetConstants.WORDNET_SOLR_URL) extends WordNetSolrConstants {
  val solrServer: SolrServer = new HttpSolrServer( solrUrl )
  val queryLimit = 100

  def this(conffile: String = WordNetConstants.WORDNET_CONF_FILE) { this(Dictionary.getInstance(new FileInputStream(conffile))) }

  def getSynsetTfidf(synset: Synset): Map[String, Double] = {
    val synsetid = getSynsetId(synset)
    getSynsetTfidf(synsetid)
  }

  def getSynsetTfidf(synsetid: String, termField: String = FIELD_TEXT): Map[String, Double] = {
    val solrQuery = new SolrQuery( makeFieldQueryString(FIELD_SYNSET_ID, synsetid) )
    solrQuery.setRequestHandler("/tvrh")
    solrQuery.set("tv.tf_idf", true)
    val response = solrServer.query(solrQuery)
    val nm = response.getResponse
    val termVectors = nm.get("termVectors").asInstanceOf[NamedList[AnyRef]]
    val synsetTermVector = termVectors.get(synsetid).asInstanceOf[NamedList[AnyRef]]
    val targetTermVector = synsetTermVector.get(termField).asInstanceOf[NamedList[AnyRef]]
    // convert to map from string to double
    val res = targetTermVector.map( e => e.getKey -> e.getValue.asInstanceOf[NamedList[Double]].get("tf-idf") ).toMap
    res
  }

  def getSynsetTfidfs(synsets: Set[Synset], termField: String = FIELD_TEXT): Map[Synset, Map[String, Double]] = {
    val synsetsQuery = makeSynsetsQueryString(synsets)
    val solrQuery = new SolrQuery(synsetsQuery)
    solrQuery.setRequestHandler("/tvrh")
    solrQuery.set("tv.tf_idf", true)
    solrQuery.setRows(synsets.size)
    val response = solrServer.query(solrQuery)
    val synsetTfidfs = getSynsetTfidfsFromResponse(response, termField, synsets)
    synsetTfidfs
  }

  private def getSynsetTfidfsFromResponse(response: QueryResponse, field: String, synsets: Set[Synset]): Map[Synset, Map[String, Double]] = {
    val nm = response.getResponse
    val termVectors = nm.get("termVectors").asInstanceOf[NamedList[AnyRef]]
    val tfidfs = for (synset <- synsets) yield {
      val synsetid = getSynsetId(synset)
      val synsetTermVector = termVectors.get(synsetid).asInstanceOf[NamedList[AnyRef]]
      val targetTermVector = synsetTermVector.get(field).asInstanceOf[NamedList[AnyRef]]
      // convert to map from string to double
      synset -> targetTermVector.map( e => e.getKey -> e.getValue.asInstanceOf[NamedList[Double]].get("tf-idf") ).toMap
    }
    tfidfs.toMap
  }

  def getTermTfidfs(termCounts: Counter[String], termField: String = FIELD_TEXT): Map[String, Double] = {
    val dfs = getTermDocumentFrequencies(termCounts.keySet().toSet, termField)
    dfs.map { case (term, df) => term -> ( if (df == 0) 0 else termCounts.getCount(term) / df) }.toMap
  }

  def getTermDocumentFrequencies(terms: Set[String], termField: String = FIELD_TEXT): Map[String, Int] = {
    val solrQuery = new SolrQuery("*:*")
    solrQuery.setRequestHandler("/tvrh")
    solrQuery.setRows(0)
    // Use facets to query terms and get document frequencies
    for (term <- terms) {
      solrQuery.addFacetQuery( makeFieldQueryString(termField, term) )
    }
    val response = solrServer.query(solrQuery)
    if (response.getFacetQuery != null) {
      response.getFacetQuery.map{ case (s,count) => extractFieldValue(s,termField) -> count.intValue }.toMap
    } else {
      Map()
    }
  }

  def extractFieldValue(queryTerm: String, termField: String): String = {
    val prefix = termField + ":"
    val s = queryTerm.substring(prefix.length)
    if (s.startsWith("\"") && s.endsWith("\"")) {
      s.substring(1, s.length-1)
    } else {
      s
    }
  }

  def toCounter[A](m: Map[A,Double]): Counter[A] = {
    val c = new ClassicCounter[A]()
    for (e <- m) {
      c.incrementCount(e._1, e._2)
    }
    c
  }

  def computeCosineSimilarity(a: Map[String,Double], b: Map[String,Double]): Double = {
    val ca = toCounter(a)
    val cb = toCounter(b)
    Counters.cosine(ca,cb)
  }

  def getOrderedSynsetSimilarity(termCounts: Counter[String], termField: String, synsets: Set[Synset]): Seq[(Synset,Double)] = {
    // order from highest score to lowest
    val synsetSims = getSynsetSimilarity(termCounts, termField, synsets)
    synsetSims.toSeq.sortBy( s => -s._2 )
  }

  def getSynsetSimilarity(termCounts: Counter[String], termField: String, synsets: Set[Synset]): Map[Synset,Double] = {
    val queryTfidfs = getTermTfidfs(termCounts, termField)
    val synsetTfidfs = getSynsetTfidfs(synsets, termField)

    // Compute cosine similarity between the two tf-idfs...
    val sims = for ( (synset, synsetTfidf) <- synsetTfidfs) yield {
      synset -> computeCosineSimilarity(queryTfidfs, synsetTfidf)
    }
    sims.toMap
  }

  def getSynsetSimilarity(s1: Synset, s2: Synset, termField: String = FIELD_TEXT): Double = {
    val synsetTfidfs = getSynsetTfidfs(Set(s1,s2), termField)
    val s1Tfidfs = synsetTfidfs(s1)
    val s2Tfidfs = synsetTfidfs(s2)

    // Compute cosine similarity between the two tf-idfs...
    computeCosineSimilarity(s1Tfidfs, s2Tfidfs)
  }

  private def makeSynsetsQueryString(synsetsFilter: Set[Synset]): String = {
    if (synsetsFilter != null) {
      synsetsFilter.map( s => makeFieldQueryString(FIELD_SYNSET_ID, getSynsetId(s)) ).mkString(" OR ")
    } else null
  }

  private def makeFieldQueryString(field: String, value: String): String = {
    field + ":\"" + ClientUtils.escapeQueryChars(value) + "\""
  }

  def queryWords(words: Iterable[String], synsetsFilter: Set[Synset] = null) = {
    val queryStr = words.map( w => makeFieldQueryString(FIELD_WORDS, w) ).mkString(" ")
    query(queryStr, makeSynsetsQueryString(synsetsFilter))
  }

  def queryText(words: Iterable[String], synsetsFilter: Set[Synset] = null) = {
    val queryStr = words.map( w => ClientUtils.escapeQueryChars(w)).mkString(" ")
    query(queryStr, makeSynsetsQueryString(synsetsFilter))
  }

  def query(q: String, fq: String = null): Seq[(Synset, Double)] = {
    val solrQuery = new SolrQuery(q)
    if (fq != null) solrQuery.setFilterQueries(fq)
    solrQuery.setIncludeScore(true)
    solrQuery.setRows(queryLimit)
    val response = solrServer.query(solrQuery)
    val docs = response.getResults
    //docs.foreach( d => println(d.toString) )
    val results = docs.map( d => {
      val synset = getSynset(d.get(FIELD_SYNSET_ID).asInstanceOf[String], dictionary)
      val score = d.get("score").asInstanceOf[Float]
      (synset, score.toDouble)
    } )
    results.toSeq
  }

  def shutdown() {
    solrServer.shutdown()
  }
}

object WordNetSolrIndexer extends App {
  val indexer = new WordNetSolrIndexer()
  indexer.importToSolr()
  indexer.shutdown()
}

class WordNetSolrIndexer(val dictionary: Dictionary, val solrUrl: String = WordNetConstants.WORDNET_SOLR_URL) extends WordNetSolrConstants {
  val batchSize = 50000

  val solrServer: SolrServer = new ConcurrentUpdateSolrServer( solrUrl, batchSize, 1 )

  def this(conffile: String = WordNetConstants.WORDNET_CONF_FILE) { this(Dictionary.getInstance(new FileInputStream(conffile))) }

  def importToSolr() {
    val postags = POS.values()
    postags.foreach( pos => importToSolr(pos))
    print("Finalizing...")
    optimize()
    println("Done finalizing")
  }

  def importToSolr(pos: POS) {
    var count = 0
    val synsets = dictionary.getSynsetIterator(pos)
    for (synset <- synsets) {
      val doc = new SolrInputDocument()
      val id = getSynsetId(synset)
      doc.addField(FIELD_SYNSET_ID, id)
      doc.addField(FIELD_POS, synset.getPOS.getLabel)
      doc.addField(FIELD_GLOSS, synset.getGloss)
      synset.getWords.foreach( w => doc.addField(FIELD_WORDS, w.getLemma) )
      solrServer.add(doc)

      count = count+1
      if (count % batchSize == 0) {
        println("Processed " + count + " entries")
        solrServer.commit()
      }
    }

    println("Total processed " + count + " entries for " + pos.getLabel + ".")
  }

  def optimize() {
    solrServer.commit()
    solrServer.optimize()
  }

  def shutdown() {
    solrServer.shutdown()
  }

  def clear()
  {
    solrServer.deleteByQuery("*:*")
    finalize()
  }

}

object WordNetSynsetId extends WordNetSynsetId {}

trait WordNetSynsetId {
  def getSynsetId(synset: Synset) = synset.getPOS.getKey + synset.getOffset
  def getSynset(synsetId: String, dictionary: Dictionary): Synset = {
    val (pos,offset) = synsetId.splitAt(1)
    try {
      dictionary.getSynsetAt(POS.getPOSForKey(pos), offset.toLong)
    } catch {
      case e:Exception => null
    }
  }
}

trait WordNetSolrConstants extends WordNetSynsetId with org.babysherlock.Constants {
  val FIELD_SYNSET_ID = "synsetid"
  val FIELD_POS = "pos"
  val FIELD_GLOSS = "gloss"
  val FIELD_WORDS = "words"

  val FIELD_TEXT = "text"
  val FIELD_WORDTEXT = "wordtext"
}

object WordNetConstants extends WordNetSynsetId with org.babysherlock.Constants




