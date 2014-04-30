package org.babysherlock.transphoner.semantics

import org.babysherlock.util.{IOUtils, CSVFile}
import org.babysherlock.transphoner.dict.{Language, Word}
import org.babysherlock.transphoner.{Similarity, Constants}

/**
 * Evaluation of semantic similarity
 * @author Angel Chang
 */
object SemanticSimilarityEvaluation extends App {
  val evalfile = Constants.TRANSPHONER_DATA_DIR + "wordsim353.csv"
  val outputfile = Constants.TRANSPHONER_DATA_DIR + "wordsim353.eval.csv"
  val evalsims = readWordSims(evalfile)

//  val simTypes = SemanticSimilarity.names.toSeq.sorted
  val simTypes = Seq("tfidfBow", "bow","wordnetJCS")

  val correlations = for (simType <- simTypes) yield {
    val r = evalSim(simType)
    println(simType + ": r is " + r)
    (simType, r)
  }
  IOUtils.saveList(outputfile, correlations.map( x => x._1 + "," + x._2))

  def evalSim(simType: String): Double = {
    val semanticSimilarity = SemanticSimilarity(simType)
    val r = evalSim(Language.EN, semanticSimilarity.wordSimilarity)
    r
  }

  def evalSim(language: Language, wordSim: Similarity[Word]): Double = {
    val predicted = getPredicted(language, wordSim)
    val humanSimRanks = simsToRanks(evalsims)
    val predSimRanks = simsToRanks(predicted)
    val h = humanSimRanks.toSeq.sortBy( x => x._1 )
    val p = predSimRanks.toSeq.sortBy( x => x._1 )
    val r = correlation(h.map( x => x._2), p.map( x => x._2 ))
    r
  }

  // Computes pearson correlation (cov(x,y)/sigma(x)sigma(y))
  def correlation(x: Seq[Double], y: Seq[Double]): Double = {
    val (xm,xstd) = meanstd(x)
    val (ym,ystd) = meanstd(y)
    val cov = x.zip(y).map( z => (z._1 - xm)*(z._2 - ym) ).sum/(x.size-1)
    cov/(xstd*ystd)
  }

  def meanstd(x: Seq[Double]) =  {
    val m = x.sum/x.size
    // Sample standard dev (divide by n-1)
    val v = x.map( x => math.pow(x - m,2) ).sum/(x.size - 1)
    val std = math.sqrt(v)
    (m,std)
  }

  def simsToRanks(sims: Seq[(String,String,Double)]): Map[String,Double] = {
    // Sort
    val sorted = sims.sortBy( s => -s._3 )
    // Add rank to sorted list
    val ranks = sorted.zipWithIndex.map( x => (x._1._1 + "-" + x._1._2, x._1._3, x._2+1))
    // For items that are tied, take average of ranks
    val grouped = ranks.groupBy( x => x._2 )
    val tied = grouped.mapValues( s => {
      val aveRank = s.map( x => x._3 ).sum.toDouble/s.size
      s.map( x => (x._1, x._2, aveRank))
    })
    tied.values.flatten.groupBy( f => f._1 ).mapValues( s => s.head._3 )
  }

  def getPredicted(language: Language, wordSim: Similarity[Word]) = {
    for ( (w1, w2, humanSim) <- evalsims ) yield {
      val word1 = language.lookup(w1)
      val word2 = language.lookup(w2)
      val sim = if (word1 != null && word2 != null) {
        wordSim.similarity(word1, word2)
        //println(word1 + "-" + word2 + ": predicted sim " + sim + ", human sim " + humanSim)
      } else {
        // Hmm, some words are not found in our dictionary
        if (word1 == null) println("Cannot find word: " + w1)
        if (word2 == null) println("Cannot find word: " + w2)
        0.0
      }
      (w1,w2,sim)
    }
  }

  def readWordSims(filename: String): Seq[(String,String,Double)] = {
    val csvFile = new CSVFile(filename, includesHeader = true)
    csvFile.map(
      row => (row(0), row(1), row(2).toDouble)
    ).toSeq
  }
}
