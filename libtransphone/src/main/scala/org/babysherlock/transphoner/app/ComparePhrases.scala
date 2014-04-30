package org.babysherlock.transphoner.app

import org.babysherlock.transphoner.dict.Language
import org.babysherlock.transphoner.eval.EvalSetGenerator
import org.babysherlock.transphoner.{TransPhoner, Constants}
import org.babysherlock.util.{IOUtils, SoftLRUCache, CSVFile}
import au.com.bytecode.opencsv.CSVWriter

/**
 * Given a list of pairs of words
 *   Produces a list of similarities between them
 * @author Angel Chang
 */
object ComparePhrases extends App {
  val inputFilename = Constants.TRANSPHONER_DATA_DIR + "ellis.suggested.csv"
  val inputFile = new CSVFile(inputFilename, includesHeader = true)
  val outputFilename = Constants.TRANSPHONER_DATA_DIR + "ellis.suggested.comparison.csv"
  val outputFile = new CSVWriter(IOUtils.filePrintWriter(outputFilename))
  val conditions = Map(
    "manual" -> "tgtString" ,
    "uniform" -> "uniform.words",
    "transphoner" -> "transphoner.words",
    "original" -> "srcString"
  )
  // Allow for source word so we can do the proper comparison with ourselves
  val options = EvalSetGenerator.getTransphonerOptions().copy( filterSourceWord = false )
  val transphonerCache = SoftLRUCache[(String,String), TransPhoner](2)

  case class OutputScores(info: Array[String],
                          scores: Map[String,Double])
  val outputScores = new scala.collection.mutable.ArrayBuffer[OutputScores]()
  val scoreFields = new scala.collection.mutable.LinkedHashSet[String]()
  for (row <- inputFile.rows) {
    for ((condition, conditionField) <- conditions) {
      val srcLang = Language(row("srcLang"))
      val tgtLang = if (condition == "original") srcLang else Language(row("tgtLang"))
      val srcString = row("srcString")
      val tgtString = row(conditionField)
      val transphoner = transphonerCache.getOrElse((srcLang.id,tgtLang.id))(TransPhoner(options.copy(source = srcLang, target = tgtLang)))
      // Compute score using transphoner of the two languages
      val scores = transphoner.score(srcString, tgtString)

      val info = Array(condition, srcLang.id, tgtLang.id, srcString, tgtString, scores.score.toString)
      outputScores.append(OutputScores(info, scores.scoreBreakdown))
      scoreFields ++= scores.scoreBreakdown.keys

//      println("Score for " + srcString + " to " + tgtString + " (condition is " + condition + "): " + scores.score)
//      println(scores.scoreBreakdown.mkString("\n"))
//      println()
    }
  }
  // Write to output
  // Figure out header
  val finalScoreFields = scoreFields.toArray
  val header = Array("condition","srcLang","tgtLang","srcString","tgtString","score") ++ finalScoreFields
  outputFile.writeNext(header)
  for (s <- outputScores) {
    val orderedScores = finalScoreFields.map( x => s.scores.getOrElse(x,"").toString )
    val row = s.info ++ orderedScores
    outputFile.writeNext(row)
  }
  outputFile.close()
}
