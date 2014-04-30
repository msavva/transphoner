package org.babysherlock.transphoner.eval

import org.babysherlock.transphoner.{WeightedLevenshteinSimilarity, Constants}
import org.babysherlock.util._
import au.com.bytecode.opencsv.CSVWriter
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.JavaConversions._
import scala.collection.immutable.MapProxy

/**
 * Analysis of MTurk Results
 * @author Angel Chang
 */
object AnalyzeMturkResults extends App {
  val mturkResultsDir = MTurkResultAnalysis.mturkResultsDir
  def appendDir(s: String) = {
    val ext = if (s.endsWith("bz2") || s.endsWith("yml")) "" else ".yml.bz2"
    mturkResultsDir + s + ext
  }
  //val files = Seq("phonerails-db-2013-08-28","phonerails-db-2013-08-28_noKeywords")
  val initialFiles = Seq(
    "phonerails-db-2013-08-29_Keywords", "phonerails-db-2013-08-30_Keywords","phonerails-db-2013-09-06_manualKeywords","phonerails-db-2013-09-07_manualKeywords",
    "phonerails-db-2013-08-29_noKeywords","phonerails-db-2013-08-30_noKeywords","phonerails-db-2013-09-06_noKeywords",
    "phonerails-db-2013-08-29_transphonerKeywords", "phonerails-db-2013-08-30_transphonerKeywords","phonerails-db-2013-09-06_transphonerKeywords","phonerails-db-2013-09-07_transphonerKeywords",
    "phonerails-db-2013-08-30_randomKeywords","phonerails-db-2013-08-30_randomKeywords2","phonerails-db-2013-09-06_randomKeywords")
  val followupFiles = Seq("phonerails-db-2013-09-03_followup")
  val followupSurveyFiles = Seq("phonerails-db-2013-09-06_withinSubjects")
  val turkerResults1 = MTurkResultAnalysis.processMTurkResults(
    initialFiles.map( s => appendDir(s)) ++ followupFiles.map( s => appendDir(s)),
    followupSurveyFiles.map( s => appendDir(s) ),
    mturkResultsDir + "phonerails-db")

  // Test with fixed time learning
  val initialFilesFixedTime = Seq(
    "phonerails-db-2013-08-26", "phonerails-db-2013-08-28",
    "phonerails-db-2013-08-28_noKeywords")
  val turkerResults2 = MTurkResultAnalysis.processMTurkResults(
    initialFilesFixedTime.map( s => appendDir(s)),
    mturkResultsDir + "phonerails-db-fixedtime")

  // Within subjects study
  val withinSubjectFiles = Seq(
    "phonerails-db-2013-09-06_withinSubjects")
  val turkerResults3 = MTurkResultAnalysis.processMTurkResults(
    withinSubjectFiles.map( s => appendDir(s)),
      mturkResultsDir + "phonerails-db-withinSubjects")

  // Check for any overlap turkers between the two set of studies
  val turkers1 = turkerResults1.map( mtr => mtr.workerId ).distinct
  val turkers2 = turkerResults2.map( mtr => mtr.workerId ).distinct
  val turkers3 = turkerResults3.map( mtr => mtr.workerId ).distinct
  val overlap = (turkers1.intersect(turkers2 ++ turkers3) ++ turkers2.intersect(turkers3)).distinct
  if (overlap.nonEmpty) {
    println("Duplicate turkers: " + overlap.mkString(","))
  }
}

object MTurkResultAnalysis {
  val mturkResultsDir = Constants.PHONERAILS_DIR + Seq("db", "backup","").mkString(File.separator)
  val turkerCSV = new CSVFile( mturkResultsDir + "phonerails-turkers.csv", includesHeader = true )
  val turkerInfo = turkerCSV.rows.map(
    r => MTurkInfo(r("workerId"), r("nlangs").toDouble)
  ).map( r => r.workerId -> r ).toMap

  // Acceptable alternatives for some of the responses
  val altCorrectWords = Map(
    "trousers" -> Set("pants"),
    "scissors" -> Set("scissor"),
    "quarrel" -> Set("argue"),
    "fly" -> Set("tofly"),
    "buy" -> Set("purchase"),
    "tell" -> Set("say"),
    "hairdresser" -> Set("stylist", "hair dresser"),
    "jackdaw (bird)" -> Set("jackdaw", "bird")
  )

  val dateFormat = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss z")
  val filterBadWorkers = true
  val posKeywords = Set("good", "nice", "love", "fun", "exciting", "informative", "enjoy", "enjoyed", "enjoyable", "interesting", "great")
  val negKeywords = Set("worst")
  val difficultKeywords = Set("tough", "difficult", "hard", "frustrating", "bad")

  // Convert from text to our scale of 1 to 5
  private val textToRatingMap = Map[String,Double](
    "moderately" -> 3.5,
    "very much" -> 5,
    "not at all" -> 1,
    "not very!" -> 2,
    "will try" -> 3,
    "unlikely" -> 1,
    "helpful" -> 4,
    "no" -> 1,
    "i liked it alot. it was fun." -> 5,
    "i felt pretty competent." -> 5,
    "i didn't really like the keywords because they didn't help me." -> 1,
    "probably" -> 4,
    "probably not" -> 2)

  def textToRating(s: String): Double = {
    try {
      val r = s.toDouble
      if (r > 10 && r < 100) {
        // Hmm, maybe they are on 1 to 100 scale, normalize to be between 1 and 5
        5*r/100
      } else r
    } catch {
      case e: NumberFormatException => {
        // Okay this wasn't an number lets see if we can convert it
        var text = s.toLowerCase
        var v = textToRatingMap.getOrElse( text, 0.0 )
        if (v == 0) {
          // take first sentence
          text = text.split("\\.").head
          v = textToRatingMap.getOrElse( text, 0.0 )
          if (v == 0) {
            // take first word
            text = text.split("\\s+").head
            v = textToRatingMap.getOrElse( text, 0.0 )
          }
        }
        v
      }
    }
  }

  def getMTurkResults(filenames: Seq[String]): Seq[MTurkResult] = {
    // Go through files and aggregate turker results
    val turkerResults = (for (filename <- filenames) yield {
      val mturkDb = new YamlMTurkDb(YamlDb(filename))
      val mturkWordsData = mturkDb.mturkAssignmentsData.filter( m => m.data.contains("words") )
      mturkWordsData.map(
        mturkAssignment => MTurkResult(mturkAssignment)
      )
    }).flatten

    turkerResults
  }

  def getFollowupSurveyMTurkResults(filenames: Seq[String]): Seq[MTurkFollowupSurvey] = {
    // Go through files and aggregate turker results
    val turkerResults = (for (filename <- filenames) yield {
      val mturkDb = new YamlMTurkDb(YamlDb(filename))
      val mturkFollowupSurveyData = mturkDb.mturkAssignmentsData.filter( m => m.data.contains("enjoyment") )
      mturkFollowupSurveyData.map(
        mturkAssignment => MTurkFollowupSurvey(mturkAssignment)
      )
    }).flatten

    turkerResults
  }

  // Takes YML file with mturk results and analyzes it
  private def processMTurkResults(exprStage: Int,
                          tasks: Map[String,YamlMTurkDb.MTurkTask],
                          turkerResults: Seq[MTurkResult],
                          followupSurvey: Seq[MTurkFollowupSurvey],
                          prevWorkerResults: Map[String, Seq[MTurkResult]],
                          takeDelta: Boolean = false,  // Take delta with respect to previous worker results
                          perWordOutput: CSVWriter,
                          perTurkerOutput: CSVWriter,
                          perTurkerScoresOutput: CSVWriter,
                          overallOutput: CSVWriter) {
    val followupSurveyByTurker = followupSurvey.groupBy( m => m.workerId ).mapValues( s => s.head )
    val hasConditions = turkerResults.exists( p => p.conditions.length > 1 )
    val keywordTypeFieldName = "keywordField"
    val confFieldNames = Seq("fl", "nl", "quizOnly"/*, "wordsFile"*/)
    val conditionFieldNames = if (hasConditions) {
      Seq("presentationOrder", "enjoyment", "competence", "keywordRating")
    } else {
      Seq()
    }
    val surveyFieldNames = if (hasConditions) {
      Seq("gender", "age", "nlangs", "timeRating", "comments")
    } else {
      Seq("gender", "age", "nlangs", "timeRating", "keywordRating", "comments")
    }
    val scoreFieldNames = MTurkScore.scoreFieldNames

    val surveyNumericFields = if (hasConditions) {
      Seq("age", "timeRating")
    } else {
      Seq("age", "timeRating", "keywordRating")
    }
    val surveyBooleanFields = Seq( ("gender", "Female"), ("gender", "Male") )

    if (exprStage == 1) {
      // first phase, save header
      val workerInfoFields = Seq("numLangs", "difficult", "turkerTaskOpinion", "workerId", "completedAt","notes")
      val perWordHeader = confFieldNames  ++
        Seq("keywordType", "phase", "quizType", "delayTime", "flword", "flwordInput", "nlword", "nlwordInput", "keyword") ++
        scoreFieldNames ++ conditionFieldNames ++ surveyFieldNames ++ workerInfoFields
      perWordOutput.writeNext(perWordHeader.toArray)

      val perTurkerHeader =
        confFieldNames  ++ Seq("keywordType", "phase", "quizType", "delayTime") ++
        scoreFieldNames ++ conditionFieldNames ++ surveyFieldNames ++ workerInfoFields ++
          (if (followupSurvey.nonEmpty) MTurkFollowupSurvey.fields else Seq())
      perTurkerOutput.writeNext(perTurkerHeader.toArray)

      val perTurkerScoresHeader = confFieldNames  ++ Seq("keywordType", "phase", "quizType", "delayTime") ++ scoreFieldNames ++
        conditionFieldNames ++ surveyFieldNames ++ workerInfoFields ++
        (if (followupSurvey.nonEmpty) MTurkFollowupSurvey.fields else Seq())
      perTurkerScoresOutput.writeNext(perTurkerScoresHeader.toArray)

      val overallFieldNames = conditionFieldNames ++ surveyNumericFields ++ surveyBooleanFields.map( f => f._2 )
      val overallHeader = confFieldNames ++ Seq("keywordType", "phase", "quizType", "workers", "completed", "incomplete", "delayTime") ++ scoreFieldNames ++ overallFieldNames
      overallOutput.writeNext(overallHeader.toArray)
    }

    // Identify turkers who did this task more than once or had some other bad results
    val dataByTurkerId = turkerResults.groupBy( mtr => mtr.workerId )
    val badTurkerInfo = dataByTurkerId.mapValues(
      rs => {
        if (rs.size == 1) {
          val x = rs.head
          val score = x.score( QuizPhaseType.all )
          val aveLearnTime = score.map( x => x.learnTime ).sum/score.size
          val aveAnswered = score.map( x => x.percentAnswered ).sum/score.size
          val aveScore = score.map( x => x.percentApproxCorrect ).sum/score.size
          if (aveAnswered < 0.27) "NotEnoughAnswers"
          else if (aveScore < 0.27) "LowScore"
          else if (aveLearnTime > 0 && aveLearnTime < 1.5) "LearnTimeTooShort"
          else ""
        } else if (rs.size > 1) {
          "DupWorker"
        } else "NoResults"
      }
    ) ++ Map(
      "A3W037AYBB91V4" -> "Confused", // This turker was confused and consistently memorized and responded with the keywords instead of the nlword
      "A2IBLAKBXPA6PQ" -> "Confused", // This turker was also a bit confused
      "A2CCL5NW0DNJN8" -> "DupWorker" // This turker was duplicate between sets of experiments
    )
    val allScores = ( for (mtr <- turkerResults) yield {
      val prevMtr = if (prevWorkerResults != null) {
        prevWorkerResults(mtr.workerId).head
      } else null

      val delayTime = if (prevMtr != null) {
        val delta = mtr.completedAt.getTime - prevMtr.completedAt.getTime
        val deltaHours = delta.toDouble/1000/3600
        deltaHours
      } else 0.0

      // Figure out keyword conditions from the configuration
      val confWithKeywordInfo =
        if (prevMtr != null) {
          // Check previous work case for keyword condition
          prevMtr.conf
        } else {
          // Check current conf
          mtr.conf
        }

      val useKeyword = confWithKeywordInfo.getOrElse("useKeywords", "false").toBoolean
      val baseKeywordType = if (useKeyword) {
        confWithKeywordInfo.get(keywordTypeFieldName).map( x => {
          x match {
            case "tgtString" => "manual"
            case _ => x
          }
        }).getOrElse( "manual" )
      } else "none"
      val defaultKeywordType = if (takeDelta) baseKeywordType + "-delta" else baseKeywordType

      // One row per word
      val curPerWordScores = mtr.perWordScores()
      val perWordScores = if (prevMtr != null && takeDelta) {
        // group by foreign word
        val prevPerWordScores = prevMtr.perWordScores()
        val prevPerWordScoresByForeignWord = prevPerWordScores.groupBy( _._2.flword )
        // Take delta
        curPerWordScores.map( x => {
          val (p1,word1,s1) = x
          val (p2,word2,s2) = prevPerWordScoresByForeignWord(word1.flword).head
          s1.zip(s2).map{
            case (mts1, mts2) => {
              if (mts1.quizType == mts2.quizType) {
                (p1, word1,
                  // Need to keep numOfWords so we can take average
                  mts1.quizType, mts1.toSeq.zip( mts2.toSeq ).zipWithIndex.map (
                    y => if (y._2 == 0) y._1._1 else (y._1._1 - y._1._2) ) )
              } else {
                throw new RuntimeException("Quiz types not matched!!!")
              }
            }
          }
        })
      } else {
        curPerWordScores.map( x => {
          val (p,word,s) = x
          s.map( mts =>
            (p, word,mts.quizType, mts.toSeq )
          )
        })
      }

      // Prepare data for and print
      val confFields = confFieldNames.map( f => mtr.conf.getOrElse(f,"") )
      val notes = badTurkerInfo(mtr.workerId)
      val surveyNumericBooleanFields =
          surveyNumericFields.map( f => mtr.survey.get(f).map(
            x => try { x.toDouble } catch { case e: NumberFormatException => 0 }
          ).getOrElse(0.0) ) ++
          surveyBooleanFields.map( f => mtr.survey.get(f._1).map( x => if (x == f._2) 1.0 else 0.0 ).getOrElse(0.0))
      val turkerComments = mtr.survey.getOrElse("comments","")
      val turkerCommentsWordCounts = turkerComments.toLowerCase.split("\\s+|\\p{Punct}+")
        .groupBy( x => x ).mapValues( x => x.size )
      val turkerCommentsWords = turkerCommentsWordCounts.keySet
      val turkerCommentPosNeg =
        if (posKeywords.intersect(turkerCommentsWords).nonEmpty) "1"
        else if (negKeywords.intersect(turkerCommentsWords).nonEmpty) "-1"
        else 0
      val turkerFoundItHard =
        if (difficultKeywords.intersect(turkerCommentsWords).nonEmpty) "1"
        else 0
      val turkerNLangs = turkerInfo.get(mtr.workerId).map( m => m.nLangs).getOrElse(0)
      val surveyFields = surveyFieldNames.map( f => mtr.survey.getOrElse(f,"")) ++
        Seq(turkerNLangs.toString, turkerFoundItHard.toString, turkerCommentPosNeg.toString,
          mtr.workerId, dateFormat.format(mtr.completedAt), notes)

      // One row per word
      val perWordScoreFields = for ( (phase, word, quizType, s ) <- perWordScores.flatten ) yield {
        val wordKeywordType = word.keywordCondition.map( x => if (takeDelta) x + "-delta" else x ).getOrElse(defaultKeywordType)
        val condition = mtr.condition(wordKeywordType).getOrElse(Map())
        val conditionFields = conditionFieldNames.map( f => condition.getOrElse(f,"") )
        val phaseQuizTypes = Seq( phase.toString, quizType.toString() )
        val scoreFieldsAsStrings = s.map( x => x.toString )
        val row = confFields ++
          Seq(wordKeywordType) ++
          phaseQuizTypes ++
          Seq(if (delayTime > 0) delayTime.toString else "") ++
          Seq(word.flword, word.flwordInput, word.nlword, word.nlwordInput, word.keyword) ++
          scoreFieldsAsStrings ++
          conditionFields ++
          surveyFields

        // Output row
        perWordOutput.writeNext(row.toArray)
        (wordKeywordType, phase, quizType, s)
      }

      // Aggregate scores across words
      val testTrainScoreFields = perWordScoreFields.groupBy( r => (r._1, r._2, r._3) ).map(
        kv => {
          val ss = kv._2.map( r => r._4 )
          val s = MTurkScore.aggregate( kv._1._3, ss )
          ( kv._1._1, kv._1._2, kv._1._3, s.toSeq )
        }
      ).toSeq

      val allScoreFields = perWordScoreFields.groupBy( r => (r._1, QuizPhaseType.all, r._3) ).map(
        kv => {
          val ss = kv._2.map( r => r._4 )
          val s = MTurkScore.aggregate( kv._1._3, ss )
          ( kv._1._1, kv._1._2, kv._1._3, s.toSeq )
        }
      ).toSeq

      val scoreFields = if (hasConditions) {
        // if multiple conditions, there is no real test/train phase
        allScoreFields
      } else {
        testTrainScoreFields ++ allScoreFields
      }

      // One row per scoreField type
      for ( (keywordType, phase, quizType, s) <- scoreFields) yield {
        val condition = mtr.condition(keywordType).getOrElse(Map())
        val conditionFields = conditionFieldNames.map( f => condition.getOrElse(f,"") )
        val phaseQuizTypes = Seq( phase.toString, quizType.toString() )
        val scoreFieldsAsStrings = s.map( x => x.toString )
        val row = confFields ++
          Seq(keywordType) ++
          phaseQuizTypes ++
          Seq(if (delayTime > 0) delayTime.toString else "") ++
          scoreFieldsAsStrings ++
          conditionFields ++
          surveyFields ++
          (if (followupSurvey.nonEmpty) {
            followupSurveyByTurker.get(mtr.workerId).map(
              x => MTurkFollowupSurvey.fields.map( f => x.getOrElse(f,""))).getOrElse(Seq())
           } else Seq())

        // Output row
        perTurkerScoresOutput.writeNext(row.toArray)
        if (phase == QuizPhaseType.all && quizType == QuizType.Combined) {
          perTurkerOutput.writeNext(row.toArray)
        }

        // Save line for global data
        (confFields ++ Seq(keywordType) ++ phaseQuizTypes,
          Seq(delayTime) ++ s ++ conditionFields.map( x => x.toDouble) ++ surveyNumericBooleanFields,
          notes, mtr.taskId)
      }
    } ).flatten

    val filteredScores = if (filterBadWorkers) {
      allScores.filter( x => x._3 == "" )
    } else {
      allScores
    }
//    val confsByTask = allScores.groupBy( x => x._4 ).mapValues( x => x.map( y => y._1 ).distinct )
//    confsByTask
    val tasksByConf = allScores.groupBy( s => s._1.mkString("-") )
      .mapValues( x => x.map( y => y._4 ).distinct.map( tid => tasks(tid) ) )
    val taskStatsByConf = tasksByConf.mapValues( s => s.map( t => Seq(t.completed.size, t.incomplete.filter(v => !turkerInfo.contains(v.workerId)).size) )
      .reduce( (a,b) => a.zip(b).map( y => y._1 + y._2 ) ) )
    val groupedScores = filteredScores.groupBy( s => s._1.mkString("-") ).mapValues( x => ( x.head._1, x.map( y => y._2 )) )
    val sortedGroupedKeys = groupedScores.keys.toSeq.sorted
    val sortedGroupedValues = sortedGroupedKeys.map( x => groupedScores(x) )
    for ( (confFields,groupScores) <- sortedGroupedValues) {
      val taskStats = taskStatsByConf(confFields.mkString("-"))
      val sums = groupScores.reduce( (a,b) => a.zip(b).map( x => x._1 + x._2 ))
      val aves = sums.map( x => x/groupScores.size )
      val row = (confFields ++ Seq(groupScores.size)  ++ taskStats ++ aves).map( x => x.toString )
      overallOutput.writeNext(row.toArray)
    }
    badTurkerInfo.filter( x => x._2.nonEmpty ).foreach(
      x => println("Bad turker: " + x._1 + " " + x._2)
    )
  }

  def processMTurkResults(filenames: Seq[String],
                          followupFilenames: Seq[String],
                          perWordOutputFilename: String,
                          perTurkerOutputFilename: String,
                          perTurkerScoresOutputFilename: String,
                          overallOutputFilename: String): Seq[MTurkResult] = {
    val perWordOutput = new CSVWriter(IOUtils.filePrintWriter(perWordOutputFilename))
    val perTurkerOutput = new CSVWriter(IOUtils.filePrintWriter(perTurkerOutputFilename))
    val perTurkerScoresOutput = new CSVWriter(IOUtils.filePrintWriter(perTurkerScoresOutputFilename))
    val overallOutput = new CSVWriter(IOUtils.filePrintWriter(overallOutputFilename))

    val turkerResults = getMTurkResults(filenames)
    val tasks = YamlMTurkDb.getMTurkTasks(filenames)
    val tasksMap = tasks.groupBy( t => t.taskId ).mapValues( s => s.head )

    val followupSurvey = getFollowupSurveyMTurkResults(followupFilenames)
    val (quizOnlyTurkerResults, learningTurkerResults) = turkerResults.partition(
      mtr => mtr.conf.getOrElse("quizOnly", "false").toBoolean )
    processMTurkResults(1, tasksMap, learningTurkerResults, followupSurvey,
      null, takeDelta = false, perWordOutput, perTurkerOutput, perTurkerScoresOutput, overallOutput)
    processMTurkResults(2, tasksMap, quizOnlyTurkerResults, followupSurvey,
      learningTurkerResults.groupBy( mtr => mtr.workerId ), takeDelta = false,
      perWordOutput, perTurkerOutput, perTurkerScoresOutput, overallOutput)
    processMTurkResults(2, tasksMap, quizOnlyTurkerResults, followupSurvey,
      learningTurkerResults.groupBy( mtr => mtr.workerId ), takeDelta = true,
      perWordOutput, perTurkerOutput, perTurkerScoresOutput, overallOutput)

    perWordOutput.close()
    perTurkerOutput.close()
    perTurkerScoresOutput.close()
    overallOutput.close()
    turkerResults
  }

  def processMTurkResults(filenames: Seq[String],
                          outputFilename: String): Seq[MTurkResult] = {
    processMTurkResults(filenames, Seq(), outputFilename)
  }

  def processMTurkResults(filenames: Seq[String],
                          followupSurveyFilenames: Seq[String],
                          outputFilename: String): Seq[MTurkResult] = {
    processMTurkResults(filenames,
      followupSurveyFilenames,
      outputFilename + ".words.summary.csv",
      outputFilename + ".turkers.allcombined.csv",
      outputFilename + ".turkers.summary.csv",
      outputFilename + ".summary.csv")
  }

  case class MTurkFollowupSurvey(workerId: String, fields: Map[String,String]) extends MapProxy[String,String] {
    override def self = fields
    def enjoyment = fields.getOrElse("enjoyment","")
    def competence = fields.getOrElse("competence","")
    def learnedAfter = fields.getOrElse("learnedAfter","")
    def learnMore = fields.getOrElse("learnMore","")
    def keywordLikeRating = fields.getOrElse("keywordLikeRating","")
    def keywordWantRating = fields.getOrElse("keywordWantRating","")
    def comments = fields.getOrElse("followupComments","")
  }

  object MTurkFollowupSurvey {
    val ratingFields = Seq("enjoyment", "competence", "learnMore", "keywordLikeRating", "keywordWantRating")
    val fields = ratingFields ++ Seq("learnedAfter", "followupComments")

    def apply(mturkAssignment: YamlMTurkDb.MTurkAssignment): MTurkFollowupSurvey = {
      var map = YamlDb.asStringMap(mturkAssignment.data)
      map = map.updated("followupComments", map.getOrElse("comments", ""))
      // make sure ratings fields are okay, and more non ratings into comments
      for (f <- ratingFields) {
        val fieldValue = map.getOrElse(f, "")
        if (fieldValue.nonEmpty) {
          val rating = textToRating(fieldValue)
          if (fieldValue.exists( c => c.isLetter )) {
            // Add this to the comments
            val comments = map("followupComments")
            val str = f + ": " + fieldValue
            map = map.updated("followupComments", (if (comments.nonEmpty) comments + "\n" + str else str) )
          }
          map = map.updated(f, rating.toString)
        }
      }
      new MTurkFollowupSurvey(mturkAssignment.workerId, map)
    }
  }

  case class MTurkWordResponse(fields: Map[String,String], keyword: String) extends MapProxy[String,String] {
    override def self = fields
    def keywordCondition = fields.get("condition").map(
      x => x match {
        case "tgtString" => "manual"
        case _ => x
      }
    )
    def nlword = fields.getOrElse("nlword","")
    def flword = fields.getOrElse("flword","")
    def nlwordInput = fields.getOrElse("nlwordInput","")
    def flwordInput = fields.getOrElse("flwordInput","")
  }

  case class MTurkInfo ( workerId: String, nLangs: Double )

  case class MTurkResult (
    taskId: String,
    workerId: String,
    completedAt: Date,
    conditions: IndexedSeq[Map[String,String]],
    survey: Map[String,String],
    wordSets: IndexedSeq[IndexedSeq[MTurkWordResponse]],
    conf: Map[String,String] = Map()
  ){
    def condition(keywordType: String) = {
      val k = keywordType match {
        case "manual" => "tgtString"
        case _ => keywordType
      }
      conditions.find( x => x.getOrElse("id","") == k )
    }

    def score(phase: QuizPhaseType.Value) = phase match {
      case QuizPhaseType.train => scoreTrain()
      case QuizPhaseType.test => scoreTest()
      case QuizPhaseType.all => scoreAll()
    }

    def scoreAll(): Seq[MTurkScore] = score(wordSets.flatten)
    def scoreTrain(): Seq[MTurkScore] = score(wordSets.head)
    def scoreTest(): Seq[MTurkScore] = score(wordSets.tail.flatten)

    def score(wordSet: IndexedSeq[MTurkWordResponse]): Seq[MTurkScore] = {
      val quizTypes = QuizType.values
      quizTypes.toSeq.map( x => score(wordSet, x.asInstanceOf[QuizType.Value]) )
    }

    def score(word: MTurkWordResponse): Seq[MTurkScore] = {
      score(IndexedSeq(word))
    }

    def perWordScores(): Seq[(QuizPhaseType.Value, MTurkWordResponse, Seq[MTurkScore])] = {
      wordSets.zipWithIndex.map( ws => {
        val q = if ( ws._2 == 0 ) QuizPhaseType.train else QuizPhaseType.test
        ws._1.map( w => (q, w, score(w)) )
      }).flatten
    }

    def perWordScores(quizType: QuizType.Value): Seq[(QuizPhaseType.Value,MTurkWordResponse,MTurkScore)] = {
      wordSets.zipWithIndex.map( ws => {
        val q = if ( ws._2 == 0 ) QuizPhaseType.train else QuizPhaseType.test
        ws._1.map( w => (q, w, score(w, quizType)) )
      }).flatten
    }

    private def score(word: MTurkWordResponse, quizType: QuizType.Value): MTurkScore = {
      score(IndexedSeq(word), quizType)
    }

    def score(wordSet: IndexedSeq[MTurkWordResponse], quizType: QuizType.Value): MTurkScore = {
      if (quizType == QuizType.Combined) {
        // add up the rest...
        val quizTypes = QuizType.values - QuizType.Combined
        val otherScores = quizTypes.toSeq.map( x => score_(wordSet, x.asInstanceOf[QuizType.Value]) )
        val numWords = otherScores.map( s => s.numWords ).sum
        val numAnswered = otherScores.map( s => s.numAnswered ).sum
        val numCorrect = otherScores.map( s => s.numCorrect ).sum
        val approxCorrect = otherScores.map( s => s.approxCorrect ).sum
        val keywordSim =  otherScores.map( s => s.totalKeywordSim ).sum
        val learnTime = otherScores.map( s => s.learnTime ).sum / otherScores.size
        MTurkScore( quizType, numWords, numAnswered, numCorrect, approxCorrect, keywordSim, learnTime )
      } else {
        score_(wordSet, quizType)
      }
    }

    private def score_(wordSet: IndexedSeq[MTurkWordResponse], quizType: QuizType.Value): MTurkScore = {
      val levSim = WeightedLevenshteinSimilarity[Char]()
      val filtered = wordSet.filter( w => w.contains( quizType.inputField ) )
      val numWords = filtered.size
      var numAnswered = 0
      var numCorrect = 0
      var approxCorrect = 0.0
      var totalKeywordSim = 0.0
      var totalLearnTime = 0.0
      for (w <- filtered) {
        val keyword = w.keyword
        val learnTime = w.getOrElse("learnTime", "0").trim().toDouble
        totalLearnTime = totalLearnTime + learnTime
        var input = w(quizType.inputField).trim().toLowerCase
        if (input.startsWith("to ")) input = input.substring(3).trim()
        var gold = w(quizType.goldField).trim().toLowerCase
        if (gold.startsWith("to ")) gold = gold.substring(3).trim()
        if (input.nonEmpty) {
          numAnswered = numAnswered + 1
          val correct = if (input == gold) true
          else {
            if (altCorrectWords.contains(gold) && altCorrectWords(gold).contains(input)) {
              true
            } else false
          }

          if (correct) {
            numCorrect = numCorrect + 1
            approxCorrect = approxCorrect + 1
          }  else {
            val sim = levSim.similarity(input, gold)
            val approx = if (sim < 0.5) 0 else sim
            approxCorrect = approxCorrect + approx
  //          println("Got " + input + ", " + gold + ", sim " + sim + ", credit " + approx)
          }
          val keywordSimRaw = levSim.similarity(input, keyword)
          val keywordSim = if (keywordSimRaw < 0.5) 0 else keywordSimRaw
          totalKeywordSim = totalKeywordSim + keywordSim
        }
      }
      MTurkScore(quizType, numWords, numAnswered,
        numCorrect, approxCorrect, totalKeywordSim, totalLearnTime/numWords)
    }
  }

  object MTurkResult {
    def apply(mturkAssignment: YamlMTurkDb.MTurkAssignment): MTurkResult = {
      val conditions =
        mturkAssignment.data.get("conditions").map(
          c => YamlDb.javaListAsStringMapSeq(c).zipWithIndex.map{ case (m,i) => m.updated( "presentationOrder", i.toString )}
        ).getOrElse(IndexedSeq())
      val conf = mturkAssignment.data.get("conf").map( m => YamlDb.javaMapAsStringMap( m ) ).getOrElse( Map() )
      MTurkResult(
        taskId = mturkAssignment.taskId,
        workerId = mturkAssignment.workerId,
        completedAt = mturkAssignment.completedAt,
        conditions = conditions,
        survey = YamlDb.javaMapAsStringMap(mturkAssignment.data("survey")),
        wordSets = mturkAssignment.data("words").asInstanceOf[java.util.List[Object]].map(
          ws => ws.asInstanceOf[java.util.List[Object]].map( w =>
          {
            val fields = YamlDb.javaMapAsStringMap(w)
            val keyword: String =
              if (!conf.getOrElse("quizOnly", "false").toBoolean) {
                if (fields.contains("keyword")) {
                  if (conf.getOrElse("useKeywords", "false").toBoolean) fields("keyword") else ""
                } else if (fields.contains("keywords")) {
                  val c = fields("condition")
                  val keywords = w.asInstanceOf[java.util.Map[String, Object]].get("keywords").asInstanceOf[java.util.List[String]].toIndexedSeq
                  conditions.find( x => x.getOrElse("id","") == c ).map( m => keywords(m("order").toInt) ).getOrElse("")
                }
                else ""
              } else ""
            MTurkWordResponse(fields, keyword)
          }
          ).toIndexedSeq
        ).toIndexedSeq,
        conf = conf
      )
    }
  }

  object QuizPhaseType extends Enumeration {
    val all, test, train = Value
  }

  object QuizType extends Enumeration {
    override def values = super.values.map( x => x.asInstanceOf[QuizType.Value])

    class Value(val inputField: String, val goldField: String) extends super.Val {
    }
    val Recognition = new Value("nlwordInput", "nlword")
    val Generation = new Value("flwordInput", "flword")
    val Combined = new Value("", "")
  }

  case class MTurkScore(
    quizType: QuizType.Value,
    numWords: Double,
    numAnswered: Double,
    numCorrect: Double, // Exact match correct
    approxCorrect: Double, // Approximately correct score
    totalKeywordSim: Double, // Similarity to keyword
    learnTime: Double // Average time spent learning the words
  ) {
    def percentCorrect = numCorrect.toDouble / numWords
    def percentApproxCorrect = approxCorrect.toDouble / numWords
    def percentAnswered = numAnswered.toDouble / numWords
    def aveKeyWordSim = totalKeywordSim.toDouble / numWords
    def toSeq: Seq[Double] =
      Seq[Number](numWords, numAnswered, percentAnswered, numCorrect, percentCorrect,
                  approxCorrect, percentApproxCorrect, totalKeywordSim, aveKeyWordSim, learnTime).map( s => s.doubleValue() )
  }

  object MTurkScore {
    // Order should match fields from MturkScore toSeq
    val scoreFieldNames = Seq("numWords", "numAnswered", "percAnswered", "numCorrect", "percCorrect",
      "numApproxCorrect", "percApproxCorrect", "totalKeywordSim", "aveKeywordSim", "learnTime")

    def apply(q: QuizType.Value, s: Seq[Double]) = {
      new MTurkScore(q, s(0), s(1), s(3), s(5), s(7), s(9))
    }

    def aggregate(q: QuizType.Value, ss: Seq[Seq[Double]]): MTurkScore = {
      var numWords = 0.0
      var numAnswered = 0.0
      var numCorrect = 0.0
      var approxCorrect = 0.0
      var totalKeywordSim = 0.0
      var totalLearnTime = 0.0
      for (s <- ss) {
        val mtr = MTurkScore(q, s)
        numWords = numWords + mtr.numWords
        numAnswered = numAnswered + mtr.numAnswered
        numCorrect = numCorrect + mtr.numCorrect
        approxCorrect = approxCorrect + mtr.approxCorrect
        totalLearnTime = totalLearnTime + mtr.learnTime*mtr.numWords
        totalKeywordSim = totalKeywordSim + mtr.totalKeywordSim
      }
      MTurkScore(q, numWords, numAnswered, numCorrect, approxCorrect, totalKeywordSim, totalLearnTime/numWords)
    }
  }
}