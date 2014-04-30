package org.babysherlock.transphoner.eval

import org.babysherlock.transphoner.{SimilarityWithOption, TransPhoner, TransPhonerOptions, Constants}
import org.babysherlock.transphoner.app.{KeywordSuggester, Keywords}
import org.babysherlock.transphoner.dict.{Word, Language, WordConverter}
import org.babysherlock.transphoner.phonetics.{PhoneticWordsMatch, PhoneSimilarity}
import org.babysherlock.util.{LRUCache, IOUtils, CSVFile, BatchSampler}
import au.com.bytecode.opencsv.CSVWriter
import java.io.PrintWriter

/**
 * Generate keywords for evaluation
 * @author Angel Chang
 */
object PrintSuggestions extends App {
  // Original ellis.suggested.csv used for transphoner evaluation had
  //   AlineExact with exact weight of 1
  EvalSetGenerator.printSuggestedKeywords(Constants.EVALUATION_ELLIS_KEYWORDS_CSV, Constants.TRANSPHONER_DATA_DIR + "ellis2.suggested.csv",
    //  Evaluation.printSuggestedKeywords(Constants.EVALUATION_SLANGUAGE_CSV, Constants.TRANSPHONER_DATA_DIR + "slanguage.suggested.csv",
    Seq("uniform", "similarUWNPath",
      "transphoner", "transphoner.noOrtho", "transphoner.noSemanticSim", "transphoner.noImageability",
      "transphoner.phoneticOnly", "transphoner.orthographicOnly", "transphoner.semanticSimOnly"))
}

object PrintSuggestionsLineup extends App {
  val n = 20
  val langs = Seq("EN", "DE", "FR", "JA", "ZH")
  val sampler = new BatchSampler
  val wordConverter = new WordConverter(true,true)
  for (lang <- langs) {
    val srcLang = Language(lang)
    // Select long words
    val longWords = srcLang.words.filter( w => wordConverter.toSyllables(w).size > 2 )
    val words = sampler.sampleWithoutReplacement( longWords, n )
    EvalSetGenerator.printSuggestedKeywordsForLangs(lang, words.map( w => w.orthographies.head).toSeq,
      Constants.TRANSPHONER_DATA_DIR + lang + n + ".suggested.csv",
      "transphoner",
      //"transphoner.noSemanticSim",
      langs)
  }
}

/**
 * Generate keywords for evaluation
 */
object EvalSetGenerator {

  /** Pair of source word/lang along with a target word/lang (the manually generated keyword) **/
  case class EvalWord(srcLangId: String, tgtLangId: String,
                      srcString: String, tgtString: String,
                      gloss: String,
                      note: String) {
    lazy val srcLang = Language(srcLangId)
    lazy val tgtLang = Language(tgtLangId)
    lazy val srcWordPairs = if (srcString.nonEmpty) srcLang.toWordPairs(srcString) else Seq()
    lazy val tgtWordPairs = if (tgtString.nonEmpty) tgtLang.toWordPairs(tgtString) else Seq()
    lazy val srcWords = srcWordPairs.map( x => x._2 )
    lazy val tgtWords = tgtWordPairs.map( x => x._2 )
  }

  /** Read the set of EvalWords from a file */
  def getEvalWords(filename: String): Seq[EvalWord] = {
    val evalCsv = new CSVFile(filename, includesHeader = true)
    evalCsv.rows.filter(_.row.length > 1).map(f => {
      EvalWord(f("srcLang"), f("tgtLang"), f("srcString"), f("tgtString"), f("gloss"), f("note"))
    }).toSeq
  }

  /**
   * Runs transphoner on the words in the file
   */
  def runTransphone(filename: String,
              wordPenalty: Double = 0,
              infreqPenalty: Double = 0,
              phoneticWeight: Double = 1,
              imageabilityWeight: Double = 1,
              semanticSimilarityWeight: Double = 0,
              orthographicSimilarityWeight: Double = 0,
              syllabify: Boolean = true,
              ignoreTones: Boolean = true) {
    val evalWords = getEvalWords(filename)
    val options = TransPhonerOptions(
      source = null,
      target = null,
      unweightedPhoneSim = PhoneSimilarity.AlineExactSimilarity,
      phoneApproxName = "SimTrie",
      wordPenalty = wordPenalty, infreqPenalty = infreqPenalty,
      phoneticWeight = phoneticWeight,
      imageabilityWeight = imageabilityWeight,
      semanticSimilarityWeight = semanticSimilarityWeight,
      semanticSimilarityType = "wordnetJCS",
      //        semanticSimilarityType = "wordnetJCSJaccard",
      //        semanticSimilarityType = "bow",
      orthographicSimilarityWeight = orthographicSimilarityWeight,
      syllabify = syllabify, ignoreTones = ignoreTones, searchBeamSize = 200
    )
    transphoneEvalWords(evalWords, options)
  }

  /** Given a sequence of EvalWords, transphones them and produces an output file **/
  def transphoneEvalWords(evalWords: Seq[EvalWord],
                          transphonerOptions: TransPhonerOptions,
                          outputFilename: Option[String] = None): Seq[PhoneticWordsMatch] = {
    val output: PrintWriter = if (outputFilename.isDefined) IOUtils.filePrintWriter(outputFilename.get) else new PrintWriter(System.out)
    val transphonerCache = LRUCache[(String,String), TransPhoner](2)
    val results = evalWords.map(f => {
      val srcLang = f.srcLang
      val tgtLang = f.tgtLang
      val options = transphonerOptions.copy(source = srcLang, target = tgtLang)
      val transphoner = transphonerCache.getOrElse((srcLang.id, tgtLang.id))(new TransPhoner(options))
      val phoneSimWithOptions = new SimilarityWithOption(transphoner.getPhoneSimilarity)
      val srcWordPairs = f.srcWordPairs
      val tgtWordsPairs = f.tgtWordPairs
      if (srcWordPairs.forall(_._2 != null)) {
        val srcWords = srcWordPairs.map( _._2 )
        val result = transphoner.transphone(srcWords)
        output.println(srcWords.mkString(",") + " -> " + result.target.mkString(",")
          + " [" + tgtWordsPairs.mkString(",") + "]" + " GLOSS: " + f.gloss)
        if (result.alignment != null) {
          output.println(result.alignment.phoneAlignment.mkString(scorer = Some(phoneSimWithOptions), prefix = "   "))
          output.println(result.alignment.syllableAlignment.mkString(prefix = "   "))
          //          println(result.alignment.wordAlignment.mkString(prefix = "   "))
        }
        output.flush()
        result
      }
      else {
        output.println("FIXME: " + srcWordPairs.mkString(",") + " -> " + "????" + " [" + tgtWordsPairs.mkString(",") + "]")
        output.flush()
        null
      }
    })
    if (outputFilename.isDefined) {
      output.close()
    }
    results
  }

  /** Get default transphoner options to use for suggesting keywords */
  def getTransphonerOptions(filterMaxSyllabesPerWord: Int = -1) = {
    val filterRareWords = 25000
    val defaultOptions = TransPhonerOptions( source = null, target = null,
      phoneticWeight = 0.0, imageabilityWeight = 0.0, semanticSimilarityWeight = 0.0, orthographicSimilarityWeight = 0.0,
      initialMatchWeight = 2.0,
      filterRareWordRank = filterRareWords,
      filterMaxSyllablesPerWord = filterMaxSyllabesPerWord,
      filterSourceWord = true,
      syllabify = false, ignoreTones = true,
      semanticSimilarityType = "bow",
      searchBeamSize = 200
    )
    val options = defaultOptions.copy(
      phoneticWeight = 1.0, imageabilityWeight = 1.0, semanticSimilarityWeight = 1, orthographicSimilarityWeight = 0.5, languageModelWeight = 0.0)
    options
  }

  /**
   * Returns a list of suggested keyword
   * @param evalWords Evaluation words
   * @param suggesterType How keywords should be generated
   *  "uniform" - Pick words randomly
   *  "transphone" - Use transphoner settings
   * @param filterMaxSyllabesPerWord If > 0, filter out any words that has more the specified number of syllables.
   * @return Sequence of keywords
   */
  def getSuggestedKeywords(evalWords: Seq[EvalWord], suggesterType: String, filterMaxSyllabesPerWord: Int = -1): Seq[Keywords] = {
    val suggesterCache = LRUCache[(String,String), KeywordSuggester](2)
    val filterRareWords = 25000
    val defaultOptions = TransPhonerOptions( source = null, target = null,
      phoneticWeight = 0.0, imageabilityWeight = 0.0, semanticSimilarityWeight = 0.0, orthographicSimilarityWeight = 0.0,
      initialMatchWeight = 2.0,
      filterRareWordRank = filterRareWords,
      filterMaxSyllablesPerWord = filterMaxSyllabesPerWord,
      filterSourceWord = true,
      syllabify = false, ignoreTones = true,
      semanticSimilarityType = "bow",
      searchBeamSize = 200
    )
    val results = evalWords.map(f => {
      val suggester = suggesterType match {
        case "uniform" => suggesterCache.getOrElse((f.srcLang.id, f.tgtLang.id))(
          KeywordSuggester.uniformMostFrequent(f.srcLang, f.tgtLang, filterRareWords))
        case "similarUWNPath" => suggesterCache.getOrElse((f.srcLang.id, f.tgtLang.id))(
          KeywordSuggester.similarUWNPath(f.srcLang, f.tgtLang))
        case "transphoner" => {
          val options = defaultOptions.copy(
            phoneticWeight = 1.0, imageabilityWeight = 1.0, semanticSimilarityWeight = 1, orthographicSimilarityWeight = 0.5, languageModelWeight = 0.0)
          suggesterCache.getOrElse((f.srcLang.id, f.tgtLang.id))( KeywordSuggester.transphoner(f.srcLang, f.tgtLang, options) )
        }
        case "transphoner.noOrtho" => {
          val options = defaultOptions.copy(
            phoneticWeight = 1.0, imageabilityWeight = 1.0, semanticSimilarityWeight = 1, orthographicSimilarityWeight = 0.0)
          suggesterCache.getOrElse((f.srcLang.id, f.tgtLang.id))( KeywordSuggester.transphoner(f.srcLang, f.tgtLang, options) )
        }
        case "transphoner.noImageability" => {
          val options = defaultOptions.copy(
            phoneticWeight = 1.0, imageabilityWeight = 0, semanticSimilarityWeight = 1, orthographicSimilarityWeight = 0.5)
          suggesterCache.getOrElse((f.srcLang.id, f.tgtLang.id))( KeywordSuggester.transphoner(f.srcLang, f.tgtLang, options) )
        }
        case "transphoner.noSemanticSim" => {
          val options = defaultOptions.copy(
            phoneticWeight = 1.0, imageabilityWeight = 1, semanticSimilarityWeight = 0, orthographicSimilarityWeight = 0.5, languageModelWeight = 1.0)
          suggesterCache.getOrElse((f.srcLang.id, f.tgtLang.id))( KeywordSuggester.transphoner(f.srcLang, f.tgtLang, options) )
        }
        case "transphoner.orthographicOnly" => {
          val options = defaultOptions.copy(orthographicSimilarityWeight = 1.0)
          suggesterCache.getOrElse((f.srcLang.id, f.tgtLang.id))( KeywordSuggester.transphoner(f.srcLang, f.tgtLang, options) )
        }
        case "transphoner.phoneticOnly" => {
          val options = defaultOptions.copy(phoneticWeight = 1.0)
          suggesterCache.getOrElse((f.srcLang.id, f.tgtLang.id))( KeywordSuggester.transphoner(f.srcLang, f.tgtLang, options) )
        }
        case "transphoner.semanticSimOnly" => {
          val options = defaultOptions.copy(semanticSimilarityWeight = 1.0)
          suggesterCache.getOrElse((f.srcLang.id, f.tgtLang.id))( KeywordSuggester.transphoner(f.srcLang, f.tgtLang, options) )
        }
      }
      val suggestions = suggester.suggestKeywords( f.srcWords, 1 )
      val result = if (suggestions.nonEmpty) suggestions.head else Keywords(Seq(),0.0)
      println(suggesterType + ": " + f.srcWords.mkString(",") + " -> " + result.words.mkString(",")
        + " [" + f.tgtWordPairs.mkString(",") + "]" + " GLOSS: " + f.gloss)
      result
    })
    results
  }

  def printSuggestedKeywords(filename: String, outputFilename: String, suggestTypes: Seq[String]) {
    def wordToOrtho(w: Word): String = Option(w).map(y => y.orthographies.head).getOrElse("")
    def wordToIpa(w: Word): String = Option(w).map(y => "/" + y.ipa + "/" ).getOrElse("")
    def wordPairToString(wp: (String,Word)): String = wp._1 + ":" + wordToOrtho(wp._2).toString

    val evalWords = getEvalWords(filename)
    val allSuggested = for (suggestType <- suggestTypes) yield {
      (suggestType, getSuggestedKeywords(evalWords, suggestType).toIndexedSeq )
    }
    val output = new CSVWriter(IOUtils.filePrintWriter(outputFilename))
    val header = Seq("srcLang", "tgtLang", "srcString", "srcWords", "srcPron",
      "tgtString", "tgtWords", "tgtPron", "gloss", "notes") ++
      suggestTypes.flatMap( x => Seq(x + ".words", x + ".pron"))
    output.writeNext(header.toArray)
    for ((word,i) <- evalWords.zipWithIndex) {
      val suggested = for ((suggestType, s) <- allSuggested) yield {
        s(i)
      }
      val row = Seq(word.srcLangId, word.tgtLangId,
        word.srcString, word.srcWordPairs.map( wp => wordPairToString(wp) ).mkString(" "),
        word.srcWords.map( w =>  wordToIpa(w) ).mkString(" "),
        word.tgtString, word.tgtWordPairs.map( wp => wordPairToString(wp) ).mkString(" "),
        word.tgtWords.map( w =>  wordToIpa(w) ).mkString(" "),
        word.gloss, word.note) ++
        suggested.flatMap( x => Seq(x.words.map( w => wordToOrtho(w) ).mkString(" "),
          x.words.map( w => wordToIpa(w) ).mkString(" ")
        ))
      output.writeNext(row.toArray)
    }
    output.close()
  }

  def printSuggestedKeywordsForLangs(srcLangId: String, words: Seq[String], outputFilename: String,
                                     suggestType: String, targetLangs: Seq[String]) {
    def wordToOrtho(w: Word): String = Option(w).map(y => y.orthographies.head).getOrElse("")
    def wordToIpa(w: Word): String = Option(w).map(y => "/" + y.ipa + "/" ).getOrElse("")
    def wordPairToString(wp: (String,Word)): String = wp._1 + ":" + wordToOrtho(wp._2).toString

    val evalWords = words.map( x => new EvalWord(srcLangId = srcLangId, tgtLangId = "",
      srcString = x, tgtString = "", gloss = "", note = ""))
    val allSuggested = for (tgtLangId <- targetLangs) yield {
      val evalWordsForTarget = evalWords.map( x => x.copy( tgtLangId = tgtLangId ))
      val filterMaxSyllabesPerWord = if (srcLangId == tgtLangId) 2 else -1
      (tgtLangId, getSuggestedKeywords(evalWordsForTarget, suggestType, filterMaxSyllabesPerWord).toIndexedSeq )
    }
    val output = new CSVWriter(IOUtils.filePrintWriter(outputFilename))
    val header = Seq("srcLang", "srcString", "srcWords", "srcPron") ++
      targetLangs.flatMap( x => Seq(x + ".words", x + ".pron"))
    output.writeNext(header.toArray)
    for ((word,i) <- evalWords.zipWithIndex) {
      val suggested = for ((tgtLang, s) <- allSuggested) yield {
        s(i)
      }
      val row = Seq(word.srcLangId,
        word.srcString, word.srcWordPairs.map( wp => wordPairToString(wp) ).mkString(" "),
        word.srcWords.map( w =>  wordToIpa(w) ).mkString(" ")) ++
        suggested.flatMap( x => Seq(x.words.map( w => wordToOrtho(w) ).mkString(" "),
          x.words.map( w => wordToIpa(w) ).mkString(" ")
        ))
      output.writeNext(row.toArray)
    }
    output.close()
  }

}

object EvaluateEllis extends App {
  EvalSetGenerator.runTransphone(Constants.EVALUATION_ELLIS_KEYWORDS_CSV)
}

object EvaluateSlanguage extends App {
  EvalSetGenerator.runTransphone(Constants.EVALUATION_SLANGUAGE_CSV)
}

