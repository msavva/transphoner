package models

import org.babysherlock.transphoner._
import org.babysherlock.transphoner.app.Soramimi
import org.babysherlock.transphoner.dict.{Language, Word}
import org.babysherlock.transphoner.imageability.Imageability
import org.babysherlock.transphoner.phonetics.{PhoneticApproximator, PhoneSimilarity, StressType, Phone}
import org.babysherlock.transphoner.semantics.SemanticSimilarity
import org.babysherlock.nlp.UWN
import org.babysherlock.util.{IOUtils, CSVRow, CSVFile}
import au.com.bytecode.opencsv.CSVWriter
import java.io.File
import scala.util.matching.Regex

/**
 *  Provides routines for using the Transphoner
 *  @author Angel Chang
 */
object Transphoners {
  def languageOptions: Seq[(String,String)] =
    Language.languages.toSeq.sorted.map( s => (s, Language.name(s) ) )
  def phoneSimOptions: Seq[(String,String)] =
    PhoneSimilarity.similarities.toSeq.map( s => (s,s) )
  def phoneApproxOptions: Seq[(String,String)] =
    PhoneticApproximator.names.toSeq.map( s => (s,s) )
  def semSimilarityOptions: Seq[(String,String)] =
    SemanticSimilarity.names.toSeq.sorted.map( s => (s,s) )

//  private val transphoners = LRUCache[(String,String), Language](10)
  val imagineability = Imageability.defaultImageability
  val uwn = UWN

  def getLanguage(lang: String) = Language(lang)

  def soramimi(params: SoramimiParams): SoramimiResult = {
    val sm = Soramimi(params.topic, params.inputLang, params.outputLang)
    val smResult = sm.transformOne(params.input)
    val result = for (phrase <- smResult.phrases) yield {
      val phraseStr = phrase.inputPhraseWords.mkString(" ")
      val suggestions = for (transform <- phrase.transformations) yield {
        var suggestion = Seq[SoramimiWord]()
        var prev = 0
        for (part <- transform.parts) {
          if (prev < part.wordIndexStart) {
            val w = phrase.inputPhraseWords.slice(prev, part.wordIndexStart).mkString(" ")
            suggestion = suggestion :+ SoramimiWord(w)
          }
          val old = phrase.inputPhraseWords.slice(part.wordIndexStart, part.wordIndexEnd).mkString(" ")
          suggestion = suggestion :+ SoramimiWord(old, part.transformation)
          prev = part.wordIndexEnd
        }
        if (prev < phrase.inputPhraseWords.size) {
          val w = phrase.inputPhraseWords.slice(prev, phrase.inputPhraseWords.size).mkString(" ")
          suggestion = suggestion :+ SoramimiWord(w)
        }
        SoramimiPhraseSuggestion(suggestion, transform.score)
      }
      SoramimiPhrase(phraseStr, suggestions)
    }
    SoramimiResult("", result)
  }

  def lookup(params: TransphonerLookupParams): TransphonerLookupResult =
  {
    val lang = getLanguage(params.inputLang)
    if (lang == null) {
      TransphonerLookupResult("Unsupported language " + params.inputLang)
    } else {
      val phoneSim = PhoneSimilarity(params.phoneSim)
      val phoneSeqSim = WeightedLevenshteinSimilarity[Phone](phoneSim)
      val pairs = lang.toWordPairs(params.input)
      val triples = pairs.map( p => {
        val pronDiff = if (p._2 != null) {
          val diff = p._2.mostDifferentPron(phoneSeqSim)
          PronunciationDifference(diff._1, diff._2, diff._3)
        } else null
        val synsets = if (p._2 != null) uwn.lookupSynsets( p._1, p._2.lang.uwnCode ).toSeq else Seq()
        val synsetDetails = synsets.map(
          s => {
            val id = s.getObject.getId
            val glosses = uwn.lookupGlosses(id).map( g => g.getObject.getTermStr ).toSeq
            val lexicalizations = uwn.lookupSynsetLexicalizations( id, Set("eng") )
              .map( x => Lexicalization(x.getObject.getTermStr, x.getObject.getTermLanguage, x.getWeight ) ).toSeq
            val sim = imagineability.senseImageability(id)
            SynsetDetails(id, glosses, lexicalizations, sim, s.getWeight)
          }
        )
        val im = if (p._2 != null) imagineability.imageability(p._2) else 0
        WordDetails(
          p._1, p._2,
          pronDiff,
          synsetDetails.toSeq,
          im)
      })
      if (pairs.forall( p => p._2 != null)) {
        TransphonerLookupResult("", triples)
      } else {
        val unknown = pairs.filter( p => p._2 == null).map( p => p._1 )
        TransphonerLookupResult("Unrecognized words: " + unknown.mkString(","), triples)
      }
    }
  }

  def transphone(params: TransphonerParams): TransphonerResult =
  {
    val src = getLanguage(params.inputLang)
    val tgt = getLanguage(params.outputLang)
    if (src == null) {
      TransphonerResult("Unsupported source language " + params.inputLang, params.input, params.inputLang,  params.outputLang)
    } else if (tgt == null) {
      TransphonerResult("Unsupported target language " + params.outputLang, params.input, params.inputLang,  params.outputLang)
    } else {
      val nrows = params.nrows.getOrElse(10)
      val pairs = src.toWordPairs(params.input)
      val words = pairs.map(p => p._2)
      val gloss = ""
      if (pairs.forall( p => p._2 != null)) {
        val options = toTransphonerOptions(src, tgt, nrows, params)
        val transphoner = TransPhoner(options)
        val output = transphoner.transphone(words, nrows)
        val clusters = if (params.showClusters.getOrElse(false)) transphoner.transphoneGrouped(words, nrows) else Seq()
        TransphonerResult("", params.input, params.inputLang, params.outputLang, words, gloss,
          output.map( p => new TransphoneScore(p.target,
            if (p.alignment != null) p.alignment.wordStresses.toIndexedSeq else null, p.score) ),
          clusters.map( p => new TransphoneClusterScore(p.target,
            if (p.alignment != null) p.alignment.wordStresses.toIndexedSeq else null, p.score) )
        )
      } else {
        val unknown = pairs.filter( p => p._2 == null).map( p => p._1 )
        TransphonerResult("Unrecognized words: " + unknown.mkString(","), params.input, params.inputLang,  params.outputLang)
      }
    }
  }


  def getGloss(text: String, lang: String): String = {
    Translator.translate(text, lang, "en").getOrElse("")
  }

  def getGloss(words: Seq[Word], lang: String): String = {
    val s = words.map( w => w.orthographies.toSeq.sorted.head ).mkString(" ")
    getGloss(s, lang)
  }

  def lineup(params: TransphonerParams): TransphonerLineupResult =
  {
    val langWordRegex = new Regex("([A-za-z]+):(.+)")

    val outputLangs = params.outputLang.split("\\s*,\\s*")
    val tgts = outputLangs.map( x => (x,getLanguage(x)) )
    if (tgts.exists( p => p._2 == null)) {
      TransphonerLineupResult("Unsupported target languages " + tgts.filter( p => p._2 == null).map( p => p._1 ).mkString(",") )
    } else {
      val nrows = params.nrows.getOrElse(1)
      val inputStrs = params.input.split("\\s*,\\s*")
      val inputs = inputStrs.map( x => x match {
        case langWordRegex(lang,w) => (w,lang.toUpperCase)
        case _ => (x, params.inputLang)
      })
      val results = for ( (input, inputLang) <- inputs) yield {
        val src = getLanguage(inputLang)
        if (src == null) {
          TransphonerResults("Unsupported source language " + inputLang, input, inputLang)
        } else {
          val pairs = src.toWordPairs(input)
          if (pairs.forall( p => p._2 != null)) {
            val words = pairs.map(p => p._2)
            val gloss = getGloss(input, inputLang)
            val resultsForLangs = for ( (outputLang,tgt) <- tgts) yield {
              val options = toTransphonerOptions(src, tgt, nrows, params)
              val transphoner = TransPhoner(options)
              val output = transphoner.transphone(words, nrows)
              val clusters = if (params.showClusters.getOrElse(false)) transphoner.transphoneGrouped(words, nrows) else Seq()
              TransphonerResult("", input, inputLang, outputLang, words, gloss,
                output.map( p => new TransphoneScore(p.target,
                  if (p.alignment != null) p.alignment.wordStresses.toIndexedSeq else null, p.score,
                  getGloss( p.target, outputLang) ) ),
                clusters.map( p => new TransphoneClusterScore(p.target,
                  if (p.alignment != null) p.alignment.wordStresses.toIndexedSeq else null, p.score) )
              )
            }
            TransphonerResults("", input, inputLang, gloss, resultsForLangs)
          } else {
            val unknown = pairs.filter( p => p._2 == null).map( p => p._1 )
            TransphonerResults("Unrecognized words: " + unknown.mkString(","), input, inputLang)
          }
        }
      }
      TransphonerLineupResult("", outputLangs, results)
    }
  }

  class SimpleWord(nativeWord: String, ipa: String) extends Word(nativeWord, null, Seq(ipa))

  def readLineup(file: java.io.File): TransphonerLineupResult =
  {
    def toWords(w: String, p: String): Seq[Word] = {
      val ws = w.split("\\s+").map( x =>
        x.replaceAll(":.*","")
      )
      val ps = p.split("\\s+").map( x =>
        if (x.startsWith("/") && x.endsWith("/")) x.substring(1, x.length-1) else x
      )
      val words = ws.zip(ps).map(
        x => new SimpleWord(x._1, x._2)
      )
      words
    }

    val csvfile = new CSVFile(file.getAbsolutePath, includesHeader = true)
    val header = csvfile.getHeader()
    val outputLangs = header.filter( w => w.endsWith(".words")).map( x => x.replace(".words", ""))
    val results = csvfile.map( row => {
      val csvrow = new CSVRow(row, csvfile)
      val input = csvrow("srcString")
      val inputLang = csvrow("srcLang")
      val inputWords = toWords(csvrow("srcWords"), csvrow("srcPron"))
      val inputGloss = csvrow.getOrElse("srcGloss", "")
      val transphones = outputLangs.map( outputLang => {
        val words = toWords(csvrow(outputLang + ".words"), csvrow(outputLang + ".pron"))
        val gloss = csvrow.getOrElse(outputLang + ".gloss", "")
        val transphoneScores = Seq(TransphoneScore(
          words = words,
          wordStresses = IndexedSeq.fill(words.size)(StressType.none),
          score = 0.0,
          gloss = gloss
        ))
        TransphonerResult(message = "", input = input, inputLang = inputLang, inputGloss = inputGloss, outputLang = outputLang,
          inputWords = inputWords, transphoneScores = transphoneScores
        )
      })
      TransphonerResults(message = "", input = input, inputLang = inputLang, inputGloss = inputGloss, transphones = transphones)
    }).toSeq
    TransphonerLineupResult("", outputLangs, results)
  }

  def augmentLineups() = {
    val n = 100
    val langs = Seq("EN", "DE", "FR", "JA", "ZH")
    val dir = Constants.TRANSPHONER_DATA_DIR + "keywords" + File.separator
    for (lang <- Seq("ZH")) {
      augmentLineup(
        dir + lang + n + ".suggested.csv",
        dir + "glosses" + File.separator + lang + n + ".suggested.csv")
    }

  }

  def augmentLineup(input: String, output: String)  =
  {
    val csvfile = new CSVFile(input, includesHeader = true)
    val header = csvfile.getHeader()
    val outputLangs = header.filter( w => w.endsWith(".words")).map( x => x.replace(".words", ""))
    val csvwriter = new CSVWriter(IOUtils.filePrintWriter(output))
    val outheader = header ++ Array("srcGloss") ++ outputLangs.map( x => x + ".gloss" )
    csvwriter.writeNext(outheader)
    val results = csvfile.map( row => {
      val csvrow = new CSVRow(row, csvfile)
      // Add glosses
      val input = csvrow("srcString")
      val inputLang = csvrow("srcLang")
      val inputGloss = getGloss(input, inputLang)
      val outputGlosses = outputLangs.map( outputLang => {
        val output = csvrow(outputLang + ".words")
        val outputGloss = getGloss(output, outputLang)
        outputGloss
      })
      var outrow = row ++ Array(inputGloss) ++ outputGlosses
      csvwriter.writeNext(outrow)
      outrow
    })
    csvwriter.close()
  }

  def compare(params: TransphonerParams): TransphonerCompareResult =
  {
    val src = getLanguage(params.inputLang)
    val tgt = getLanguage(params.outputLang)
    if (src == null) {
      TransphonerCompareResult("Unsupported source language " + params.inputLang)
    } else if (tgt == null) {
      TransphonerCompareResult("Unsupported target language " + params.outputLang)
    } else if (params.target.isEmpty) {
      TransphonerCompareResult("Comparison target not provided")
    } else {
      val nrows = params.nrows.getOrElse(10)
      val srcPairs = src.toWordPairs(params.input)
      val tgtPairs = tgt.toWordPairs(params.target.get)
      val srcAllKnown = srcPairs.forall( p => p._2 != null)
      val tgtAllKnown = tgtPairs.forall( p => p._2 != null)
      if (srcAllKnown && tgtAllKnown) {
        val options = toTransphonerOptions(src, tgt, nrows, params)
        val transphoner = TransPhoner(options)
        val sourceWords = srcPairs.map(p => p._2)
        val targetWords = tgtPairs.map(p => p._2)
        val output = transphoner.score(sourceWords, targetWords)
        val phoneSimWithOption = new SimilarityWithOption(options.phoneSim)
        TransphonerCompareResult("", sourceWords, targetWords, output, Some(phoneSimWithOption))
      } else {
        val unknownSrc = srcPairs.filter( p => p._2 == null).map( p => p._1 )
        val unknownTgt = tgtPairs.filter( p => p._2 == null).map( p => p._1 )
        TransphonerCompareResult(
          Seq("Unrecognized input words: " + unknownSrc.mkString(","),
              "Unrecognized target words: " + unknownTgt.mkString(",")).mkString(". "))
      }
    }
  }

  private def toTransphonerOptions(src: Language, tgt: Language, nrows: Int, params: TransphonerParams) = {
    val phoneSim = PhoneSimilarity(params.options.phoneSim.getOrElse("AlineExactSimilarity"))
    TransPhonerOptions(
      source = src, target = tgt,
      unweightedPhoneSim = phoneSim,
      phoneApproxName = params.options.phoneApprox.getOrElse(PhoneticApproximator.defaultName),
      wordPenalty = params.options.wordPenalty.getOrElse(0),
      infreqPenalty = params.options.infreqPenalty.getOrElse(0),
      phoneticWeight = params.options.phoneticWeight.getOrElse(0),
      imageabilityWeight = params.options.imageabilityWeight.getOrElse(0),
      semanticSimilarityWeight = params.options.semanticSimilarityWeight.getOrElse(0),
      semanticSimilarityType = params.options.semanticSimilarityType.getOrElse(SemanticSimilarity.defaultSemanticSimilarityType),
      orthographicSimilarityWeight = params.options.orthographicSimilarityWeight.getOrElse(0),
      initialMatchWeight = params.options.initialMatchWeight.getOrElse(1),
      languageModelWeight = params.options.languageModelWeight.getOrElse(0),
      filterAmbiguousWord = params.options.filterAmbiguousWord.getOrElse(false),
      filterRareWordCount = params.options.filterRareWordCount.getOrElse(-1),
      filterRareWordRank = params.options.filterRareWordRank.getOrElse(-1),
      filterMaxSyllablesPerWord = params.options.filterMaxSyllablesPerWord.getOrElse(-1),
      filterSourceWord = params.options.filterSourceWord.getOrElse(false),
      syllabify = params.options.syllabify.getOrElse(false),
      ignoreTones = params.options.ignoreTones.getOrElse(false),
      searchBeamSize = params.searchBeamSize.getOrElse(2*nrows)
    )
  }
}

case class TransphonerResult(message: String,
                             input: String = "",
                             inputLang: String = "",
                             outputLang: String = "",
                             inputWords: Seq[Word] = Seq(),
                             inputGloss: String = "",
                             // Best transphoners for this input
                             transphoneScores: Seq[TransphoneScore] = Seq(),
                             transphoneClusters: Seq[TransphoneClusterScore] = Seq())
case class TransphonerResults(message: String,
                              input: String = "",
                              inputLang: String = "",
                              inputGloss: String = "",
                              transphones: Seq[TransphonerResult] = Seq())
case class TransphonerLineupResult(message: String,
                                   outputLangs: Seq[String] = Seq(),
                                   // Transphones: sequence of results per inputword
                                   //              for each input word, we have one result per language
                                   transphones: Seq[TransphonerResults] = Seq())
case class UploadLineupParams(expandImages: Boolean)
case class TransphoneScore(words: Seq[Word],
                           wordStresses: IndexedSeq[StressType.Value],
                           score: Double,
                           gloss: String = "")
case class TransphoneClusterScore(words: Seq[Set[Word]], wordStresses: IndexedSeq[StressType.Value], score: Double)

/** Parameters for transphoning from one language to another */
case class TransphonerParams(
                              input: String,
                              inputLang: String,
                              outputLang: String,
                              options: TransphonerOptionsParams,
                              showClusters: Option[Boolean],
                              nrows: Option[Int],
                              searchBeamSize: Option[Int],
                              target: Option[String] = None
                             )

case class TransphonerOptionsParams(
                              phoneSim: Option[String],
                              phoneApprox: Option[String],
                              wordPenalty: Option[Double],
                              infreqPenalty: Option[Double],
                              phoneticWeight: Option[Double],
                              imageabilityWeight: Option[Double],
                              semanticSimilarityWeight: Option[Double],
                              semanticSimilarityType: Option[String],
                              orthographicSimilarityWeight: Option[Double],
                              initialMatchWeight: Option[Double],
                              languageModelWeight: Option[Double],
                              filterAmbiguousWord: Option[Boolean],
                              filterRareWordCount: Option[Int],
                              filterRareWordRank: Option[Int],
                              filterMaxSyllablesPerWord: Option[Int],
                              filterSourceWord: Option[Boolean],
                              syllabify: Option[Boolean],
                              ignoreTones: Option[Boolean]
                             )

case class TransphonerCompareResult(message: String,
                                    inputWords: Seq[Word] = Seq(),
                                    targetWords: Seq[Word] = Seq(),
                                    score: TransPhoneScore = null,
                                    phoneSim: Option[Similarity[Option[Phone]]] = None)


/** Parameters for looking up a phrase in one language */
case class TransphonerLookupParams(
                              input: String,
                              inputLang: String,
                              phoneSim: String
                              )

case class TransphonerLookupResult(message: String,
                                   words: Seq[WordDetails] = Seq())

// Additional details about the word
case class WordDetails(orthography: String,
                       word: Word,
                       mostDifferentPron: PronunciationDifference,
                       synsets: Seq[SynsetDetails],
                       imageability: Double = 0)

case class SynsetDetails(id: String,
                         glosses: Seq[String],
                         lexicalizations: Seq[Lexicalization],
                         imageability: Double = 0,
                         score: Double = 1)

case class PronunciationDifference( pron1: Seq[Phone], pron2: Seq[Phone], diff: Double)
case class Lexicalization( lex: String, lang: String, score: Double )

case class SoramimiWord(word: String, transform: String = null)
case class SoramimiPhraseSuggestion(words: Seq[SoramimiWord], score: Double)
case class SoramimiPhrase(phrase: String, suggestions: Seq[SoramimiPhraseSuggestion]) {
  def topSuggestion = suggestions.head
}
case class SoramimiParams(topic: String, input: String, inputLang: String, outputLang: String)
case class SoramimiResult(message: String, phrases: Seq[SoramimiPhrase] = null)
