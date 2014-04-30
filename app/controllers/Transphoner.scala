package controllers

import org.babysherlock.transphoner.dict.Word
import org.babysherlock.transphoner.phonetics.{PhoneticApproximator, StressType, Phone}
import org.babysherlock.transphoner.semantics.SemanticSimilarity
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._
import play.api.libs.json._
import models._
import scala.Some

object Transphoner extends Controller {
  // JSON writers - make sure that writes for embedded classes are declared container classes
  implicit val wordWrites: Writes[Word] = Writes { word: Word =>
  JsObject( Seq(
    ("word", JsString(word.nativeWord)),
    ("ipa", JsString(word.ipa))
  )) }
  implicit val stressTypeWrites: Writes[StressType.Value] = Writes { s: StressType.Value =>
    JsString( s.toString )
  }
  implicit val transphoneScoreWrites = Json.writes[TransphoneScore]
  implicit val transphoneClusterScoreWrites = Json.writes[TransphoneClusterScore]
  implicit val transphonerResultWrites = Json.writes[TransphonerResult]

  implicit val soramimiWorldWrites = Json.writes[SoramimiWord]
  implicit val soramimiPhraseSuggestionWrites = Json.writes[SoramimiPhraseSuggestion]
  implicit val soramimiPhraseWrites = Json.writes[SoramimiPhrase]
  implicit val soramimiResultWrites = Json.writes[SoramimiResult]

  implicit val phoneWrites: Writes[Phone] = Writes { phone: Phone =>
    JsString( phone.toString )
  }
  implicit val pronDiffWrites: Writes[PronunciationDifference] = Writes { pronDiff: PronunciationDifference =>
      JsObject( Seq(
          ("ipa1", JsString(pronDiff.pron1.toString())),
          ("ipa2", JsString(pronDiff.pron2.toString())),
          ("diff", JsString(pronDiff.diff.toString))
        ))
  }
  implicit val lexicalizationWrites = Json.writes[Lexicalization]
  implicit val synsetDetailsWrites = Json.writes[SynsetDetails]
  implicit val wordDetailsWrites = Json.writes[WordDetails]
  implicit val transphonerLookupResultWrites = Json.writes[TransphonerLookupResult]

  val transphonerForm = Form(
    mapping(
      "input" -> nonEmptyText,
      "inputLang" -> nonEmptyText,
      "outputLang" -> nonEmptyText,
      "" -> mapping(
        "phoneSim" -> optional[String](nonEmptyText),
        "phoneApprox" -> optional[String](nonEmptyText),
        "wordPenalty" -> optional[Double](bigDecimal.transform( x => x.doubleValue(), y => BigDecimal(y) )),
        "infreqPenalty" -> optional[Double](bigDecimal.transform( x => x.doubleValue(), y => BigDecimal(y) )),
        "phoneticWeight" -> optional[Double](bigDecimal.transform( x => x.doubleValue(), y => BigDecimal(y) )),
        "imageabilityWeight" -> optional[Double](bigDecimal.transform( x => x.doubleValue(), y => BigDecimal(y) )),
        "semanticSimilarityWeight" -> optional[Double](bigDecimal.transform( x => x.doubleValue(), y => BigDecimal(y) )),
        "semanticSimilarity" -> optional[String](nonEmptyText),
        "orthographicSimilarityWeight" -> optional[Double](bigDecimal.transform( x => x.doubleValue(), y => BigDecimal(y) )),
        "initialMatchWeight" -> optional[Double](bigDecimal.transform( x => x.doubleValue(), y => BigDecimal(y) )),
        "languageModelWeight" -> optional[Double](bigDecimal.transform( x => x.doubleValue(), y => BigDecimal(y) )),
        "filterAmbiguousWord" -> optional[Boolean](boolean),
        "filterRareWordCount" -> optional[Int](number),
        "filterRareWordRank" -> optional[Int](number),
        "filterMaxSyllablesPerWord" -> optional[Int](number),
        "filterSourceWord" -> optional[Boolean](boolean),
        "syllabify" -> optional[Boolean](boolean),
        "ignoreTones" -> optional[Boolean](boolean)
      )(TransphonerOptionsParams.apply)(TransphonerOptionsParams.unapply),
      "showClusters" -> optional[Boolean](boolean),
      "nrows" -> optional[Int](number),
      "searchBeamSize" -> optional[Int](number),
      "target" -> optional[String](text)
    )(TransphonerParams.apply)(TransphonerParams.unapply)
  )

  val defaultTransphonerForm = transphonerForm.fill(
    new TransphonerParams("", "EN", "EN",
      new TransphonerOptionsParams(
        Some("AlineExactSimilarity"), Some(PhoneticApproximator.defaultName),
        Option(0.0), Option(0.0), Option(1.0), Option(0.0), Option(0.0), Some(SemanticSimilarity.defaultSemanticSimilarityType),
        Option(0.0), Option(1.0), Option(0.0), Some(false), Option(-1), Option(-1), Option(-1), Some(false), Some(false), Some(false)),
      Some(false), Option(10), None, None ))

  val transphonerLookupForm = Form(
    mapping(
      "input" -> nonEmptyText,
      "inputLang" -> nonEmptyText,
      "phoneSim" -> nonEmptyText
    )(TransphonerLookupParams.apply)(TransphonerLookupParams.unapply)
  )

  val defaultTransphonerLookupForm = transphonerLookupForm.fill(
    new TransphonerLookupParams("", "EN", "AlineExactSimilarity"))

  val noTransphonerResults = TransphonerResult("")
  val noTransphonerLineupResults = TransphonerLineupResult("")
  val noLookupResults = TransphonerLookupResult("")
  val noCompareResults = TransphonerCompareResult("")

  val soramimiForm = Form(
    mapping(
      "topic" -> nonEmptyText,
      "input" -> nonEmptyText,
      "inputLang" -> nonEmptyText,
      "outputLang" -> nonEmptyText
    )(SoramimiParams.apply)(SoramimiParams.unapply)
  )
  val defaultSoramimiForm = soramimiForm.fill(new SoramimiParams(
    "food", "", "EN", "EN"
  ))
  val noSoramimiResults = SoramimiResult("")

  def index = Action {
    Ok(views.html.transphoner(noTransphonerResults, defaultTransphonerForm))
  }

  // Soramimi - Topic based homophonic transformation
  def soramimi = Action { implicit request =>
    val filled = defaultSoramimiForm.bindFromRequest
    filled.fold(
      errors =>  {
        render {
          case Accepts.Html() => BadRequest(views.html.soramimi(noSoramimiResults, errors))
          case Accepts.Json() => BadRequest(Application.toJsonError(errors))
        }
      },
      params => {
        val results = Transphoners.soramimi(params)
        render {
          case Accepts.Html() => Ok(views.html.soramimi(results, filled))
          case Accepts.Json() => Ok(Application.toJsonOk(results))
        }
      }
    )
  }

  // TransPhone a single word - given one word in language1, find a similar sequence of words in language2
  def transphone = Action { implicit request =>
    // TODO: handle errors from nested options
    val filled = defaultTransphonerForm.bindFromRequest
    filled.fold(
      errors =>  {
        render {
          case Accepts.Html() => BadRequest(views.html.transphoner(noTransphonerResults, errors))
          case Accepts.Json() => BadRequest(Application.toJsonError(errors))
        }
      },
      params => {
        val results = Transphoners.transphone(params)
        render {
          case Accepts.Html() => Ok(views.html.transphoner(results, filled))
          case Accepts.Json() => Ok(Application.toJsonOk(results))
        }
      }
    )
  }

  def lineup = Action { implicit request =>
  // TODO: handle errors from nested options
    //val filledTransphonerLineupForm = transphonerLineupForm.bindFromRequest
    // Make the outputLang a comma delimited string
    val outputLangs = request.queryString.getOrElse("outputLang", Seq()).mkString(",")
    val filled = defaultTransphonerForm.bindFromRequest
    filled.fold(
      errors =>  {
        render {
          case Accepts.Html() => BadRequest(views.html.transphonerLineup(noTransphonerLineupResults, errors))
        }
      },
      params => {
        val transphonerLineupParams = params.copy( outputLang = outputLangs )
        val filledWithMultiParams = filled.fill( transphonerLineupParams )
        val results = Transphoners.lineup(transphonerLineupParams)
        render {
          case Accepts.Html() => Ok(views.html.transphonerLineup(results, filledWithMultiParams))
        }
      }
    )
  }

  val uploadLineupForm = Form(
    mapping(
      "expandImages" -> boolean)(UploadLineupParams.apply)(UploadLineupParams.unapply)
  )

  def uploadLineup = Action(parse.multipartFormData) { implicit request =>
    request.body.file("lineup").map { lineup =>
      val results = Transphoners.readLineup(lineup.ref.file)
      val filled = uploadLineupForm.bindFromRequest(request.body.asFormUrlEncoded)
      Ok(views.html.transphonerLineupResults(results,filled.get))
    }.getOrElse {
      val results = TransphonerLineupResult("No lineup file")
      BadRequest(views.html.transphonerLineupResults(results,null))
    }
  }

  // Lookup a single word/phrase in our dictionary and provide information about the word
  def lookup = Action { implicit request =>
    val filled = defaultTransphonerLookupForm.bindFromRequest
    filled.fold(
      errors =>  {
        render {
          case Accepts.Html() => BadRequest(views.html.transphonerLookup(noLookupResults, errors))
          case Accepts.Json() => BadRequest(Application.toJsonError(errors))
        }
      },
      params => {
        val results = Transphoners.lookup(params)
        render {
          case Accepts.Html() => Ok(views.html.transphonerLookup(results, filled))
          case Accepts.Json() => Ok(Application.toJsonOk(results))
        }
      }
    )
  }

  // Compare two words/phrases and provide information on how closely they match
  def compare = Action { implicit request =>
    val filled = defaultTransphonerForm.bindFromRequest
    filled.fold(
      errors =>  {
        render {
          case Accepts.Html() => BadRequest(views.html.transphonerCompare(noCompareResults, errors))
          case Accepts.Json() => BadRequest(Application.toJsonError(errors))
        }
      },
      params => {
        val results = Transphoners.compare(params)
        render {
          case Accepts.Html() => Ok(views.html.transphonerCompare(results, filled))
//          case Accepts.Json() => Ok(Application.toJsonOk(results))
        }
      }
    )
  }

}