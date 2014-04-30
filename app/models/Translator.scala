package models

import play.api.libs.ws.WS
import scala.concurrent.{ExecutionContext, Await}
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

/**
 * Translate
 * @author Angel Chang
 */
object Translator {
  def translate(text: String, fromLang: String, toLang: String) = {
    def normalizeLang(lang: String) = if (lang == "ZH") "zh-CHS" else lang.toLowerCase
    val from = normalizeLang(fromLang)
    val to = normalizeLang(toLang)
    if (from == to) Option(text)
    else {
      val auth = Auth.authToken(Auth.BING_SERVICE, Auth.MS_TRANSLATE)
      if (auth.isDefined) {
        val token = auth.get.token
        val request = WS.url("http://api.microsofttranslator.com/v2/Http.svc/Translate")
          .withQueryString(
          ("text", text),
          ("from", from),
          ("to", to)
        ).withHeaders(("Authorization", token))
        val responseFuture = request.get

        val resultFuture = responseFuture map { response =>
          response.status match {
            case 200 => Some(response.xml.text)
            case _ => { println(response.body); None }
          }
        }

        val result = Await.result(resultFuture, 5 seconds)
        result
      } else {
        None
      }
    }
  }
}
