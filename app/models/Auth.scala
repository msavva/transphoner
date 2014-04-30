package models

import play.api.libs.ws.WS
import scala.concurrent.{ExecutionContext, Await}
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import org.babysherlock.util.LRUCache

/**
 * Authentication methods
 * @author Angel Chang
 */
object Auth {
  val MS_TRANSLATE = "http://api.microsofttranslator.com"
  val BING_SERVICE = "bing"
  // Retrieve the two tokens below from: http://www.bing.com/dev/en-us/translator
  val BING_CLIENT_ID = "FILL ME IN!!!"
  val BING_CLIENT_SECRET = "FILL ME IN!!!"

  case class AuthToken(token: String, expires: Long) {
    def isExpired = System.currentTimeMillis() > expires
  }

  val authCache = new LRUCache[String,AuthToken](20)
  def authToken(service: String, scope: String): Option[AuthToken] = {
    val key = service + "-" + scope
    val cached = authCache.get(key)
    if (cached.isEmpty || cached.get.isExpired) {
      // Refetch
      val t = service match {
        case BING_SERVICE => bingAuthToken(scope)
      }
      if (t.nonEmpty) { authCache.put(key, t.get) }
      t
    } else {
      cached
    }
  }

  def bingAuthToken(scope: String): Option[AuthToken] = {
    val auth = bingauth(Seq(scope))
    if (auth.isDefined) {
      val x = auth.get
      val token = "Bearer " + x.\("access_token").as[String]
      val expiresin = x.\("expires_in").as[String].toInt  // in seconds
      val expires = System.currentTimeMillis() + expiresin*1000
      Some(AuthToken(token, expires))
    } else {
      None
    }
  }

  // Authenticates with microsoft - gets us a access token that can be used...
  // TODO: Get client_id and client_secret from somewhere...

  def bingauth(scope: Seq[String]) =  {
    val parameters = Map(
      "grant_type" -> Seq("client_credentials"),
      "client_id" -> Seq(BING_CLIENT_ID),
      "client_secret" -> Seq(BING_CLIENT_SECRET),
      "scope" -> scope
    )

    println("Fetching bing auth token for " + scope)
    val responseFuture = WS.url("https://datamarket.accesscontrol.windows.net/v2/OAuth2-13")
      .post(parameters)

    val resultFuture = responseFuture map { response =>
      response.status match {
        case 200 => Some(response.json)
        case _ => None
      }
    }

    val result = Await.result(resultFuture, 5 seconds)
    result
  }
}
