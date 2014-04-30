package models

import play.api.libs.ws.WS
import scala.concurrent.{ExecutionContext, Await}
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

/**
 * Image Search
 * @author Angel Chang
 */
object ImageSearch {
  def search(query: String, start: Int = 0, limit: Int = 1) = {
      val request = WS.url("https://ajax.googleapis.com/ajax/services/search/images")
        .withQueryString(
        ("v", "1.0"),
        ("rsz", limit.toString),
        ("safe", "active"),
        ("q", query),
        ("start", start.toString)
      )
      val responseFuture = request.get

      val resultFuture = responseFuture map { response =>
        response.status match {
          case 200 => Some(response.json)
          case _ => { println(response.body); None }
        }
      }

      val result = Await.result(resultFuture, 5 seconds)
      println(result)
      result.map( x => x.\("responseData").\("results")(0).\("tbUrl").as[String] )
  }
}
