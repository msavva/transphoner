package controllers

import play.api.mvc._
import play.api.libs.json.Json._

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index(""))
  }

  def toJsonError(errors: Any) = {
    toJson(Map("status" -> "ERROR", "message" -> errors.toString()))
  }

  def toJsonOk[T](results: T)(implicit tjs : play.api.libs.json.Writes[T]) = {
    toJson(Map("status" -> toJson("OK"), "results" -> toJson(results)))
  }

  def toJsonOk[S,T](request: S, results: T)(implicit tjs1 : play.api.libs.json.Writes[S], tjs2 : play.api.libs.json.Writes[T]) = {
    toJson(Map("status" -> toJson("OK"),
               "request" -> toJson(request),
               "results" -> toJson(results)))
  }
}