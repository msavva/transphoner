package org.babysherlock.util

import dispatch._, Defaults._
import org.json.simple.JSONValue
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.File


/**
 * Web utils
 * @author Angel Chang
 */
object WebUtils {
  val defaultTimeout = 30 seconds

  def loadJson(path: String, params: Map[String,String] = Map.empty, timeout: Duration = defaultTimeout): Option[Object] = {
    val result = loadString(path, params, timeout)
    result.map(
      s => JSONValue.parse(s)
    )
  }

  def loadBytes(path: String, params: Map[String,String] = Map.empty, timeout: Duration = defaultTimeout): Option[Array[Byte]] = {
    // Check if file
    if (IOUtils.isReadableFileWithData(path))  {
      Some(File(path).toByteArray())
    } else {
      val json = url(path) <<? params
      val resultFuture = Http(json OK as.Bytes).option
      Await.result(resultFuture, timeout)
    }
  }

  def loadString(path: String, params: Map[String,String] = Map.empty, timeout: Duration = defaultTimeout): Option[String] = {
    // Check if file
    if (IOUtils.isReadableFileWithData(path) && params.isEmpty)  {
      val file = File(path)
      file.safeSlurp()
    } else {
      val req = url(path) <<? params
      val resultFuture = Http(req OK as.String).option
      Await.result(resultFuture, timeout)
    }
  }
}
