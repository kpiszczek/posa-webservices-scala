package controllers

import scala.util.{ Try, Success, Failure }

import play.api._
import play.api.mvc._
import play.api.mvc.BodyParsers.parse

import models._

case class MissingParameterException(param: String) extends Exception(param) {
  override def toString = s"missing parameter: ${param}"
}
case class TooShortException(param: String, length: Int) extends Exception(param) {
  override def toString = s"parameter ${param} should be at least ${length} characters long"
}
case class NotANumberException(param: String) extends Exception(param) {
  override def toString = s"parameter ${param} must be a number"
}

object Application extends Controller {
  val VIDEO_ADDED = "Video added."

  var videos: List[Video] = Nil

  def get = Action {
    Ok(videos.map((v: Video) => s"${v.name} : ${v.url}").mkString("\n"))
  }

  def post = Action(parse.urlFormEncoded) { implicit request =>
    val body = request.body
    val tryName = checkParam(body, "name", checkLength("name", 1))
    val tryUrl = checkParam(body, "url", checkLength("url", 10))
    val tryDuration = checkParam(body, "duration", checkNumber("duration"))

    // transforming parameters to Try[Video]
    val result = for {
      name <- tryName
      url <- tryUrl
      duration <- tryDuration
    } yield {
      Video(name, url, duration)
    }

    result match {
      case Success(video) => {
        videos = video :: videos
        Ok(VIDEO_ADDED)
      }
      case _: Failure[_] =>
        BadRequest(getErrorMessage(List(tryName, tryUrl, tryDuration)))
    }
  }

  private def getParam(params: Map[String, Seq[String]], name: String): Try[String] = {
    val s = params.get(name).flatMap(_.headOption)
    s match {
      case None => Failure(MissingParameterException(name))
      case Some(paramValue) => Success(paramValue)
    }
  }

  private def checkParam[A](params: Map[String, Seq[String]], name: String, validator: String => Try[A]) =
    getParam(params, name).flatMap(validator)

  private def checkLength(name: String, minimalLength: Int): String => Try[String] = (s: String) =>
    if (s.length >= minimalLength) Success(s)
    else Failure(TooShortException(name, minimalLength))

  private def checkNumber(name: String): String => Try[Long] = (s: String) =>
    try { Success(s.toLong) }
    catch { case _: Throwable => Failure(NotANumberException(name)) }

  private def getErrorMessage(params: Seq[Try[Any]]): String = {
    params.map(_ match {
      case Failure(exception) => exception.toString + "\n"
      case Success(_) => ""
    }).mkString("")
  }
}