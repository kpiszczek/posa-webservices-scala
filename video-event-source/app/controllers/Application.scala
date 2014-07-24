package controllers

import scala.util.{ Try, Success, Failure }
import scala.concurrent._
import scala.concurrent.duration._

import akka.actor._
import akka.util.Timeout
import akka.pattern.ask

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
  // akka boilerplate - start
  import play.api.Play.current
  import play.api.libs.concurrent.Akka.system
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val timeout: Timeout = Timeout(5.seconds)
  // akka boilerplate - end

  val VIDEO_ADDED = "Video added."

  // we're selecting 'video-store' actor (which has been created in Global.scala) 
  // from global akka system
  def videoStore = system.actorSelection("user/video-store")

  // When we're communicating with actor system all operations are asynchronous
  // We must provide a Future[Result] instead of pure Result
  def get = Action.async {
    // We are using "ask pattern" to receive list of videos from video-store.
    // The "?" method sends message to an actor and returns Future of response.
    // Message channels are untyped so we need to map response to expected type. 
    (videoStore ? Videos).mapTo[List[Video]].map(videos =>
      Ok(videos.map((v: Video) => s"${v.name} : ${v.url}").mkString("\n")))
  }

  // again we're using Action's async method so we will be returning Future[Result]
  def post = Action.async(parse.urlFormEncoded) { implicit request =>
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
      case Success(video) => 
        // Once again we're using 'ask' pattern.
        // We're sending AddVideo command to videoStore and mapping 
        // Future of message to Store event
        (videoStore ? AddVideo(video)).map {
          case Stored => Ok(VIDEO_ADDED)
          // in case when actor is not responding with expected message we assume that something went terribly wrong
          case _ => InternalServerError("Server error: unable to save video to store")
        }
      case _: Failure[_] => Future {
        // if any of paramters has was missing or faild validation we're
        // returning BadRequest result with body containing list of errors
        BadRequest(getErrorMessage(List(tryName, tryUrl, tryDuration)))
      }
    }
  }

  private def getParam(params: Map[String, Seq[String]], name: String): Try[String] = {
    val s = params.get(name).flatMap(_.headOption)
    s match {
      case None => Failure(MissingParameterException(name))
      case Some(paramValue) => Success(paramValue)
    }
  }

  private def checkParam[A](params: Map[String, Seq[String]], name: String, validator: String => Try[A]): Try[A] =
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