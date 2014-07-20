package controllers

import scala.util.{ Try, Success, Failure }

import play.api._
import play.api.mvc._

import models._

case class MissingParameterException(param: String) extends Exception(param)
case class BadParameterException(param: String) extends Exception(param)

object Application extends Controller {
  val VIDEO_ADDED = "Video added."

  var videos: List[Video] = Nil

  def get = Action {
    Ok(videos.map((v: Video) => s"${v.name} : ${v.url}").mkString("\n"))
  }

  def post = Action { implicit request =>
  	val queryString = request.queryString
  	val tryName = getName(queryString)
  	val tryUrl = getUrl(queryString)
  	val tryDuration = getDuration(queryString)

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

  private def getParam(queryString: Map[String, Seq[String]], param: String): Try[String] = {
  	val s = queryString.get(param).flatMap(_.headOption)
  	s match {
  	  case None => Failure(MissingParameterException(param))
  	  case Some(paramValue) => Success(paramValue)
  	}
  }

  private def getName(queryString: Map[String, Seq[String]]): Try[String] = {
  	val name = getParam(queryString, "name")
  	name match {
  	  case f: Failure[String] => f
  	  case Success(s) => 
  	    if (s.length < 1) Failure(new BadParameterException("name"))
  	    else Success(s)
  	}
  }

  private def getUrl(queryString: Map[String, Seq[String]]): Try[String] = {
  	val url = getParam(queryString, "url")
  	url match {
  	  case f: Failure[String] => f
  	  case Success(s) => 
  	    if (s.length < 10) Failure(new BadParameterException("url"))
  	    else Success(s)
  	}
  }

  private def getDuration(queryString: Map[String, Seq[String]]): Try[Long] = {
  	val duration = getParam(queryString, "duration")
  	duration match {
  	  // Failure[String] => Failure[Long]
  	  case Failure(ex) => Failure(ex)
  	  case Success(s) => try {
  	  	Success(s.toLong)
  	  } catch {
  	  	case e: Exception => Failure(new BadParameterException("duration"))
  	  }
  	}
  }

  private def getErrorMessage(params: Seq[Try[Any]]): String = {
  	params.map(_ match {
	    case Failure(exception) => exception.toString + "\n"
	    case Success(_) => ""
	  }).mkString("")
  }
}