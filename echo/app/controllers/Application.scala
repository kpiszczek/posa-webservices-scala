package controllers

import play.api._
import play.api.mvc._
import play.api.mvc.BodyParsers.parse

object Application extends Controller {

  def index = Action {
  	Ok("")
  }

  def echo = Action(parse.urlFormEncoded) { implicit request =>
    val msg = request.body.get("msg")
    // msg is an Option of Seq of Strings
    // we are transforming the head of sequence into Option
    // so we can handle missing paramenter case
    Ok(msg.flatMap(_.headOption).getOrElse(""))
  }
}