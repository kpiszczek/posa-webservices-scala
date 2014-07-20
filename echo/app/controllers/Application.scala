package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {

  def echo = Action { implicit request =>
    val msg = request.queryString.get("msg")
    // msg is an Option of Seq of Strings
    // we are transforming the head of sequence into Option
    // so we can handle missing paramenter case
    Ok(msg.flatMap(_.headOption).getOrElse(""))
  }
}