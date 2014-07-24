import play.api._
import akka.actor._

import models._

object Global extends GlobalSettings {
  // creating videoStore actor on the start of the system
  override def onStart(app: Application) {
    import play.api.Play.current
    import play.api.libs.concurrent.Akka.system
    val videoStore = system.actorOf(Props[VideoStore], "video-store")
  }
}