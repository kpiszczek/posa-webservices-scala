package models

import akka.actor._
import akka.persistence._

// Events
case class Video(name: String, url: String, duration: Long)
case object Stored

// Commands
case class AddVideo(video: Video)
case object Videos

class VideoStore extends PersistentActor {
  var state: List[Video] = Nil

  def persistenceId: String = "video-store"

  val receiveRecover: Receive = {
  	case video: Video => { state = video :: state }
  }

  val receiveCommand: Receive = {
  	case AddVideo(v) => persist(v){ v => {
  	  state = v :: state 
  	  sender() ! Stored 
  	}}
  	case Videos => sender() ! state
  	case "snap" => saveSnapshot(state)
  }
}