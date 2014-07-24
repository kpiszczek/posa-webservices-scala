package models

import akka.actor._
import akka.persistence._

// Events
case class Video(name: String, url: String, duration: Long)
case object Stored

// Commands
case class AddVideo(video: Video)
case object Videos

class StoreState {
  var videos: List[Video] = Nil
}

class VideoStore extends PersistentActor {
  var state = new StoreState

  def persistenceId: String = "video-store"

  val receiveRecover: Receive = {
    case video: Video => { state.videos = video :: state.videos }
    case SnapshotOffer(_, snapshot: StoreState) => state = snapshot
  }

  val receiveCommand: Receive = {
    case AddVideo(v) => persist(v) { v =>
      {
        state.videos = v :: state.videos
        sender() ! Stored
      }
    }
    case Videos => sender() ! state.videos
    case "snap" => saveSnapshot(state)
  }
}