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
  	// state can be restored either by replaying 
  	// note that in recover state we're receiving events stored
  	// via 'persist' method NOT the original commands
    case video: Video => state.videos = video :: state.videos
    // Also we can restore state in one go if we're provided with a snapshot of a state
    case SnapshotOffer(_, snapshot: StoreState) => state = snapshot
  }

  val receiveCommand: Receive = {
    case AddVideo(video) => persist(video) { v =>
      {	
      	// adding video to store
        state.videos = video :: state.videos
        // after successful video storage 
        // we're passing Stored message back to sender
        sender() ! Stored
      }
    }
    // when asked we're sending back list of videos from store
    case Videos => sender() ! state.videos
    // saving snapshot of current state
    // now akka can remove previous events from journal/db
    // and still be able restore current state successfully
    case "snap" => saveSnapshot(state)
  }
}