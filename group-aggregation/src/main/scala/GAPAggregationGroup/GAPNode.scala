import Monoid.Monoid
import akka.actor.{ActorRef, Cancellable}

import scala.concurrent.duration.Duration

class GAPNode[A](monoid:Monoid[A]) extends GAPNodeActors[A](monoid:Monoid[A]){
  // helper functions

  def receive: Receive = {
    case NewM(newActor, isRoot) =>
      utils_use.new_entry(newActor, isRoot)

    case FailM(removeActor) =>
      utils_use.remove_entry(removeActor)

    case WeightM(weightM) =>
      utils_use.handle_weight(weightM.asInstanceOf[A])

    case UpdateM(updateActor, weight, level, parent) =>
      utils_use.update_entry(updateActor, weight.asInstanceOf[A], level, parent)
  }
}