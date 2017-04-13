import Monoid.Monoid
import akka.actor.{ActorRef, Cancellable}

import scala.concurrent.duration.Duration

class GAPNode[A](monoid:Monoid[A]) extends GAPNodeActors[A](monoid:Monoid[A]){
  // helper functions

  def receive: Receive = {
    case NewM(newActor, isRoot) =>
      table = utils_use.new_entry(newActor, isRoot, table)

    case FailM(removeActor) =>
      table = utils_use.remove_entry(removeActor, table)

    case WeightM(weightM) =>
      table = utils_use.handle_weight(weightM.asInstanceOf[A], table)

    case UpdateM(updateActor, weight, level, parent) =>
      table = utils_use.update_entry(updateActor, weight.asInstanceOf[A], level, parent, table)
  }
}