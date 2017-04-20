import Monoid.Monoid
import akka.actor.Actor.Receive
import akka.actor.{ActorRef, Cancellable}

import scala.concurrent.duration.Duration

case class NewM(newActor:ActorRef, isRoot:Boolean)
case class FailM(removeActor:ActorRef)
case class UpdateM[A](updateActor:ActorRef, weight:A, level:Int, parent:ActorRef)
case class WeightM[A](weight:A)

class GAPNode[A](monoid:Monoid[A]){
  // helper functions

  var table:Map[ActorRef, Tuple3[The_Status, Int, A]] = Map.empty
  var utils_use:GAPUtil[A] = new GAPUtil(monoid:Monoid[A])

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