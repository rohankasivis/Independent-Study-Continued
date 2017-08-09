import Monoid.Monoid
import akka.actor.{Actor, ActorRef, Cancellable}

import scala.concurrent.duration.Duration

case class NewM(newActor:ActorRef, secondActor:ActorRef, isRoot:Boolean)
case class FailM(removeActor:ActorRef)
case class UpdateM[A](updateActor:ActorRef, secondActor:ActorRef, weight:A, level:Int, parent:ActorRef)
case class WeightM[A](weight:A)

class GAPNode[A](monoid:Monoid[A]) extends Actor{
  // helper functions

  var table:Map[ActorRef, Table_Info[A]] = Map.empty
  var utils_use:GAPUtil[A] = new GAPUtil(monoid:Monoid[A])

  def receive: Receive = {
    case NewM(newActor, secondActor, isRoot) =>
      table = utils_use.new_entry(newActor, secondActor, isRoot, table)

    case FailM(removeActor) =>
      table = utils_use.remove_entry(removeActor, table)

    case WeightM(weightM) =>
      table = utils_use.handle_weight(weightM.asInstanceOf[A], table)

    case UpdateM(updateActor, secondActor, weight, level, parent) =>
      table = utils_use.update_entry(updateActor, secondActor, weight.asInstanceOf[A], level, parent, table)
  }
}