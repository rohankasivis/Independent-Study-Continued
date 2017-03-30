import Group.Group
import akka.actor.{ActorRef, Cancellable}

import scala.concurrent.duration.Duration

class GAPNode[A](group:Group[A]) extends GAPNodeActors[A](group:Group[A]){
  // helper functions

  def getLevel(curr_actor:ActorRef) = {
    0 // for now
  }

  def new_entry(newActor:ActorRef, isRoot:Boolean) =
  {
    if(isRoot && table.isEmpty)
    {
      val addTuple:Tuple3[String, Int, A] = Tuple3("parent", 0, 0.asInstanceOf[A])
      table += (newActor -> addTuple)
    }
    else if(table.isEmpty)
    {
      val addTuple:Tuple3[String, Int, A] = Tuple3("self", getLevel(newActor), 0.asInstanceOf[A])
      table += (newActor -> addTuple)
    }
    else
    {
      table.get(newActor) match {
        case Some((curr_string, level, weight)) =>
          None
        case None =>
          val adder:Tuple3[String, Int, A] = Tuple3("peer", getLevel(newActor), 0.asInstanceOf[A])
          table += (newActor -> adder)
      }
    }
  }

  def remove_entry(removeActor:ActorRef) = {
    table.get(removeActor) match {
      case Some((curr_string, level, weight)) =>
        table = table.filterKeys(_ != removeActor)
      case None => None
    }
  }

  def update_entry(updateActor:ActorRef, weight:A, level:Int, parent:ActorRef) = {
    if(table.get(updateActor).isEmpty)
      new_entry(updateActor, false)
    table.get(parent).get._1 match {
      case "self" =>
        val addEntry:Tuple3[String, Int, A] = Tuple3("child", level, weight)
        table = table + (updateActor -> addEntry)
      case "child" =>
        val addEntry:Tuple3[String, Int, A] = Tuple3("peer", level, weight)
        table = table + (updateActor -> addEntry)
      case "peer" => None
    }
  }

  def handle_weight(weight:A) = {
    // empty for now
  }

  def receive: Receive = {
    case NewM(newActor, isRoot) =>
      new_entry(newActor, isRoot)

    case FailM(removeActor) =>
      remove_entry(removeActor)

    case WeightM(weightM) =>
      handle_weight(weightM.asInstanceOf[A])

    case UpdateM(updateActor, weight, level, parent) =>
      update_entry(updateActor, weight.asInstanceOf[A], level, parent)
  }
}