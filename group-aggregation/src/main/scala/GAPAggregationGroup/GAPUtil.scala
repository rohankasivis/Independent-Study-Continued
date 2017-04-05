import Monoid.Monoid
import akka.actor.ActorRef

class GAPUtil[A] (monoid: Monoid[A], var table:Map[ActorRef, Tuple3[The_Status, Int, A]]) {
  def new_entry(newActor:ActorRef, isRoot:Boolean) =
  {
    if(isRoot && table.isEmpty)
    {
      val addTuple:Tuple3[The_Status, Int, A] = Tuple3(Par(), 0, 0.asInstanceOf[A])
      table += (newActor -> addTuple)
    }
    else if(table.isEmpty)
    {
      val addTuple:Tuple3[The_Status, Int, A] = Tuple3(Self(), getLevel(newActor), 0.asInstanceOf[A])
      table += (newActor -> addTuple)
    }
    else
    {
      table.get(newActor) match {
        case Some((curr_string, level, weight)) =>
          None
        case None =>
          val adder:Tuple3[The_Status, Int, A] = Tuple3(Peer(), getLevel(newActor), 0.asInstanceOf[A])
          table += (newActor -> adder)
      }
    }
  }

  def getLevel(curr_actor:ActorRef) = {
    0 // for now
  }

  def remove_entry(removeActor:ActorRef) = {
    table.get(removeActor) match {
      case Some((curr_status, level, weight)) =>
        table = table.filterKeys(_ != removeActor)
      case None => None
    }
  }

  def update_entry(updateActor:ActorRef, weight:A, level:Int, parent:ActorRef) = {
    if(table.get(updateActor).isEmpty)
      new_entry(updateActor, false)
    table.get(parent).get._1.getId match {
      case 0 =>
        val addEntry:Tuple3[The_Status, Int, A] = Tuple3(Child(), level, weight)
        table += (updateActor -> addEntry)
      case 1 =>
        val addEntry:Tuple3[The_Status, Int, A] = Tuple3(Peer(), level, weight)
        table += (updateActor -> addEntry)
      case 3 => None
    }
  }

  def handle_weight(weight:A) = {
    // empty for now
  }
}
