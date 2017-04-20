import Monoid.Monoid
import akka.actor.ActorRef

class GAPUtil[A] (monoid: Monoid[A]) {
  def new_entry(newActor:ActorRef, isRoot:Boolean, table:Map[ActorRef, Tuple3[The_Status, Int, A]]) =
  {
    if(isRoot && table.isEmpty)
    {
      val addTuple:Tuple3[The_Status, Int, A] = Tuple3(Par(), 0, 0.asInstanceOf[A])
      var new_table = table + (newActor -> addTuple)
      new_table
    }
    else if(table.isEmpty)
    {
      val addTuple:Tuple3[The_Status, Int, A] = Tuple3(Self(), getLevel(newActor, table), 0.asInstanceOf[A])
      var new_table = table + (newActor -> addTuple)
      new_table
    }
    else
    {
      table.get(newActor) match {
        case Some((curr_string, level, weight)) =>
          table   // return the original table simply, no modification made
        case None =>
          val adder:Tuple3[The_Status, Int, A] = Tuple3(Peer(), getLevel(newActor, table), 0.asInstanceOf[A])
          var new_table = table + (newActor -> adder)
          new_table
      }
    }
  }

  def getLevel(curr_actor:ActorRef, table:Map[ActorRef, Tuple3[The_Status, Int, A]]) = {
    0 // for now
  }

  def remove_entry(removeActor:ActorRef, table:Map[ActorRef, Tuple3[The_Status, Int, A]]) = {
    table.get(removeActor) match {
      case Some((curr_status, level, weight)) =>
        var new_table = table - removeActor
        new_table
      case None => table  // just return the original table without changes
    }
  }

  def update_entry(updateActor:ActorRef, weight:A, level:Int, parent:ActorRef, table:Map[ActorRef, Tuple3[The_Status, Int, A]]) = {
    if(table.get(updateActor).isEmpty)
      new_entry(updateActor, false, table)
    table.get(parent).get._1 match {
      case Self() =>
        val addEntry:Tuple3[The_Status, Int, A] = Tuple3(Child(), level, weight)
        var new_table = table + (updateActor -> addEntry)
        new_table
      case Child() =>
        val addEntry:Tuple3[The_Status, Int, A] = Tuple3(Peer(), level, weight)
        var new_table = table + (updateActor -> addEntry)
        new_table
      case Peer() => table  // return the original table without much modification
    }
  }

  def handle_weight(weight:A, table:Map[ActorRef, Tuple3[The_Status, Int, A]]) = {
    // empty for now
    table   // just return this for now
  }
}
