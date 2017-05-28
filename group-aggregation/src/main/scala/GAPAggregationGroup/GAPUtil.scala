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

  // gets the level of the actor
  def getLevel(curr_actor:ActorRef, table:Map[ActorRef, Tuple3[The_Status, Int, A]]):Int = {
    var minimum:Int = Int.MaxValue
    for(value <- table.keys)
    {
      var curr_min:Int = table.get(value).get._2
      if(curr_min < minimum)
        minimum = curr_min
    }
    return minimum + 1
  }

  // gets the actor's parent
  def getParent(table:Map[ActorRef, Tuple3[The_Status, Int, A]]):ActorRef = {
    var minimum:Int = Int.MaxValue
    if(table.size == 0 || table.size == 1)
      return null
    for(value <- table.keys)
    {
      var curr_min:Int = table.get(value).get._2
      if(curr_min < minimum)
        minimum = curr_min
    }

    for(value <- table.keys)
    {
      if(table.get(value).get._2 == minimum)
        return value
    }
    null
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
    else {
      if(table.get(parent).get._1 == Self())
      {
        val addEntry: Tuple3[The_Status, Int, A] = Tuple3(Child(), level, weight)
        print("here")
        var new_table = table + (updateActor -> addEntry)
        new_table
      }
      else
      {
        if(table.get(updateActor).get._1 == Child())
        {
          val addEntry: Tuple3[The_Status, Int, A] = Tuple3(Peer(), level, weight)
          var new_table = table + (updateActor -> addEntry)
          new_table
        }
        else {
          val addEntry: Tuple3[The_Status, Int, A] = Tuple3(table.get(updateActor).get._1, level, weight)
          var new_table = table + (updateActor -> addEntry)
          new_table
        }
      }
    }
  }

  def handle_weight(weight:A, table:Map[ActorRef, Tuple3[The_Status, Int, A]]) = {
    // empty for now
    table   // just return this for now
  }

  def restore_table_invariant() = {

  }
}
