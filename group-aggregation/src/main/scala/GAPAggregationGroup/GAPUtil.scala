import Monoid.{IntAddition, Monoid}
import akka.actor.{ActorRef, ActorSystem, Props}

class GAPUtil[A] (monoid: Monoid[A]) {
  var use_helpers:GAPHelper[A] = new GAPHelper(monoid:Monoid[A])

  def new_entry(newActor:ActorRef, isRoot:Boolean, table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]] =
  {
    if(isRoot && table.isEmpty)
    {
      val addTuple:Table_Info[A] = new Table_Info[A](Par(), Some(0), monoid.id)
      var new_table = table + (newActor -> addTuple)
      val secondActor:ActorRef = ActorSystem("curr_system").actorOf(Props(new GAPNode[A](monoid)), name="root_node")
      val addSecond:Table_Info[A] = new Table_Info[A](Self(), Some(1), monoid.id)
      new_table = new_table + (secondActor -> addSecond)
      new_table
    }
    else if(table.isEmpty)
    {
      val addTuple:Table_Info[A] = new Table_Info[A](Self(), None, monoid.id)
      var new_table = table + (newActor -> addTuple)
      new_table
    }
    else
    {
      table.get(newActor) match {
        case Some((curr_string, level, weight)) =>
          table   // return the original table simply, no modification made
        case None =>
          val adder:Table_Info[A] = new Table_Info[A](Peer(), None, monoid.id)
          var new_table = table + (newActor -> adder)
          new_table
      }
    }
  }

  // gets the level of the actor
  def getLevel(table:Map[ActorRef, Table_Info[A]]):Option[Int] = {
    if(table.size < 2)
      return None
    Some(use_helpers.get_minimum(table) + 1)
  }

  // gets the actor's parent
  def getParent(table:Map[ActorRef, Table_Info[A]]):Option[ActorRef] = {
    if(table.size < 2)
      None
    for(value <- table.keys)
    {
      if(table.get(value).get.get_status() == Par())
        return Some(value)
    }
    None
  }

  // removes an entry from the table
  def remove_entry(removeActor:ActorRef, table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]] = {
    table.get(removeActor) match {
      case Some(table_Info) =>
        var new_table = table - removeActor
        new_table
      case None => table  // just return the original table without changes
    }
  }

  def update_entry(updateActor:ActorRef, weight:A, level:Int, parent:ActorRef, table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]] = {
    if(table.get(updateActor).isEmpty)
      new_entry(updateActor, false, table)
    else {
      if(table.get(parent).get.get_status() == Self())
      {
        val addEntry: Table_Info[A] = new Table_Info[A](Child(), Some(level), weight)
        var new_table = table + (updateActor -> addEntry)
        new_table
      }
      else
      {
        if(table.get(updateActor).get.get_status() == Child())
        {
          val addEntry: Table_Info[A] = new Table_Info[A](Peer(), Some(level), weight)
          var new_table = table + (updateActor -> addEntry)
          new_table
        }
        else {
          val addEntry: Table_Info[A] = new Table_Info[A](table.get(updateActor).get.get_status(), Some(level), weight)
          var new_table = table + (updateActor -> addEntry)
          new_table
        }
      }
    }
  }

  def handle_weight(weight:A, table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]] = {
    // empty for now
    table   // just return this for now
  }

  // the main carry forward function
  def restore_table_invariant(table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]] = {
    if(table.size == 1)
    {
      // in this case, make sure there is an entry self - if not, then fix this
      var final_table:Map[ActorRef, Table_Info[A]] = use_helpers.handle_single_row_table(table)

      // returns the result of the above call
      final_table
    }
    else if(table.size > 1)
    {
      // check the following conditions: each node has only one row, exactly one node has status self,
      // there exists a parent and it has the minimum level among all entries; parent's level is 1 greater than self

      // first - make sure the entry with minimum value has status parent
      var first_table:Map[ActorRef, Table_Info[A]] = use_helpers.parent_min_val(table)

      // next - make sure that the actor with self has level of minimum + 1
      var second_table:Map[ActorRef, Table_Info[A]] = use_helpers.handle_self_level(first_table)

      // third - make sure only one parent - if not, then convert the other status to peer/child
      var third_table:Map[ActorRef, Table_Info[A]] = use_helpers.confirm_one_parent(second_table)

      // fourth - make sure only one self - if not, then convert the other status to peer/child
      var fourth_table:Map[ActorRef, Table_Info[A]] = use_helpers.confirm_one_self(third_table)

      // fifth - make sure that no node occurs more than once
      var final_table:Map[ActorRef, Table_Info[A]] = use_helpers.confirm_no_repetition(fourth_table)

      // return the last table after all modifications have been made
      final_table
    }
    else
    {
      // no need to do anything then - simply return the table
      table
    }
  }
}
