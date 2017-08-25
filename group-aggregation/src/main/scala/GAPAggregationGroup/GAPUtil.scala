import Monoid.{IntAddition, Monoid}
import akka.actor.{ActorRef, ActorSystem, Props}

class GAPUtil[A] (monoid: Monoid[A]) {
  var use_helpers:GAPHelper[A] = new GAPHelper(monoid:Monoid[A])

  def new_entry(newActor:ActorRef, secondActor:ActorRef, isRoot:Boolean, table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]] =
  {
    if(isRoot && table.isEmpty)
    {
      val addTuple:Table_Info[A] = new Table_Info[A](Par(), Some(0), monoid.id)
      var new_table = table + (newActor -> addTuple)
      val addSecond:Table_Info[A] = new Table_Info[A](Self(), Some(1), monoid.id)
      new_table = new_table + (secondActor -> addSecond)
      new_table
    }
    else if(table.isEmpty)
    {
      val addTuple:Table_Info[A] = new Table_Info[A](Self(), Some(1), monoid.id)
      var new_table = table + (newActor -> addTuple)
      new_table
    }
    else
    {
      table.get(newActor) match {
        case Some(table_Info) =>
          table   // return the original table simply, no modification made
        case None =>
          val adder:Table_Info[A] = new Table_Info[A](Peer(), None, monoid.id)
          var new_table = table + (newActor -> adder)
          new_table
      }
    }
  }

  // util function added for testing purposes to make things easier
  def setStatus(table:Map[ActorRef, Table_Info[A]], modify_actor:ActorRef, new_status:The_Status):Map[ActorRef, Table_Info[A]] = {
    val adder:Table_Info[A] = new Table_Info[A](new_status, table.get(modify_actor).get.get_level(), table.get(modify_actor).get.get_weight())
    var new_table = table + (modify_actor -> adder)
    new_table
  }

  // util function added for testing purposes to make things easier
  def setLevel(table:Map[ActorRef, Table_Info[A]], modify_actor:ActorRef, new_level:Option[Int]):Map[ActorRef, Table_Info[A]] = {
    val adder:Table_Info[A] = new Table_Info[A](table.get(modify_actor).get.get_status(), new_level, table.get(modify_actor).get.get_weight())
    var new_table = table + (modify_actor -> adder)
    new_table
  }

  def getNumParent(table:Map[ActorRef, Table_Info[A]]):Int = {
    var count:Int = 0
    for(value <- table.keys)
    {
      if(table.get(value).get.get_status() == Par())
        count += 1
    }
    count
  }

  def getNumSelf(table:Map[ActorRef, Table_Info[A]]):Int = {
    var count:Int = 0
    for(value <- table.keys)
    {
      if(table.get(value).get.get_status() == Self())
        count += 1
    }
    count
  }

  // gets the level of the actor
  def getLevel(table:Map[ActorRef, Table_Info[A]]):Option[Int] = {
    if(table.size < 2)
      return None
    Some(use_helpers.get_minimum(table).get + 1)
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

  // get's the actor's self value
  def getSelf(table:Map[ActorRef, Table_Info[A]]):Option[ActorRef] = {
    for(value <- table.keys) {
      if (table.get(value).get.get_status() == Self())
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

  // make sure everything is satisfied - helper function for testing purposes
  def everything_works(table:Map[ActorRef, Table_Info[A]]):Boolean = {
    var count_parent:Int = 0
    var count_self:Int = 0
    var parent_level:Int = 0
    var self_level:Int = 0
    var helper:GAPHelper[A] = new GAPHelper(monoid:Monoid[A])
    for(value <- table.keys)
    {
        if(table.get(value).get.get_status() == Self())
          count_self += 1
        else if(table.get(value).get.get_status() == Par())
          count_parent += 1
    }

    if(count_self > 1 || count_parent > 1)
      return false

    val minimum:Int = helper.get_minimum(table).get
    for(value <- table.keys)
    {
        if(table.get(value).get.get_status() == Self())
          self_level = table.get(value).get.get_level().get
        else if(table.get(value).get.get_status() == Par())
          parent_level = table.get(value).get.get_level().get
    }
    if(parent_level != minimum)
      return false
    else if(self_level != minimum + 1)
      return false
    else
      return true
  }

  def update_entry(updateActor:ActorRef, secondActor:ActorRef, weight:A, level:Int, parent:ActorRef, table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]] = {
    if(table.get(updateActor).isEmpty)
      new_entry(updateActor, secondActor, false, table)
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

      // second - make sure only one parent - if not, then convert the other status to peer/child
      var second_table:Map[ActorRef, Table_Info[A]] = use_helpers.confirm_one_parent(first_table)

      // next - make sure that the actor with self has level of minimum + 1
      var third_table:Map[ActorRef, Table_Info[A]] = use_helpers.handle_self_level(second_table)

      // fourth - make sure only one self - if not, then convert the other status to peer/child
      var final_table:Map[ActorRef, Table_Info[A]] = use_helpers.confirm_one_self(third_table)

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
