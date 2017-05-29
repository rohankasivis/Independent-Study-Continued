import Monoid.{IntAddition, Monoid}
import akka.actor.ActorRef
import akka.testkit.TestActorRef

class GAPUtil[A] (monoid: Monoid[A]) {
  def new_entry(newActor:ActorRef, isRoot:Boolean, table:Map[ActorRef, Tuple3[The_Status, Int, A]]) =
  {
    if(isRoot && table.isEmpty)
    {
      val monoid = new IntAddition
      val addTuple:Tuple3[The_Status, Int, A] = Tuple3(Par(), -1, 0.asInstanceOf[A])
      var new_table = table + (newActor -> addTuple)
      val secondAdd:ActorRef = TestActorRef(new GAPNode[Int](monoid))
      val addSecond:Tuple3[The_Status, Int, A] = Tuple3(Self(), 0, 0.asInstanceOf[A])
      new_table = new_table + (secondAdd -> addSecond)
      new_table
    }
    else if(table.isEmpty)
    {
      val addTuple:Tuple3[The_Status, Int, A] = Tuple3(Self(), 0, 0.asInstanceOf[A])
      var new_table = table + (newActor -> addTuple)
      new_table
    }
    else
    {
      table.get(newActor) match {
        case Some((curr_string, level, weight)) =>
          table   // return the original table simply, no modification made
        case None =>
          val adder:Tuple3[The_Status, Int, A] = Tuple3(Peer(), 0, 0.asInstanceOf[A])
          var new_table = table + (newActor -> adder)
          new_table
      }
    }
  }

  // gets the level of the actor
  def getLevel(curr_actor:ActorRef, table:Map[ActorRef, Tuple3[The_Status, Int, A]]):Int = {
    if(table.size < 2)
      return null
    get_minimum(table) + 1
  }

  // gets the actor's parent
  def getParent(table:Map[ActorRef, Tuple3[The_Status, Int, A]]):ActorRef = {
    if(table.size < 2)
      return null
    for(value <- table.keys)
    {
      if(table.get(value).get._1 == Par())
        return value
    }
    null
  }

  // removes an entry from the table
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

// ----------------Helper functions----------------------------------------------
  // gets the minimum value in a table
  def get_minimum(table:Map[ActorRef, Tuple3[The_Status, Int, A]]): Int = {
    var minimum:Int = Int.MaxValue
    for(value <- table.keys)
    {
      var curr_min:Int = table.get(value).get._2
      if(curr_min < minimum)
        minimum = curr_min
    }
    minimum
  }

  // this handles a table with only one row - makes sure that the only type class is self(), and if not, fixes that
  def handle_single_row_table(table:Map[ActorRef, Tuple3[The_Status, Int, A]]):Map[ActorRef, Tuple3[The_Status, Int, A]] = {
    for(value <- table.keys)
    {
      if(table.get(value).get._1 != Self())
      {
        // fix here by making status self
        val level = table.get(value).get._2
        val weight = table.get(value).get._3
        val addEntry: Tuple3[The_Status, Int, A] = Tuple3(Self(), level, weight)
        var new_table = table + (value -> addEntry)
        return new_table
      }
    }
    table
  }

  // a helper function which makes sure the parent has minimum level in the table
  def parent_min_val(table:Map[ActorRef, Tuple3[The_Status, Int, A]]):Map[ActorRef, Tuple3[The_Status, Int, A]] = {
    var minimum:Int = get_minimum(table)

    for(value <- table.keys)
    {
      if(table.get(value).get._2 == minimum)
      {
        // now we are at what should be the parent
        val the_weight:A = table.get(value).get._3
        if(table.get(value).get._1 != Par())
        {
          // then make this have a status of parent
          val addEntry: Tuple3[The_Status, Int, A] = Tuple3(Par(), minimum, the_weight)
          var new_table = table + (value -> addEntry)
          return new_table
        }
      }
    }
    table
  }

  // a helper function which makes sure that the level (minimum + 1) element has status self()
  def handle_self_level(table:Map[ActorRef, Tuple3[The_Status, Int, A]]):Map[ActorRef, Tuple3[The_Status, Int, A]]  = {
    var minimum:Int = get_minimum(table)
    var cond_two_satisfied:Boolean = false
    for(value <- table.keys)
    {
      if(table.get(value).get._1 == Self())
      {
        val curr_level:Int = table.get(value).get._2
        if(curr_level == minimum + 1)
          cond_two_satisfied = true
      }
    }

    // if cond_two is not satisfied, find minimum + 1 level in the table and make that self
    if(!cond_two_satisfied)
    {
      for(value <- table.keys)
      {
        if(table.get(value).get._2 == minimum + 1)
        {
          val the_weight:A = table.get(value).get._3
          val addEntry: Tuple3[The_Status, Int, A] = Tuple3(Self(), minimum + 1, the_weight)
          var new_table = table + (value -> addEntry)
          return new_table
        }
      }
    }
    table
  }

  // makes sure only one parent exists, and if more than one does, replace the other one with an appropriate status
  def confirm_one_parent(table:Map[ActorRef, Tuple3[The_Status, Int, A]]):Map[ActorRef, Tuple3[The_Status, Int, A]] = {
    var minimum:Int = get_minimum(table)
    var new_table = table
    for(value <- table.keys)
    {
      if(table.get(value).get._1 == Par())
      {
        // now check the level. if the level is the minimum, then its good.. otherwise an appropriate switch is necessary
        if(table.get(value).get._2 != minimum)
        {
          new_table = switch_element(value, new_table)
        }
      }
    }
    // return the modified table here
    new_table
  }

  // makes sure only one self exists, and if more than one does, replace the other ones with appropriate status
  def confirm_one_self(table:Map[ActorRef, Tuple3[The_Status, Int, A]]):Map[ActorRef, Tuple3[The_Status, Int, A]] = {
    var minimum:Int = get_minimum(table)
    var new_table = table
    for(value <- table.keys)
    {
      if(table.get(value).get._1 == Self())
      {
        // check the level and see if it = minimum + 1. make an appropriate switch if it does not
        if(table.get(value).get._2 != (minimum + 1))
        {
          new_table = switch_element(value, new_table)
        }
      }
    }
    // return the modified table here
    new_table
  }

  // make sure no elements are repeated twice
  def confirm_no_repetition(table:Map[ActorRef, Tuple3[The_Status, Int, A]]):Map[ActorRef, Tuple3[The_Status, Int, A]] = {
    // simply returns current table as of now, since this should not be a problem with maps?
    // may modify if necessary
    table
  }

  // switches the element as necessary and returns the modified table
  def switch_element(switcher:ActorRef, table:Map[ActorRef, Tuple3[The_Status, Int, A]]):Map[ActorRef, Tuple3[The_Status, Int, A]] = {
    // switch to peer for now.. see when to switch to child?
    val curr_level:Int = table.get(switcher).get._2
    val curr_weight:A = table.get(switcher).get._3
    val addEntry: Tuple3[The_Status, Int, A] = Tuple3(Peer(), curr_level, curr_weight)
    var new_table = table + (switcher -> addEntry)
    new_table
  }

  // the main carry forward function
  def restore_table_invariant(table:Map[ActorRef, Tuple3[The_Status, Int, A]]):Map[ActorRef, Tuple3[The_Status, Int, A]] = {
    if(table.size == 1)
    {
      // in this case, make sure there is an entry self - if not, then fix this
      var final_table:Map[ActorRef, Tuple3[The_Status, Int, A]] = handle_single_row_table(table)

      // returns the result of the above call
      final_table
    }
    else if(table.size > 1)
    {
      // check the following conditions: each node has only one row, exactly one node has status self,
      // there exists a parent and it has the minimum level among all entries; parent's level is 1 greater than self

      // first - make sure the entry with minimum value has status parent
      var first_table:Map[ActorRef, Tuple3[The_Status, Int, A]] = parent_min_val(table)

      // next - make sure that the actor with self has level of minimum + 1
      var second_table:Map[ActorRef, Tuple3[The_Status, Int, A]] = handle_self_level(first_table)

      // third - make sure only one parent - if not, then convert the other status to peer/child
      var third_table:Map[ActorRef, Tuple3[The_Status, Int, A]] = confirm_one_parent(second_table)

      // fourth - make sure only one self - if not, then convert the other status to peer/child
      var fourth_table:Map[ActorRef, Tuple3[The_Status, Int, A]] = confirm_one_self(third_table)

      // fifth - make sure that no node occurs more than once
      var final_table:Map[ActorRef, Tuple3[The_Status, Int, A]] = confirm_no_repetition(fourth_table)

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
