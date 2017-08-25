import Monoid.Monoid
import akka.actor.ActorRef

// a set of helper functions..
class GAPHelper [A] (monoid: Monoid[A]) {
  // gets the minimum value in a table
  def get_minimum(table:Map[ActorRef, Table_Info[A]]): Option[Int] = {
    var minimum:Int = Int.MaxValue
    var count_none:Int = 0
    for(value <- table.keys)
    {
      var curr_min:Option[Int] = table.get(value).get.get_level()
      curr_min match {
        case Some(value) => if(value < minimum)
                              minimum = value
        case None => count_none += 1
      }
    }
    if(count_none == table.size)
      return None
    else
      Some(minimum)
  }

  // this handles a table with only one row - makes sure that the only type class is self(), and if not, fixes that
  def handle_single_row_table(table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]] = {
    for(value <- table.keys)
    {
      if(table.get(value).get.get_status() != Self())
      {
        // fix here by making status self
        val level = table.get(value).get.get_level()
        val weight = table.get(value).get.get_weight()
        val addEntry: Table_Info[A] = new Table_Info[A](Self(), level, weight)
        var new_table = table + (value -> addEntry)
        return new_table
      }
    }
    table
  }

  // a helper function which makes sure the parent has minimum level in the table
  def parent_min_val(table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]] = {
    var minimum:Option[Int] = get_minimum(table)
    // new case added here for none's....
    if(minimum == None)
    {
      var parCount:Int = 0
      for(value <- table.keys)
      {
        if(table.get(value).get.get_status() == Par())
          parCount += 1
      }

      // two cases here: no parent exists, or one parent exists where the level is null
      if(parCount == 0)
      {
        for(value <- table.keys)
        {
          if(table.get(value).get.get_status() != Self())
          {
            val the_weight: A = table.get(value).get.get_weight()
            val addEntry:Table_Info[A] = new Table_Info[A](Par(), Some(0), the_weight)
            var new_table = table + (value -> addEntry)
            return new_table
          }
        }
        table
      }
      else
      {
        // in this case, find the first parent and set its level to 0
        for(value <- table.keys)
        {
          if(table.get(value).get.get_status() == Par())
          {
            val the_weight: A = table.get(value).get.get_weight()
            val addEntry: Table_Info[A] = new Table_Info[A](Par(), Some(0), the_weight)
            var new_table = table + (value -> addEntry)
            return new_table
          }
        }
        table
      }
    }
    else {
      for (value <- table.keys) {
        if (table.get(value).get.get_level() == None) {
          // do nothing
        }
        else {
          if (table.get(value).get.get_level().get == minimum.get) {
            // now we are at what should be the parent
            val the_weight: A = table.get(value).get.get_weight()
              if (table.get(value).get.get_status() != Par()) {
                // then make this have a status of parent
                val addEntry: Table_Info[A] = new Table_Info[A](Par(), minimum, the_weight)
                var new_table = table + (value -> addEntry)
                return new_table
              }
          }
        }
      }
      table
    }
  }

  // a helper function which makes sure that the level (minimum + 1) element has status self()
  def handle_self_level(table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]]  = {
    var minimum:Option[Int] = get_minimum(table)

    if(minimum == None) {
      return table
    }
    var null_count:Int = 0
    for(value <- table.keys)
    {
      if(table.get(value).get.get_level() == None)
        null_count += 1
    }
    if(null_count == table.size - 1)
    {
      var num_self:Int = 0
      for(value <- table.keys)
      {
        if(table.get(value).get.get_status() == Self())
        {
          num_self += 1
        }
      }

      if(num_self == 0)
      {
        // check for case in which no self exists
        for(value <- table.keys)
        {
          if(table.get(value).get.get_status() != Par())
          {
            val the_weight: A = table.get(value).get.get_weight()
            val addEntry:Table_Info[A] = new Table_Info[A](Self(), Some(minimum.get + 1), the_weight)
            var new_table = table + (value -> addEntry)
            return new_table
          }
        }
        table
      }
      else
      {
        // check for case in which one self exists
        for(value <- table.keys)
        {
          if(table.get(value).get.get_status() == Self())
          {
            val the_weight: A = table.get(value).get.get_weight()
            val addEntry: Table_Info[A] = new Table_Info[A](Self(), Some(minimum.get + 1), the_weight)
            var new_table = table + (value -> addEntry)
            return new_table
          }
        }
        table
      }
    }
    else
    {
      var minPlusOneExists:Boolean = false
      for(value <- table.keys)
      {
        if(table.get(value).get.get_level() != None && table.get(value).get.get_level() == minimum.get + 1)
        {
          minPlusOneExists = true
        }
      }
      if(minPlusOneExists) {
        var cond_two_satisfied: Boolean = false
        for (value <- table.keys) {
          if (table.get(value).get.get_status() == Self()) {
            if (table.get(value).get.get_level() == None) {
              // do nothing
            }
            else {
              val curr_level: Int = table.get(value).get.get_level().get
              if (curr_level == minimum.get + 1)
                cond_two_satisfied = true
            }
          }
        }

        // if cond_two is not satisfied, find minimum + 1 level in the table and make that self
        if (!cond_two_satisfied) {
          for (value <- table.keys) {
            if (table.get(value).get.get_level() == None) {
              // do nothing
            }
            else {
              if (table.get(value).get.get_level().get == minimum.get + 1) {
                val the_weight: A = table.get(value).get.get_weight()
                val addEntry: Table_Info[A] = new Table_Info[A](Self(), Some(minimum.get + 1), the_weight)
                var new_table = table + (value -> addEntry)
                return new_table
              }
            }
          }
        }
        table
      }
      else
      {
        // - check to see if a self exists
        var num_self:Int = 0
        for(value <- table.keys)
        {
          if(table.get(value).get.get_status() == Self())
            num_self += 1
        }
        if(num_self != 0)
        {
          // change its level to minimum + 1
          for(value <- table.keys)
          {
            if(table.get(value).get.get_status() == Self())
            {
              val the_weight: A = table.get(value).get.get_weight()
              val addEntry: Table_Info[A] = new Table_Info[A](Self(), Some(minimum.get + 1), the_weight)
              var new_table = table + (value -> addEntry)
              return new_table
            }
          }
          table
        }
        else {
          // - if no self exists - if there is a null element, replace it with self, otherwise change a peer element to self and modify its level
          for(value <- table.keys)
          {
            if(table.get(value).get.get_level() == None)
            {
              val the_weight: A = table.get(value).get.get_weight()
              val addEntry: Table_Info[A] = new Table_Info[A](Self(), Some(minimum.get + 1), the_weight)
              var new_table = table + (value -> addEntry)
              return new_table
            }
          }

          for(value <- table.keys)
          {
            if(table.get(value).get.get_status() != Par())
            {
              val the_weight: A = table.get(value).get.get_weight()
              val addEntry: Table_Info[A] = new Table_Info[A](Self(), Some(minimum.get + 1), the_weight)
              var new_table = table + (value -> addEntry)
              return new_table
            }
          }
          table
        }
      }
    }
  }

  // makes sure only one parent exists, and if more than one does, replace the other one with an appropriate status
  // if no parent exists, add one
  def confirm_one_parent(table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]] = {
    var minimum:Option[Int] = get_minimum(table)
    var new_table = table
    var count:Int = 0
    var minimumChecked:Boolean = false
    for(value <- table.keys)
    {
      if(table.get(value).get.get_status() == Par())
        count += 1
    }
    if(count == 1)
      new_table
    else if(count == 0)
      parent_min_val(new_table)
    else {
      for (value <- table.keys) {
        if (table.get(value).get.get_status() == Par()) {
          // now check the level. if the level is the minimum, then its good.. otherwise an appropriate switch is necessary
          if (table.get(value).get.get_level() == None || table.get(value).get.get_level().get != minimum.get || minimumChecked) {
            new_table = switch_element(value, new_table)
          }
          else
          {
            minimumChecked = true
          }
        }
      }
      // return the modified table here
      new_table
    }
  }

  // makes sure only one self exists, and if more than one does, replace the other ones with appropriate status
  def confirm_one_self(table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]] = {
    var minimum:Option[Int] = get_minimum(table)
    var new_table = table
    var count:Int = 0
    var checked:Boolean = false
    for(value <- table.keys)
    {
      if(table.get(value).get.get_status() == Self())
        count += 1
    }
    if(count == 0) {
      handle_self_level(new_table)
    }
    else if(count == 1) {
      new_table
    }
    else {
      for (value <- table.keys) {
        if (table.get(value).get.get_status() == Self()) {
          // check the level and see if it = minimum + 1. make an appropriate switch if it does not
          if (table.get(value).get.get_level() == None || table.get(value).get.get_level().get != (minimum.get + 1) || checked) {
            new_table = switch_element(value, new_table)
          }
          else
            checked = true
        }
      }
      // return the modified table here
      new_table
    }
  }

  // switches the element as necessary and returns the modified table
  def switch_element(switcher:ActorRef, table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]] = {
    // switch to peer for now.. see when to switch to child?
    val curr_level:Option[Int] = table.get(switcher).get.get_level()
    val curr_weight:A = table.get(switcher).get.get_weight()
    val addEntry: Table_Info[A] = new Table_Info[A](Peer(), curr_level, curr_weight)
    var new_table = table + (switcher -> addEntry)
    new_table
  }

  // make sure no elements are repeated twice
  def confirm_no_repetition(table:Map[ActorRef, Table_Info[A]]):Map[ActorRef, Table_Info[A]] = {
    // simply returns current table as of now, since this should not be a problem with maps?
    // may modify if necessary
    table
  }
}
