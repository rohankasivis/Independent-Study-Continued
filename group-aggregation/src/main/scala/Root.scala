import akka.actor.ActorRef

class Root extends NodeActors
{
  // these are all of the private variables that are used
  // a new entry is added.. simply instantiate the private variables accordingly here
  def new_entry(nodeActors:ActorRef)
  {
    adjacent += nodeActors
    sent_mass = sent_mass + (nodeActors -> 0)
    received_mass = received_mass + (nodeActors -> 0)
    levels += (nodeActors -> 1)
  }

  def remove_entry(nodeActors:ActorRef)
  {
    adjacent -= nodeActors
  }

  def level(nodeActors:Set[ActorRef], levels:Map[ActorRef, Int]): Option[Int] =
  {
    // not implemented here - no need to, as this is the root with default level 0
    val ret:Option[Int] = Some(0)
    return ret
  }

  def parent(nodeActors:Set[ActorRef], levels:Map[ActorRef, Int]): Option[ActorRef] =
  {
    // not implemented here - no need to, as the root does not contain any parents
    null
  }

  def send(nodeActors:ActorRef, value:Status)
  {
    // in this case, we do not have to check for option, as 0 will always be passed in

    nodeActors ! value
  }

  def broadcast_var()
  {
    // not implemented here
  }

  def receive: Receive = {
    case New(arg1) => val result = {
      arg1 match {
        case null => None
        case some =>
          System.out.println ("Start Calling in Root Case New :" + arg1.toString () )
          var send_int: Option[Int] = Some (0)
          val actorref = self
          send (arg1, Status (actorref, send_int) ) // passing in a status of level 0 to the send function
          new_entry (arg1)
          System.out.println ("Stop Calling in Root Case New :" + arg1.toString () )
      }
      sender ! true
    }

    case Fail(arg1) => val result = {
      arg1 match
      {
        case null => None
        case some =>
          remove_entry (arg1)
          val sent_val: Int = sent_mass.get (arg1).get
          val received_val: Int = received_mass.get (arg1).get
          aggregate_mass = aggregate_mass + sent_val - received_val
      }
    }

    case Aggregate(arg1, arg2) => val result = {
      arg1 match {
        case null => None
        case some =>
          println ("received Aggregate(" + arg2 + ") from " + arg1.toString () )
          aggregate_mass = aggregate_mass + arg2
          println ("Aggregate Mass value = " + aggregate_mass)
          var temp: Int = received_mass.get (arg1).get + arg2
          received_mass = received_mass + (arg1 -> temp) // reassignment of received mass to modify index
      }
    }

    case Local(arg1) => val result = {
      println("Received Aggregate in root node :"+arg1)
      aggregate_mass = aggregate_mass + arg1 - local_mass
      println(" Aggregate in root node :"+aggregate_mass)
      local_mass = arg1
    }

    case Status(arg1, arg2) => val result = {
      // dont do anything here
    }
  }
}