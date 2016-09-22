import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorSystem}

import scala.concurrent.duration.Duration

class NonRoot extends NodeActors
{
  override def postStop() = {

  }

  def new_entry(nodeActors:ActorRef)
  {
    adjacent += nodeActors
    sent_mass = sent_mass + (nodeActors -> 0)
    received_mass = received_mass + (nodeActors -> 0)
  }

  def remove_entry(nodeActors:ActorRef)
  {
    adjacent -= nodeActors        // remove an element from the nodeactors set
    // remove the element corresponding to the nodeactor from level map
  }

  def parent(nodeActors:Set[ActorRef], levels:Map[ActorRef, Int]): Option[ActorRef] =
  {
    par(nodeActors, levels) match
    {
      case Some((parentRef, _)) => Some(parentRef)
      case None => None
    }
  }

  def level(nodeActors:Set[ActorRef], levels:Map[ActorRef, Int]): Option[Int] =
  {
    par(nodeActors, levels) match
    {
      case Some((_, parentLevel)) => Some(parentLevel + 1)
      case None => None
    }
  }

  def par(nodeActors:Set[ActorRef], levels:Map[ActorRef, Int]): Option[Tuple2[ActorRef, Int]] =
  {
    nodeActors.isEmpty match
    {
      case false =>
      levels.isEmpty match {
        case false =>
          val currRef: ActorRef = nodeActors.head
          par (nodeActors.tail, levels) match {
          case Some ((parRef, parLevel) ) =>
          levels.get (currRef) match {
          case Some (currLevel) =>
          if (currLevel < parLevel)
          Some ((currRef, currLevel) )
          else
          Some ((parRef, parLevel) )
          case None =>
          Some ((parRef, parLevel) )
          }
          case None =>
          levels.get (currRef) match {
          case Some (currLevel) =>
          Some ((currRef, currLevel) )
          case None =>
          None
           }
           }
        case true => None
      }
      case true => None

    }
  }

  def send(nodeActors:Set[ActorRef], value:Status)
  {
    for(curr <- nodeActors)
      curr ! value
  }


  def broadcast_var()
  {
    println("Value of broadcast : "+broadcast+" in ActorRef: "+self.toString())
    if(broadcast)
    {
        println ("Entering broadcast_var")
        send (adjacent, Status (self, level (adjacent, levels) ) )
        broadcast = false
        println ("Exiting broadcast_var")
    }
  }

  def send(arg1: ActorRef, status: Status) =
  {
    arg1 ! status
  }

  def send_agg(arg1:ActorRef, adjacent:Aggregate) = {
    arg1 ! adjacent
  }

  def handle_aggregate() =
  {
    val res:Option[ActorRef] = parent(adjacent, levels)
    res match {
      case Some(value) =>
        sent_mass.get(res.get) match {
          case Some(s) =>
            println ("Self :" + self.toString () + "levels size :" + levels.size + " adjacent size:" + adjacent.size)
            println (self.toString () + " sending Aggregate(" + aggregate_mass + ") to " + res.get.toString () )
            send_agg(res.get, Aggregate(self, aggregate_mass))
            val tmp1: Int = sent_mass.get(res.get).get
            val temp = tmp1 + aggregate_mass
            sent_mass = sent_mass + (res.get -> temp)
            aggregate_mass = 0
          case None => // do nothing
        }
      case None => // do nothing
    }
  }

  def receive: Receive = {
    case New(arg1) => val result = {
      //    System.out.println ("Start Calling in NonRoot Case New :" + arg1.toString () )
      val first: Option[Int] = level (adjacent, levels) match {
        case Some (s) => Option (s)
        case None => Option (- 1)
      }
      if (first.get != - 1) {
        // then the level does exist
        send(arg1, Status(self, first))
      }
      new_entry (arg1)
      println("adjacent size in self :"+self.toString()+" " +adjacent.size)
    //      System.out.println ("Finish Calling in NonRoot Case New :" + self.toString () )
      sender ! true
    }

    case Fail(arg1) => val result = {
      sent_mass.get(arg1) match
      {
        case Some(s) =>
          received_mass.get(arg1) match {
            case Some(s) =>
              System.out.println ("Inside Fail")
              println ("Fail message received from " + arg1.toString () + " to ActorRef :" + self.toString () )

              val temp: Set[ActorRef] = adjacent - arg1
              val temp_lvl: Map[ActorRef, Int] = levels.filterKeys (_!= arg1) // created two temp variables to do the level check condition
              if (level (adjacent, levels) != level (temp, temp_lvl) )
                broadcast = true

              adjacent -= arg1
              levels = levels.filterKeys (_!= arg1)
                // adjacent(sent_index) = null     // effectively removes element
              val sent_val: Option[Int] = sent_mass.get (arg1)
              val received_val: Option[Int] = received_mass.get (arg1)
              aggregate_mass = aggregate_mass + sent_val.get - received_val.get
              System.out.println ("Inside Fail")
            case None => None
          }
        case None => None
      }
    }

    case Aggregate(arg1, arg2) => val result = {
      println ("received Aggregate(" + arg2 + ") from " + arg1.toString () )
      aggregate_mass = aggregate_mass + arg2
      println ("Aggregate Mass value = " + aggregate_mass)
        //aggregate_mass = aggregate_mass + arg2
      val tmp_val = received_mass.get (arg1) match {
      case Some (s) => s
      case None => 0
      }
      tmp_val + arg2
      handle_aggregate ()
    }

    case Drop(arg1, arg2) => val result = {
      aggregate_mass = aggregate_mass + arg2
      val tmp_val=sent_mass.get(arg1)match
      {
        case Some(s) => s
        case None => 0
      }
      tmp_val - arg2
    }

    case Local(arg1) => val result = {
      println("Received Aggregate in "+self.toString()+" node : " + arg1)
      aggregate_mass = aggregate_mass + arg1 - local_mass
      println(" Aggregate in "+self.toString()+" node  :"+aggregate_mass)
     // handle_aggregate()

      local_mass = arg1
    }
    case SendAggregate() => {
      handle_aggregate()
    }

    case sendBroadcast() => {
      broadcast_var()
    }

    case Status(arg1, arg2) => val result = {
      // check the adjacent contains the passed in arg1 if not
      // add it
        System.out.println ("Start Calling in NonRoot Case Status: " + arg1.toString () )
          println(self.toString()+" adjacent size in status "+adjacent.size)
          if (adjacent.isEmpty)
            {
              println("Empty")
            }
        if (! adjacent.contains (arg1) ) {
        adjacent += arg1
        }
        levels += (arg1 -> arg2.get)
        val temp_lvl: Map[ActorRef, Int] = levels.filterKeys (_!= arg1)
        if (level (adjacent, levels) != level (adjacent, temp_lvl) )
        broadcast = true
          //  levels = levels.filterKeys(_ != arg1)
        System.out.println ("Stop Calling in NonRoot Case Status :" + arg1.toString () )
      }
    }
}