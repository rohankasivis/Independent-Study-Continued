import javafx.print.Printer

import Group.Group
import akka.actor.{ActorRef, Cancellable}
import akka.event.slf4j.Logger

import scala.concurrent.duration.Duration

class NonRoot extends NodeActors {

  import context.dispatcher

  private var hasStartedSelfSend = false
  private var deliverToSelf: Cancellable = null

  override def postStop() = {
    if (hasStartedSelfSend)
      deliverToSelf.cancel()
  }

  // added by Karl
  def parent(nodeActors: Set[ActorRef], levels: Map[ActorRef, Int]): Option[ActorRef] = {
    par(nodeActors, levels) match {
      case Some((parentRef, _)) => Some(parentRef)
      case None => None
    }
  }

  // added by Karl
  def level(nodeActors: Set[ActorRef], levels: Map[ActorRef, Int]): Option[Int] = {
    par(nodeActors, levels) match {
      case Some((_, parentLevel)) => Some(parentLevel + 1)
      case None => None
    }
  }

  // added by Karl
  def getLevel = level(adjacent, levels)

  // added by Karl
  def getParent = parent(adjacent, levels)

  // added by Karl
  def par(nodeActors: Set[ActorRef], levels: Map[ActorRef, Int]): Option[Tuple2[ActorRef, Int]] = {
    if (nodeActors.isEmpty)
      return None

    val currRef: ActorRef = nodeActors.head
    par(nodeActors.tail, levels) match {
      case Some((parRef, parLevel)) =>
        levels.get(currRef) match {
          case Some(currLevel) =>
            if (currLevel < parLevel)
              Some((currRef, currLevel))
            else
              Some((parRef, parLevel))
          case None =>
            Some((parRef, parLevel))
        }
      case None =>
        levels.get(currRef) match {
          case Some(currLevel) =>
            Some((currRef, currLevel))
          case None =>
            None
        }
    }
  }

  def broadcast_var(operations: Group[Int]) {
    if (isEnabled)
      println("Value of broadcast : " + broadcast + " in ActorRef: " + self.toString())
    if (broadcast) {
      if (isEnabled)
        println("Entering broadcast_var")
      for(curr <- adjacent)
        curr ! Status(self, level(adjacent, levels), operations)
      broadcast = false
      if (isEnabled)
        println("Exiting broadcast_var")
    }
  }

  def handle_aggregate(operations: Group[Int]) = {
    val res: Option[ActorRef] = parent(adjacent, levels)
    res match {
      case Some(value) =>
        balance.get(res.get) match {
          case Some(s) =>
            if (isEnabled) {
              println("Self :" + self.toString() + "levels size :" + levels.size + " adjacent size:" + adjacent.size)
              println(self.toString() + " sending Aggregate(" + aggregate_mass + ") to " + res.get.toString())
            }
            res.get ! Aggregate(self, aggregate_mass, operations)
            balance = balance + (res.get -> (operations.op(balance.get(res.get).get, aggregate_mass)))
            aggregate_mass = 0
          case None => // do nothing
        }
      case None => // do nothing
    }
  }

  def handle_new(newActor: ActorRef, operations: Group[Int]) =
  {
    //    System.out.println ("Start Calling in NonRoot Case New :" + arg1.toString () )
    val first: Option[Int] = level (adjacent, levels) match {
      case Some (s) => Option (s)
      case None => Option (- 1)
    }
    if (first.get != - 1) {
      // then the level does exist
      newActor ! Status(self, first, operations)
    }
    adjacent += newActor
    balance = balance + (newActor -> 0)
    if(isEnabled)
      println("adjacent size in self :"+self.toString()+" " +adjacent.size)
    //      System.out.println ("Finish Calling in NonRoot Case New :" + self.toString () )
    sender ! true
  }

  def handle_fail(removeActor: ActorRef, operations: Group[Int]) =
  {
    balance.get(removeActor) match
    {
      case Some(s) =>
        balance.get(removeActor) match {
          case Some(s) =>
            if(isEnabled) {
              System.out.println("Inside Fail")
              println("Fail message received from " + removeActor.toString() + " to ActorRef :" + self.toString())
            }

            if (level (adjacent, levels) != level (adjacent - removeActor, levels.filterKeys (_!= removeActor)) )
              broadcast = true

            adjacent -= removeActor
            levels = levels.filterKeys (_!= removeActor)
            aggregate_mass = operations.op(aggregate_mass, balance.get(removeActor).get)
            if(isEnabled)
              System.out.println ("Inside Fail")
          case None => None
        }
      case None => None
    }
  }

  def handle_agg_message(aggregateActor: ActorRef, valueToAdd: Int, operations: Group[Int]) =
  {
    if(isEnabled)
      println ("received Aggregate(" + valueToAdd + ") from " + aggregateActor.toString () )
    aggregate_mass = operations.op(aggregate_mass, valueToAdd)
    if(isEnabled)
      println ("Aggregate Mass value = " + aggregate_mass)
    balance.get (aggregateActor) match {
      case Some (s) =>
        balance = balance + (aggregateActor -> (operations.op(balance.get(aggregateActor).get, operations.inverse(valueToAdd))))
      case None => 0
    }
    handle_aggregate(operations)
  }

  def handle_local(localAdd:Int, operations: Group[Int]) =
  {
    if(isEnabled)
      println("Received Aggregate in "+self.toString()+" node : " + localAdd)
    aggregate_mass = operations.op(aggregate_mass, operations.op(localAdd, operations.inverse(local_mass)))
    if(isEnabled)
      println(" Aggregate in "+self.toString()+" node  :"+aggregate_mass)

    local_mass = localAdd
  }

  def handle_status(actorOne:ActorRef, arg2:Option[Int], operations: Group[Int]) =
  {
    // check the adjacent contains the passed in arg1 if not
    // add it
    if(isEnabled) {
      System.out.println("Start Calling in NonRoot Case Status: " + actorOne.toString())
      println(self.toString() + " adjacent size in status " + adjacent.size)
    }
    if (adjacent.isEmpty)
    {
      if(isEnabled)
        println("Empty")
    }
    if (! adjacent.contains (actorOne) ) {
      adjacent += actorOne
    }
    levels += (actorOne -> arg2.get)
    if (level (adjacent, levels) != level (adjacent, levels.filterKeys (_!= actorOne)) )
      broadcast = true
    //  levels = levels.filterKeys(_ != arg1)
    if(isEnabled)
      System.out.println ("Stop Calling in NonRoot Case Status :" + actorOne.toString () )
  }

  def receive: Receive = {
    case New(newActor, operations) =>
      handle_new(newActor, operations)

    case Fail(removeActor, operations) =>
      handle_fail(removeActor, operations)

    case Aggregate(aggregateActor, valueToAggregate, operations) =>
      handle_agg_message(aggregateActor,valueToAggregate, operations)

    case Local(localAdd, operations) =>
      handle_local(localAdd, operations)

    case SendAggregate(operations) => {
      handle_aggregate(operations)
    }

    case sendBroadcast(operations) => {
      broadcast_var(operations)
    }

    case Status(actorOne, theStatus, operations) =>
      handle_status(actorOne, theStatus, operations)

    case sendToSelf(operations) =>
    {
      if(!hasStartedSelfSend)
      {
        deliverToSelf = context.system.scheduler.schedule(Duration(500, "millis"), Duration(1000, "millis"), self, SendAggregate(operations))
        deliverToSelf = context.system.scheduler.schedule(Duration(500, "millis"), Duration(1000, "millis"), self, sendBroadcast(operations))
        hasStartedSelfSend = true
      }
    }
  }
}