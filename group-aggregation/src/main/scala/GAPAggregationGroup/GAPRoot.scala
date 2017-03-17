import Group.Group
import akka.actor.ActorRef

class GAPRoot[A](group:Group[A]) extends NodeActors[A](group:Group[A]){
  def level(nodeActors: Set[ActorRef], levels: Map[ActorRef, Int]): Option[Int] = {
    // not implemented here - no need to, as this is the root with default level 0
    val ret: Option[Int] = Some(0)
    return ret
  }

  def parent(nodeActors: Set[ActorRef], levels: Map[ActorRef, Int]): Option[ActorRef] = {
    // not implemented here - no need to, as the root does not contain any parents
    None
  }

  def send(nodeActors: ActorRef, value: Status) {
    // in this case, we do not have to check for option, as 0 will always be passed in

    nodeActors ! value
  }

  // added by Karl
  def getLevel = Some(0)

  // added by Karl
  def getParent = null

  def broadcast_var() {
    // not implemented here
  }

  def handle_new(newActor: ActorRef) = {
    newActor ! Status(self, Some(0)) // passing in a status of level 0 to the send function
    adjacent += newActor
    balance = balance + (newActor -> 0.asInstanceOf[A])
    sender ! true
  }

  def handle_fail(removeActor: ActorRef) = {
    balance.get(removeActor) match {
      case Some(s) =>
        adjacent -= removeActor
        aggregate_mass = group.op(aggregate_mass, balance.get(removeActor).get)
      case None => None
    }

  }

  def handle_agg_message(aggregateActor: ActorRef, valueToAggregate: A) = {
    aggregate_mass = group.op(aggregate_mass, valueToAggregate)
    balance = balance + (aggregateActor -> (group.op(balance.get(aggregateActor).get, group.inverse(valueToAggregate)))) // reassignment of received mass to modify index
  }

  def handle_local(localAdd: A) = {
    aggregate_mass = group.op(aggregate_mass, group.op(localAdd, group.inverse(local_mass)))
    local_mass = localAdd
  }

  def receive: Receive = {
    case New(newActor) =>
      handle_new(newActor)

    case Fail(removeActor) =>
      handle_fail(removeActor)

    case Aggregate(aggregateActor, valueToAggregate) =>
      handle_agg_message(aggregateActor, valueToAggregate.asInstanceOf[A])

    case Local(localAdd) =>
      handle_local(localAdd.asInstanceOf[A])

    case Status(arg1, arg2) => val result = {
      // dont do anything here
    }
  }
}
