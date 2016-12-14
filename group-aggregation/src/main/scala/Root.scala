import Group.Group
import akka.actor.ActorRef

class Root[A](group:Group[A]) extends NodeActors[A](group:Group[A])
{
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
    if (isEnabled)
      System.out.println("Start Calling in Root Case New :" + newActor.toString())
    newActor ! Status(self, Some(0)) // passing in a status of level 0 to the send function
    adjacent += newActor
    balance = balance + (newActor -> 0.asInstanceOf[A])
    if (isEnabled)
      System.out.println("Stop Calling in Root Case New :" + newActor.toString())
    sender ! true
  }

  def handle_fail(removeActor: ActorRef) = {
    adjacent -= removeActor
    aggregate_mass = innerGroup.op(aggregate_mass, balance.get(removeActor).get)
  }

  def handle_agg_message(aggregateActor: ActorRef, valueToAggregate: A) = {
    if (isEnabled)
      println("received Aggregate(" + valueToAggregate + ") from " + aggregateActor.toString())
    aggregate_mass = innerGroup.op(aggregate_mass, valueToAggregate)
    if (isEnabled)
      println("Aggregate Mass value = " + aggregate_mass)
    balance = balance + (aggregateActor -> (innerGroup.op(balance.get(aggregateActor).get, innerGroup.inverse(valueToAggregate)))) // reassignment of received mass to modify index
  }

  def handle_local(localAdd: A) = {
    if (isEnabled)
      println("Received Aggregate in root node :" + localAdd)
    aggregate_mass = innerGroup.op(aggregate_mass, innerGroup.op(localAdd, innerGroup.inverse(local_mass)))
    if (isEnabled)
      println(" Aggregate in root node :" + aggregate_mass)
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

  override protected val innerGroup: Group[A] = group

}