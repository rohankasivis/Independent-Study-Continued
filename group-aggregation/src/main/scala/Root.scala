import akka.actor.ActorRef

class Root extends NodeActors
{
  def level(nodeActors:Set[ActorRef], levels:Map[ActorRef, Int]): Option[Int] =
  {
    // not implemented here - no need to, as this is the root with default level 0
    val ret:Option[Int] = Some(0)
    return ret
  }

  def parent(nodeActors:Set[ActorRef], levels:Map[ActorRef, Int]): Option[ActorRef] =
  {
    // not implemented here - no need to, as the root does not contain any parents
    None
  }

  def send(nodeActors:ActorRef, value:Status)
  {
    // in this case, we do not have to check for option, as 0 will always be passed in

    nodeActors ! value
  }

  // added by Karl
  def getLevel = Some(0)

  // added by Karl
  def getParent = null

  def broadcast_var()
  {
    // not implemented here
  }

  def handle_new(newActor:ActorRef) =
  {
    if(isEnabled)
      System.out.println ("Start Calling in Root Case New :" + newActor.toString () )
    newActor ! Status(self, Some(0)) // passing in a status of level 0 to the send function
    adjacent += newActor
    balance = balance + (newActor -> 0)
    if(isEnabled)
      System.out.println ("Stop Calling in Root Case New :" + newActor.toString () )
    sender ! true
  }

  def handle_fail(removeActor:ActorRef) =
  {
    adjacent -= removeActor
    aggregate_mass = aggregate_mass + balance.get(removeActor).get
  }

  def handle_agg_message(aggregateActor:ActorRef, valueToAggregate:Int) =
  {
    if(isEnabled)
      println ("received Aggregate(" + valueToAggregate + ") from " + aggregateActor.toString () )
    aggregate_mass = aggregate_mass + valueToAggregate
    if(isEnabled)
      println ("Aggregate Mass value = " + aggregate_mass)
    balance = balance + (aggregateActor -> (balance.get(aggregateActor).get - valueToAggregate)) // reassignment of received mass to modify index
  }

  def handle_local(localAdd:Int) =
  {
    if(isEnabled)
      println("Received Aggregate in root node :"+localAdd)
    aggregate_mass = aggregate_mass + localAdd - local_mass
    if(isEnabled)
      println(" Aggregate in root node :"+aggregate_mass)
    local_mass = localAdd
  }

  def receive: Receive = {
    case New(newActor) =>
      handle_new(newActor)

    case Fail(removeActor:ActorRef) =>
      handle_fail(removeActor)

    case Aggregate(aggregateActor, valueToAggregate) =>
      handle_agg_message(aggregateActor, valueToAggregate)

    case Local(localAdd) =>
      handle_local(localAdd)

    case Status(arg1, arg2) => val result = {
      // dont do anything here
    }
  }
}