import Group.Group
import akka.actor._

// these are all of the necessary case classes
case class New(newActor:ActorRef, operations: Group[Int])
case class Fail(removeActor:ActorRef, operations: Group[Int])
case class Aggregate(aggregateActor:ActorRef, valueToAggregate:Int, operations: Group[Int])
case class Local(localAdd:Int, operations: Group[Int])
case class Status(actorOne:ActorRef, theStatus:Option[Int], operations: Group[Int])
case class Drop(actorRemove:ActorRef, removeVal:Int, operations: Group[Int])
case class SendAggregate(operations: Group[Int])
case class sendBroadcast(operations: Group[Int])
case class sendToSelf(operations: Group[Int])

// this is the class which the root/non-root will extend
abstract class NodeActors extends Actor
{
  protected var levels:Map[ActorRef, Int] = Map.empty
  protected var balance:Map[ActorRef, Int] = Map.empty
  protected var local_mass:Int = 0
  protected var aggregate_mass:Int = 0
  protected var adjacent:Set[ActorRef] = Set.empty
  protected var broadcast:Boolean = false
  protected val isEnabled = sys.props.get("DEBUG").getOrElse("false").toBoolean

  val system = ActorSystem("NodeActors")

  def getLevel: Option[Int]
  def getParent: Option[ActorRef]

  def getAggregateMass = aggregate_mass
  def getLocalMass = local_mass
  def getBroadcast = broadcast

  def isAdjacentTo(a: ActorRef) = adjacent.contains(a)
  def getLevelFor(a: ActorRef) = levels.get(a)
  def getBalanceFor(a: ActorRef) = balance.get(a)
}