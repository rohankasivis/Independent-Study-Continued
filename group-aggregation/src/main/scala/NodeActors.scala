import Group.Group
import akka.actor._

// these are all of the necessary case classes
case class New(newActor:ActorRef)
case class Fail(removeActor:ActorRef)
case class Aggregate[A](aggregateActor:ActorRef, valueToAggregate:A)
case class Local[A](localAdd:A)
case class Status(actorOne:ActorRef, theStatus:Option[Int])
case class SendAggregate()
case class sendBroadcast()
case class sendToSelf()

// this is the class which the root/non-root will extend
abstract class NodeActors[A](group:Group[A]) extends Actor
{
  protected var levels:Map[ActorRef, Int] = Map.empty
  protected var balance:Map[ActorRef, A] = Map.empty
  protected var local_mass:A=0.asInstanceOf[A]
  protected var aggregate_mass:A=0.asInstanceOf[A]
  protected var adjacent:Set[ActorRef] = Set.empty
  protected var broadcast:Boolean = false
  protected val isEnabled = sys.props.get("DEBUG").getOrElse("false").toBoolean
  protected val innerGroup:Group[A]

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