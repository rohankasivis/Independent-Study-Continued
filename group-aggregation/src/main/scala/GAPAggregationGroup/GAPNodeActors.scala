import Group.Group
import akka.actor._

// these are all of the necessary case classes
case class NewM(newActor:ActorRef, isRoot:Boolean)
case class FailM(removeActor:ActorRef)
case class UpdateM[A](updateActor:ActorRef, weight:A, level:Int, parent:ActorRef)
case class WeightM[A](weight:A)
case class sendBroadcast()

// this is the class which the root/non-root will extend
abstract class GAPNodeActors[A](group:Group[A]) extends Actor
{
  var table:Map[ActorRef, Tuple3[String, Int, A]] = Map.empty
  val system = ActorSystem("NodeActors")
}