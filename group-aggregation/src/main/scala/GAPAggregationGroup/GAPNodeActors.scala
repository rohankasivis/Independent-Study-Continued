import Monoid.Monoid
import akka.actor._

// these are all of the necessary case classes
case class NewM(newActor:ActorRef, isRoot:Boolean)
case class FailM(removeActor:ActorRef)
case class UpdateM[A](updateActor:ActorRef, weight:A, level:Int, parent:ActorRef)
case class WeightM[A](weight:A)

// this is the class which the root/non-root will extend
abstract class GAPNodeActors[A](monoid:Monoid[A]) extends Actor
{
  var table:Map[ActorRef, Tuple3[The_Status, Int, A]] = Map.empty
  var utils_use:GAPUtil[A] = new GAPUtil(monoid:Monoid[A], table)
  val system = ActorSystem("NodeActors")
}