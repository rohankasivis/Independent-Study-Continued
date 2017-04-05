import Monoid.{IntAddition, Monoid}
import akka.actor.{ActorRef, ActorSystem, Props}

object GAPTest extends App
{
  var table:Map[ActorRef, Tuple3[The_Status, Int, Int]] = Map.empty
  val system = ActorSystem("NeighborSetSystem")
  val monoid = new IntAddition

  var utils_use:GAPUtil[Int] = new GAPUtil(monoid:Monoid[Int], table)
  val root_node = system.actorOf(Props(new GAPNode[Int](monoid)), name="root_node")
  val node_one = system.actorOf(Props(new GAPNode[Int](monoid)), name="node_one")
  val node_two = system.actorOf(Props(new GAPNode[Int](monoid)), name="node_two")
  val node_three = system.actorOf(Props(new GAPNode[Int](monoid)), name="node_three")
  utils_use.new_entry(root_node, true)
  utils_use.new_entry(node_one, true)
  utils_use.new_entry(node_two, true)
  utils_use.new_entry(node_three, true)

  // more test cases needed
}
