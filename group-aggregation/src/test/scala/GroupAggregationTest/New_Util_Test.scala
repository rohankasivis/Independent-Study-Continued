// class generally will be updated as more functions are added
// used to test one on one functions

import Group.IntPlus
import Monoid.{IntAddition, MaxMonoid, Monoid, MultiModuloEleven}
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import akka.util.Timeout
import org.scalatest.WordSpecLike
import org.scalatest.matchers.MustMatchers

class New_Util_Test extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender
{
  "The Monoid class test cases " must {
  "do the following " in {

      val monoid = new IntAddition
      val utils_use:GAPUtil[Int] = new GAPUtil(monoid:Monoid[Int])
      // test with four nodes and make sure everything works
      val root_node = TestActorRef(Props(new GAPNode[Int](monoid)), name="root_node")
      val node_one = TestActorRef(Props(new GAPNode[Int](monoid)), name="node_one")
      val node_two = TestActorRef(Props(new GAPNode[Int](monoid)), name="node_two")
      val node_three = TestActorRef(Props(new GAPNode[Int](monoid)), name="node_three")

      // underlying values of these four nodes used for testing purposes
      val underlying_root_node = root_node.underlyingActor
      val underlying_node_one = node_one.underlyingActor
      val underlying_node_two = node_two.underlyingActor
      val underlying_node_three = node_three.underlyingActor

      // initialize tables here, one for each of these
      var root_table:Map[ActorRef, Tuple3[The_Status, Int, Int]] = Map.empty
      var one_table:Map[ActorRef, Tuple3[The_Status, Int, Int]] = Map.empty
      var two_table:Map[ActorRef, Tuple3[The_Status, Int, Int]] = Map.empty
      var three_table:Map[ActorRef, Tuple3[The_Status, Int, Int]] = Map.empty

      // test first new here - with root node
      root_table = utils_use.new_entry(root_node, true, root_table)
      root_table.size must equal(1)
      root_table.get(root_node).get._1 must equal(Par())

      // test first new here - without root node
      one_table = utils_use.new_entry(node_one, false, one_table)
      one_table.size must equal(1)
      one_table.get(node_one).get._1 must equal(Self())

      // test second new here on root_table
      root_table = utils_use.new_entry(node_one, true, root_table)
      root_table.size must equal(2)
      root_table.get(node_one).get._1 must equal(Peer())

      // test second new here on one_table
      one_table = utils_use.new_entry(node_one, false, one_table)
      one_table.size must equal(1)
    }
  }
}