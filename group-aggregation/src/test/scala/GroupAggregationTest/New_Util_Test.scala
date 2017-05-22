// class generally will be updated as more functions are added
// used to test one on one functions

import Group.IntPlus
import Monoid._
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
    "The GAPUtil test cases" must {
        val monoid = new IntAddition
        val utils_use:GAPUtil[Int] = new GAPUtil(monoid:Monoid[Int])
        // test with four nodes and make sure everything works
        val root_node = TestActorRef(new GAPNode[Int](monoid))
        val node_one = TestActorRef(new GAPNode[Int](monoid))
        val node_two = TestActorRef(new GAPNode[Int](monoid))
        val node_three = TestActorRef(new GAPNode[Int](monoid))

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

        // everything under new passes correctly
        "test for new: " in {
            // test_add_id
            // test first new here - with root node
            root_table = utils_use.new_entry(root_node, true, root_table)
            root_table.size must equal(1)
            root_table.get(root_node).get._1 must equal(Par())

            // test first new here - without root node
            one_table = utils_use.new_entry(node_one, false, one_table)
            one_table.size must equal(1)
            one_table.get(node_one).get._1 must equal(Self())

            // test second new here on root_table (adding different elements each time)
            root_table = utils_use.new_entry(node_one, true, root_table)
            root_table.size must equal(2)
            root_table.get(node_one).get._1 must equal(Peer())

            // test second new here on one_table (adding the same element twice)
            one_table = utils_use.new_entry(node_one, false, one_table)
            one_table.size must equal(1)
        }
        "test for remove " in {
            root_table = utils_use.new_entry(root_node, true, root_table)
            one_table = utils_use.new_entry(node_one, false, one_table)
            root_table = utils_use.new_entry(node_one, true, root_table)

            // test for existing delete in root_table - size should be 1
            root_table = utils_use.remove_entry(node_one, root_table)
            root_table.size must equal(1)

            // test for nonexisting delete in root_table - size should be 1
            root_table = utils_use.remove_entry(node_three, root_table)
            root_table.size must equal(1)

            // test for nonexisting delete in one_table - size should be 1
            one_table = utils_use.remove_entry(node_two, one_table)
            one_table.size must equal(1)

            // test for existing delete in root_table - size should be 0
            root_table = utils_use.remove_entry(root_node, root_table)
            root_table.size must equal(0)

            // test for existing delete in one_table - size should be 0
            one_table = utils_use.remove_entry(node_one, root_table)
            one_table.size must equal(0)
        }

        "test for update " in {
            root_table = utils_use.new_entry(root_node, true, root_table)
            one_table = utils_use.new_entry(node_one, false, one_table)
            root_table = utils_use.new_entry(node_one, true, root_table)

            // test updating a nonexistant entry in root
            root_table = utils_use.update_entry(node_two, 34, 2, root_node, root_table)
            root_table.size must equal(3)

            // test updating an existant entry in root where parent = self
            one_table = utils_use.update_entry(node_two, 40, 2, node_one, one_table)
            one_table.size must equal(2)
            one_table.get(node_one).get._1 must equal(Self())
            one_table.get(node_two).get._1 must equal(Child())

            // test updating an existant entry in root where T(n).Status = child
            root_table = utils_use.update_entry(node_two, 41, 2, root_node, root_table)
            root_table.size must equal(3)
            root_table.get(node_two).get._1 must equal(Peer())

            // test other case
            root_table = utils_use.update_entry(node_two, 67, 2, root_node, root_table)
            root_table.size must equal(3)
            root_table.get(node_two).get._1 must equal(Peer())
        }

        "test for level " in {

        }

        "test for parent " in {

        }

        "test for weight " in {

        }
    }
}