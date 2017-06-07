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
        val helpers_use:GAPHelper[Int] = new GAPHelper(monoid:Monoid[Int])
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
        var root_table:Map[ActorRef, Table_Info[Int]] = Map.empty
        var one_table:Map[ActorRef, Table_Info[Int]] = Map.empty
        var two_table:Map[ActorRef, Table_Info[Int]] = Map.empty
        var three_table:Map[ActorRef, Table_Info[Int]] = Map.empty

        // everything under new passes correctly
        "test for new: check first new with root node" in {
            // test_add_id
            // test first new here - with root node
            root_table = utils_use.new_entry(root_node, true, root_table)
            root_table.size must equal(2)
            root_table.get(root_node).get.get_status() must equal(Par())
        }
        "test for new: check first new on nonroot node" in {
            // test first new here - without root node
            one_table = utils_use.new_entry(node_one, false, one_table)
            one_table.size must equal(1)
            one_table.get(node_one).get.get_status() must equal(Self())
        }
        "test for new: check second new on root table (adding different elements)" in {
            // test second new here on root_table (adding different elements each time)
            root_table = utils_use.new_entry(root_node, true, root_table)
            root_table = utils_use.new_entry(node_one, true, root_table)
            root_table.size must equal(3)
            root_table.get(node_one).get.get_status() must equal(Peer())
        }
        "test for new: check adding secnd new on one_table (adding the same element twice)" in {
            // test second new here on one_table (adding the same element twice)
            one_table = utils_use.new_entry(node_one, false, one_table)
            one_table = utils_use.new_entry(node_one, false, one_table)
            one_table.size must equal(1)
        }

        // make sure that everything under remove works
        "test for remove: delete in root_table " in {
            // test for existing delete in root_table - size should be 1
            root_table = utils_use.new_entry(root_node, true, root_table)
            root_table = utils_use.new_entry(node_one, true, root_table)
            root_table = utils_use.remove_entry(node_one, root_table)
            root_table.size must equal(2)
        }
        "test for remove: nonexisting delete in root_table" in {
            // test for nonexisting delete in root_table - size should be 1
            root_table = utils_use.new_entry(root_node, true, root_table)
            root_table = utils_use.remove_entry(node_three, root_table)
            root_table.size must equal(2)
        }
        "test for remove: nonexisting delete in one_table" in {
            // test for nonexisting delete in one_table - size should be 1
            one_table = utils_use.new_entry(node_one, false, one_table)
            one_table = utils_use.remove_entry(node_two, one_table)
            one_table.size must equal(1)
        }
        "test for remove: existing delete in root_table" in {
            // test for existing delete in root_table - size should be
            root_table = utils_use.new_entry(root_node, true, root_table)
            root_table = utils_use.remove_entry(root_node, root_table)
            root_table.size must equal(1)
        }
        "test for remove: existing delete in one_table" in {
            // test for existing delete in one_table - size should be 0
            one_table = utils_use.new_entry(node_one, false, one_table)
            one_table = utils_use.remove_entry(node_one, root_table)
            one_table.size must equal(0)
        }

        // make sure that everything under update works
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
            one_table.get(node_one).get.get_status() must equal(Self())
            one_table.get(node_two).get.get_status() must equal(Child())

            // test updating an existant entry in root where T(n).Status = child
            root_table = utils_use.update_entry(node_two, 41, 2, root_node, root_table)
            root_table.size must equal(3)
            root_table.get(node_two).get.get_status() must equal(Peer())

            // test other case
            root_table = utils_use.update_entry(node_two, 67, 2, root_node, root_table)
            root_table.size must equal(3)
            root_table.get(node_two).get.get_status() must equal(Peer())
        }

        // level tests
        "test for level: check level in table with 1 element " in {
          one_table = utils_use.new_entry(node_one, false, one_table)
          utils_use.getLevel(one_table) must equal (None)
        }
        "test for level: check level in newly instantiated root table with 2 elements " in {
          root_table = utils_use.new_entry(root_node, true, root_table)
          utils_use.getLevel(root_table) must equal (Some(0))
        }
        "test for level: check level in a root table with multiple elements" in {
          root_table = utils_use.new_entry(root_node, true, root_table)
          root_table = utils_use.new_entry(node_one, true, root_table)
          root_table = utils_use.new_entry(node_two, true, root_table)
          root_table = utils_use.new_entry(node_three, true, root_table)
          utils_use.getLevel(root_table) must equal (Some(0))
        }
        "test for level: check level in a nonroot table with multiple elements" in {
          one_table = utils_use.new_entry(node_one, false, one_table)
          one_table = utils_use.update_entry(node_one, monoid.id, utils_use.getLevel(one_table).get, null, one_table)
          utils_use.getLevel(one_table) must equal (Some(0))
        }

        // parent tests
        "test for parent: make sure no parent in table with 1 element" in {
          one_table = utils_use.new_entry(node_one, false, one_table)
          utils_use.getParent(one_table) must equal (null)
        }
        "test for parent: check parent in new root table with 2 elements" in {
          root_table = utils_use.new_entry(root_node, true, root_table)
          utils_use.getParent(root_table) must equal (root_node)
        }
        "test for parent: check parent in a root table with multiple elements" in {
          root_table = utils_use.new_entry(root_node, true, root_table)
          root_table = utils_use.new_entry(node_one, true, root_table)
          root_table = utils_use.new_entry(node_two, true, root_table)
          root_table = utils_use.new_entry(node_three, true, root_table)
          utils_use.getParent(root_table) must equal (root_node)
        }
        "test for parent: check parent in a nonroot table with multiple elements" in {
          one_table = utils_use.new_entry(node_one, false, one_table)
          one_table = utils_use.update_entry(node_one, monoid.id, utils_use.getLevel(one_table).get, null, one_table)
          utils_use.getParent(one_table) must equal (Par())
        }

        // minimum tests
        "test for minimum: check min in table with 1 element" in {
          one_table = utils_use.new_entry(node_one, false, one_table)
          helpers_use.get_minimum(one_table) must equal (None)
        }
        "test for minimum: check min in new root table with 2 elements" in {
          root_table = utils_use.new_entry(root_node, true, root_table)
          helpers_use.get_minimum(root_table) must equal (Some(0))
        }
        "test for minimum: check min in a root table with multiple elements" in {
          root_table = utils_use.new_entry(root_node, true, root_table)
          root_table = utils_use.new_entry(node_one, true, root_table)
          root_table = utils_use.new_entry(node_two, true, root_table)
          root_table = utils_use.new_entry(node_three, true, root_table)
          helpers_use.get_minimum(root_table) must equal (Some(0))
        }
        "test for minimum: check min in a nonroot table with multiple elements" in {
          one_table = utils_use.new_entry(node_one, false, one_table)
          one_table = utils_use.update_entry(node_one, monoid.id, utils_use.getLevel(one_table).get, null, one_table)
          helpers_use.get_minimum(one_table) must equal (Some(1))
        }

        "test for parent_min_val " in {
          one_table = utils_use.new_entry(node_one, false, one_table)
          one_table = utils_use.update_entry(node_one, monoid.id, utils_use.getLevel(one_table).get, null, one_table)
          one_table = utils_use.new_entry(node_two, false, one_table)
          one_table = utils_use.update_entry(node_one, monoid.id, utils_use.getLevel(one_table).get, node_two, one_table)
          one_table = helpers_use.parent_min_val(one_table)
          // add truth statement
        }

        "test for handle_self_level " in {
          one_table = utils_use.new_entry(node_one, false, one_table)
          one_table = utils_use.update_entry(node_one, monoid.id, utils_use.getLevel(one_table).get, null, one_table)
          one_table = utils_use.new_entry(node_two, false, one_table)
          one_table = utils_use.update_entry(node_one, monoid.id, utils_use.getLevel(one_table).get, node_two, one_table)
          one_table = helpers_use.handle_self_level(one_table)
        }

        "test for confirm_one_parent " in {
          one_table = utils_use.new_entry(node_one, false, one_table)
          one_table = utils_use.update_entry(node_one, monoid.id, utils_use.getLevel(one_table).get, null, one_table)
          one_table = utils_use.new_entry(node_two, false, one_table)
          one_table = utils_use.update_entry(node_one, monoid.id, utils_use.getLevel(one_table).get, node_two, one_table)
          one_table = helpers_use.confirm_one_parent(one_table)
        }

        "test for confirm_one_self " in {
          one_table = utils_use.new_entry(node_one, false, one_table)
          one_table = utils_use.update_entry(node_one, monoid.id, utils_use.getLevel(one_table).get, null, one_table)
          one_table = utils_use.new_entry(node_two, false, one_table)
          one_table = utils_use.update_entry(node_one, monoid.id, utils_use.getLevel(one_table).get, node_two, one_table)
          one_table = helpers_use.confirm_one_self(one_table)
        }

        "test for restore_table_invariant " in {

        }
    }
}