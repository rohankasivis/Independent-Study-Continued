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
        val root_node_second = TestActorRef(new GAPNode[Int](monoid))
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
            root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
            root_table.size must equal(2)
            root_table.get(root_node).get.get_status() must equal(Par())
        }
        "test for new: check first new on nonroot node" in {
            // test first new here - without root node
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table.size must equal(1)
            one_table.get(node_one).get.get_status() must equal(Self())
        }
        "test for new: check second new on root table (adding different elements)" in {
            // test second new here on root_table (adding different elements each time)
            root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
            root_table = utils_use.new_entry(node_one, root_node_second, true, root_table)
            root_table.size must equal(3)
            root_table.get(node_one).get.get_status() must equal(Peer())
        }
        "test for new: check adding secnd new on one_table (adding the same element twice)" in {
            // test second new here on one_table (adding the same element twice)
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table.size must equal(1)
        }

        // make sure that everything under remove works
        "test for remove: delete in root_table " in {
            // test for existing delete in root_table - size should be 1
            root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
            root_table = utils_use.new_entry(node_one, root_node_second, true, root_table)
            root_table = utils_use.remove_entry(node_one, root_table)
            root_table.size must equal(2)
        }
        "test for remove: nonexisting delete in root_table" in {
            // test for nonexisting delete in root_table - size should be 1
            root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
            root_table = utils_use.remove_entry(node_three, root_table)
            root_table.size must equal(2)
        }
        "test for remove: nonexisting delete in one_table" in {
            // test for nonexisting delete in one_table - size should be 1
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table = utils_use.remove_entry(node_two, one_table)
            one_table.size must equal(1)
        }
        "test for remove: existing delete in root_table" in {
            // test for existing delete in root_table - size should be
            root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
            root_table = utils_use.remove_entry(root_node, root_table)
            root_table.size must equal(1)
        }
        "test for remove: existing delete in one_table" in {
            // test for existing delete in one_table - size should be 0
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table = utils_use.remove_entry(node_one, root_table)
            one_table.size must equal(0)
        }

        // here are the tests for update
        "test for update: check for case where the actor to update does not exist (with root table)" in {
            // the first test simply adds elements to a root table and updates a nonexistant node with dummy values
            // no additional tests since new_entry tests for all the details
            root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
            root_table = utils_use.new_entry(node_one, root_node_second, true, root_table)
            root_table = utils_use.update_entry(node_two, root_node_second, 34, 2, root_node, root_table)
            root_table.size must equal(4)
        }
        "test for update: check for case where the actor to update does not exist (not root table)" in {
            // the first test simply adds elements to a nonroot table and updates a nonexistant node with dummy values
            // no additional tests since new_entry tests for all the details
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table = utils_use.update_entry(node_two, root_node_second, 24, 2, node_one, one_table)
            one_table.size must equal(2)
        }
        "test for update: check for case where the newly entered parent is self (in root)" in {
            root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
            root_table = utils_use.update_entry(root_node, root_node_second, 30, 2, root_node_second, root_table)
            root_table.size must equal(2)
            root_table.get(root_node_second).get.get_status() must equal(Self())
            root_table.get(root_node).get.get_status() must equal(Child())
        }
        "test for update: check for case where the newly entered parent is self (in nonroot)" in {
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table = utils_use.new_entry(node_two, root_node_second, false, one_table)
            one_table = utils_use.update_entry(node_two, root_node_second, 30, 2, node_one, one_table)
            one_table.size must equal(2)
            one_table.get(node_one).get.get_status() must equal(Self())
            one_table.get(node_two).get.get_status() must equal(Child())
        }
        "test for update: check for case where the newly entered parent is None" in {
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table = utils_use.new_entry(node_two, root_node_second, false, one_table)
            one_table = utils_use.update_entry(node_two, root_node_second, 30, 2, node_one, one_table)
            one_table.size must equal(2)
            one_table.get(node_one).get.get_status() must equal(Self())
            one_table.get(node_two).get.get_status() must equal(Child())
        }
        "test for update: check for case where the newly entered parent is not self, and current status is child (in root)" in {
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table = utils_use.new_entry(node_two, root_node_second, false, one_table)
            one_table = utils_use.update_entry(node_two, root_node_second, 30, 2, node_one, one_table)
            one_table.size must equal(2)
            one_table.get(node_one).get.get_status() must equal(Self())
            one_table.get(node_two).get.get_status() must equal(Child())
        }
        "test for update: check for case where the newly entered parent is not self, and current status is child (in nonroot)" in {
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table = utils_use.new_entry(node_two, root_node_second, false, one_table)
            one_table = utils_use.update_entry(node_two, root_node_second, 30, 2, node_one, one_table)
            one_table.size must equal(2)
            one_table.get(node_one).get.get_status() must equal(Self())
            one_table.get(node_two).get.get_status() must equal(Child())
        }
        "test for update: check for case where the newly entered parent is not self, and current status is not child (in root)" in {
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table = utils_use.new_entry(node_two, root_node_second, false, one_table)
            one_table = utils_use.update_entry(node_two, root_node_second, 30, 2, node_one, one_table)
            one_table.size must equal(2)
            one_table.get(node_one).get.get_status() must equal(Self())
            one_table.get(node_two).get.get_status() must equal(Child())
        }
        "test for update: check for case where the newly entered parent is not self, and current status is not child (in nonroot)" in {
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table = utils_use.new_entry(node_two, root_node_second, false, one_table)
            one_table = utils_use.update_entry(node_two, root_node_second, 30, 2, node_one, one_table)
            one_table.size must equal(2)
            one_table.get(node_one).get.get_status() must equal(Self())
            one_table.get(node_two).get.get_status() must equal(Child())
        }

        // level tests
        "test for level: check level in table with 1 element " in {
          one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
          utils_use.getLevel(one_table) must equal (None)
        }
        "test for level: check level in newly instantiated root table with 2 elements " in {
          root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
          utils_use.getLevel(root_table) must equal (Some(1))
        }
        "test for level: check level in a root table with multiple elements" in {
          root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
          root_table = utils_use.new_entry(node_one, root_node_second, true, root_table)
          root_table = utils_use.new_entry(node_two, root_node_second, true, root_table)
          root_table = utils_use.new_entry(node_three, root_node_second, true, root_table)
          utils_use.getLevel(root_table) must equal (Some(1))
        }
        "test for level: check level in a nonroot table with multiple elements" in {
          one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
          one_table = utils_use.new_entry(node_two, root_node_second, false, one_table)
          utils_use.getLevel(one_table) must equal (Some(1))
        }

        // parent tests
        "test for parent: make sure no parent in table with 1 element" in {
          one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
          utils_use.getParent(one_table) must equal (None)
        }
        "test for parent: check parent in new root table with 2 elements" in {
          root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
          utils_use.getParent(root_table) must equal (root_node)
        }
        "test for parent: check parent in a root table with multiple elements" in {
          root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
          root_table = utils_use.new_entry(node_one, root_node_second, true, root_table)
          root_table = utils_use.new_entry(node_two, root_node_second, true, root_table)
          root_table = utils_use.new_entry(node_three, root_node_second, true, root_table)
          utils_use.getParent(root_table) must equal (root_node)
        }
        "test for parent: check parent in a nonroot table with multiple elements" in {
          one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
          one_table = utils_use.update_entry(node_one, root_node_second, monoid.id, utils_use.getLevel(one_table).get, null, one_table)
          utils_use.getParent(one_table) must equal (Par())
        }

        // minimum tests
        "test for minimum: check min in table with 1 element" in {
          one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
          helpers_use.get_minimum(one_table) must equal (None)
        }
        "test for minimum: check min in new root table with 2 elements" in {
          root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
          helpers_use.get_minimum(root_table) must equal (Some(0))
        }
        "test for minimum: check min in a root table with multiple elements" in {
          root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
          root_table = utils_use.new_entry(node_one, root_node_second, true, root_table)
          root_table = utils_use.new_entry(node_two, root_node_second, true, root_table)
          root_table = utils_use.new_entry(node_three, root_node_second, true, root_table)
          helpers_use.get_minimum(root_table) must equal (Some(0))
        }
        "test for minimum: check min in a nonroot table with multiple elements" in {
          one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
          one_table = utils_use.update_entry(node_one, root_node_second, monoid.id, utils_use.getLevel(one_table).get, null, one_table)
          helpers_use.get_minimum(one_table) must equal (Some(1))
        }

        // overall category of helper functions
        // handle single row tests
        "test for handle_single_row: check for a table which already has self as an element " in {
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table = helpers_use.handle_single_row_table(one_table)
            one_table.size must equal (1)
            one_table.get(node_one).get.get_status() must equal (Self())
        }
        "test for handle_single_row: check for a table which does not have self as an element " in {
            root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
            root_table = utils_use.remove_entry(root_node_second, root_table)
            root_table = helpers_use.handle_single_row_table(root_table)
            root_table.size must equal(1)
            root_table.get(root_node).get.get_status() must equal (Self())
        }

        // parent min val tests
        "test for parent_min_val: check for parent in a table which already has parent as the min level (in root) " in {
            root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
            root_table = utils_use.new_entry(node_one, root_node_second, true, root_table)
            root_table = utils_use.new_entry(node_two, root_node_second, true, root_table)
            root_table = helpers_use.parent_min_val(root_table)
            root_table.size must equal (4)
            val minimum:Int = helpers_use.get_minimum(root_table).get
            val parent:ActorRef = utils_use.getParent(root_table).get
            root_table.get(parent).get.get_level().get must equal (minimum)
        }
        "test for parent_min_val: check for parent in a table which already has parent as the min level (in nonroot) " in {

        }
        "test for parent_min_val: check for parent in a table which does not have parent as min level (in root: test 1)" in {

        }
        "test for parent_min_val: check for parent in a table which does not have parent as min level (in root: test 2)" in {

        }
        "test for parent_min_val: check for parent in a table which does not have parent as min level (in nonroot: test 1)" in {
            // not working yet
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table = utils_use.new_entry(node_two, root_node_second, false, one_table)
            one_table = utils_use.new_entry(node_three, root_node_second, false, one_table)
            one_table = helpers_use.parent_min_val(one_table)
            one_table.size must equal (3)
            val minimum:Int = helpers_use.get_minimum(one_table).get
            val parent:ActorRef = utils_use.getParent(one_table).get
            one_table.get(parent).get.get_level().get must equal (minimum)
        }
        "test for parent_min_val: check for parent in a table which does not have parent as min level (in nonroot: test 2)" in {

        }

        // handle self level tests
        "test for handle_self_level: check for self status in a table where minimum + 1 level element has status self (in root) " in {
            root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
            root_table = utils_use.new_entry(node_one, root_node_second, true, root_table)
            root_table = utils_use.new_entry(node_two, root_node_second, true, root_table)
            root_table = helpers_use.handle_self_level(root_table)
            root_table.size must equal (4)
            val minimum:Int = helpers_use.get_minimum(root_table).get
            val self:ActorRef = utils_use.getSelf(root_table).get
            root_table.get(self).get.get_level().get must equal (minimum + 1)
        }
        "test for handle_self_level: check for self status in a table where minimum + 1 level element has status self (in nonroot) " in {

        }
        "test for handle_self_level: check for self status in a table where minimum + 1 does not have self (in root: test 1)" in {

        }
        "test for handle_self_level: check for self status in a table where minimum + 1 does not have self (in root: test 2)" in {

        }
        "test for handle_self_level: check for self status in a table where minimum + 1 does not have self (in nonroot: test 1)" in {

        }
        "test for handle_self_level: check for self status in a table where minimum + 1 does not have self (in nonroot: test 2)" in {

        }

        // confirm one parent tests
        "test for confirm_one_parent: only one parent already exists (in root) " in {
            root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
            root_table = utils_use.new_entry(node_one, root_node_second, true, root_table)
            root_table = utils_use.new_entry(node_two, root_node_second, true, root_table)
            root_table = helpers_use.confirm_one_parent(root_table)
            root_table.size must equal (4)
            var count:Int = 0
            for(value <- root_table.keys)
            {
                if(root_table.get(value).get.get_status() == Par())
                    count += 1
            }
            count must equal (1)
        }
        "test for confirm_one_parent: only one parent already exists (in nonroot) " in {

        }
        "test for confirm_one_parent: more than one parent exists (test 1: in root)" in {

        }
        "test for confirm_one_parent: more than one parent exists (test 2: in root)" in {

        }
        "test for confirm_one_parent: more than one parent exists (test 3: in root)" in {

        }
        "test for confirm_one_parent: more than one parent exists (test 1: in nonroot)" in {

        }
        "test for confirm_one_parent: more than one parent exists (test 2: in nonroot)" in {

        }
        "test for confirm_one_parent: more than one parent exists (test 3: in nonroot)" in {

        }

        // confirm one self tests
        "test for confirm_one_self: only one self already exists (in root) " in {
            root_table = utils_use.new_entry(root_node, root_node_second, true, root_table)
            root_table = utils_use.new_entry(node_one, root_node_second, true, root_table)
            root_table = utils_use.new_entry(node_two, root_node_second, true, root_table)
            root_table = helpers_use.confirm_one_self(root_table)
            root_table.size must equal (4)
            var count:Int = 0
            for(value <- root_table.keys)
            {
                if(root_table.get(value).get.get_status() == Self())
                    count += 1
            }
            count must equal (1)
        }
        "test for confirm_one_self: only one self already exists (in nonroot) " in {
            // not working
            one_table = utils_use.new_entry(node_one, root_node_second, false, one_table)
            one_table = utils_use.new_entry(node_two, root_node_second, false, one_table)
            one_table = utils_use.new_entry(node_three, root_node_second, false, one_table)
            one_table = helpers_use.confirm_one_self(one_table)
            one_table.size must equal (3)
            var count:Int = 0
            for(value <- one_table.keys)
            {
                if(one_table.get(value).get.get_status() == Self())
                    count += 1
            }
            count must equal (1)
        }
        "test for confirm_one_self: more than one self exists (test 1: in root)" in {

        }
        "test for confirm_one_self: more than one self exists (test 2: in root)" in {

        }
        "test for confirm_one_self: more than one self exists (test 3: in root)" in {

        }
        "test for confirm_one_self: more than one self exists (test 1: in nonroot)" in {

        }
        "test for confirm_one_self: more than one self exists (test 2: in nonroot)" in {

        }
        "test for confirm_one_self: more than one self exists (test 3: in nonroot)" in {

        }

        // back to main function: restore_table_invariant
        // all needed tests for this
        "test for restore_table_invariant: table does not need changes (test 1 for root)" in {

        }
        "test for restore_table_invariant: table does not need changes (test 2 for root)" in {

        }
        "test for restore_table_invariant: table does not need changes (test 1 for nonroot)" in {

        }
        "test for restore_table_invariant: table does not need changes (test 2 for nonroot)" in {

        }
        "test for restore_table_invariant: table does need changes (test 1 for root)" in {

        }
        "test for restore_table_invariant: table does need changes (test 2 for root)" in {

        }
        "test for restore_table_invariant: table does need changes (test 3 for root)" in {

        }
        "test for restore_table_invariant: table does need changes (test 4 for root)" in {

        }
        "test for restore_table_invariant: table does need changes (test 1 for nonroot)" in {

        }
        "test for restore_table_invariant: table does need changes (test 2 for nonroot)" in {

        }
        "test for restore_table_invariant: table does need changes (test 3 for nonroot)" in {

        }
        "test for restore_table_invariant: table does not need changes (test 4 for nonroot)" in {

        }
    }
}