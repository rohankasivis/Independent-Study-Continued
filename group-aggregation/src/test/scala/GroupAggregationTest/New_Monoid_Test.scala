import Group.IntPlus
import Monoid.{IntAddition, MaxMonoid, MinMonoid, MultiModuloEleven}
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import akka.util.Timeout
import org.scalatest.WordSpecLike
import org.scalatest.matchers.MustMatchers

class New_Monoid_Test extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender
{
  "The Monoid class test cases" must {
  "add properly: " in {
    // test_add_id
    val test_add = new IntAddition
    test_add.id must equal(0)

    // test_add_op
    val result: Int = test_add.op(5, 6)
    result must equal(11)
  }
    "modulo works properly: " in {
      // test_modulo_id
      val test_modulo = new MultiModuloEleven
      test_modulo.id must equal(1)

      // test_modulo_op
      val first_res: Int = test_modulo.op(2, 4)
      first_res must equal(8)

      val second_res: Int = test_modulo.op(first_res, 2)
      second_res must equal(5)
    }
    "test max" in {
      // test max id
      val max_test = new MaxMonoid
      max_test.id must equal(Int.MinValue)

      // test max op
      val max_one = max_test.op(5, 9)
      max_one must equal(9)

      val max_two = max_test.op(-5, -10)
      max_two must equal(-5)
    }

    "test min" in {
      // test min id
      val min_test = new MinMonoid
      min_test.id must equal(Int.MaxValue)

      // test min op
      val min_one = min_test.op(5, 9)
      min_one must equal(5)

      val min_two = min_test.op(-5, -10)
      min_two must equal(-10)
    }
  }

}