package Group

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import org.scalatest.WordSpecLike
import org.scalatest.matchers.MustMatchers

// this class is used to test inline multiplications with the group trait
class MultiplicativeTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender
{
  "An instance of multiplicative group" must
    {
      implicit val IntSemigroup = new MultiplicativeGroup[Double]{
        def times(x: Double, y: Double): Double = x.asInstanceOf[Double] * y.asInstanceOf[Double]    // multiplying two ints by each other
        def div(x: Double, y: Double): Double = x.asInstanceOf[Double] * 1 / y.asInstanceOf[Double]
        def one: Int = 1    // id for multiplication is 1
        override def reciprocal(x: Double): Double = 1 / x.asInstanceOf[Double]
      }
      "receive messages" in {
        val curr: Group[Double] = Group.multiplicative[Double]
        curr.id.asInstanceOf[Int] must equal(1)
        curr.op(6, 7) must equal(42)
        curr.inverse(6) must equal(1.toDouble / 6.toDouble) // since integers here
        curr.op(6, curr.inverse(6)) must equal(1)
      }
    }
}