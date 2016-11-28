package Group

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import org.scalatest.WordSpecLike
import org.scalatest.matchers.MustMatchers

// this class is used to test inline additions with the group trait
class AdditiveTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender
{
  "An additive instance of Group" must {
    implicit val IntAdditivegroup = new AdditiveGroup[Double]{
      def plus(x: Double, y: Double): Double = x.asInstanceOf[Double] + y.asInstanceOf[Double]
      def minus(x: Double, y: Double): Double = x.asInstanceOf[Double] - y.asInstanceOf[Double]
      def zero: Int = 0
      def negate(x: Double): Double = x.asInstanceOf[Double] * -1
    }
    "receive messages" in {
      val curr = Group.additive[Double]
      curr.id.asInstanceOf[Int] must equal(0)
      curr.op(6, 7) must equal(13)
      curr.inverse(6) must equal(-6)
      curr.op(6, curr.inverse(6)) must equal(0)
    }
  }
}
