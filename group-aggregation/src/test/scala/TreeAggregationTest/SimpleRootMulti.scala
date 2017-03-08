import Group.IntMulti
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import akka.util.Timeout
import org.scalatest.WordSpecLike
import org.scalatest.matchers.MustMatchers

class SimpleRootMulti extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender {
  "A Root" must {
    "receive messages" in {
      val grp = new IntMulti
      val n1 = TestActorRef(new Root[Int](grp))
      val n1act = n1.underlyingActor
      val n2 = TestActorRef(new NonRoot[Int](grp))
      val n2act = n2.underlyingActor
      n1 ! New(n2)
      n1act.getBalanceFor(n2) must equal(Some(1))
    }
  }
}