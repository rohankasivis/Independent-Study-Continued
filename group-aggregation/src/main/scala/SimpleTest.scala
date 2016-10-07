import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpecLike


class SimpleTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender {

  "A Root actor" must {
    // Creation of the TestActorRef
    val actorRef = TestActorRef[Root]

    "receive messages" in {
      // This call is synchronous. The actor receive() method will be called in the current thread
      val node_one = TestActorRef[NonRoot]
      //  val node_one = system.actorOf(Props(new NonRoot()), name="node_one")
      actorRef ! New(node_one)

      expectMsg(true)
    }
  }

  "A Non Root actor" must {
    // Creation of the TestActorRef
    val actorRef = TestActorRef[NonRoot]

    "receive messages" in {
      // This call is synchronous. The actor receive() method will be called in the current thread
      val node_one = system.actorOf(Props(new NonRoot()), name="node_one")
      actorRef ! New(node_one)
      expectMsg(Status)
    }
  }
}

