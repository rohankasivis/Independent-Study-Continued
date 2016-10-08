import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import org.scalatest.WordSpecLike
import org.scalatest.matchers.MustMatchers
import scala.concurrent.duration._


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

      actorRef ! New(node_one)

      val underLyingRootActor=actorRef.underlyingActor
      underLyingRootActor.getAdjacent.contains(node_one)
      expectMsg(true)
      // check the properties of node one
      val underLyingNodeOneActor =node_one.underlyingActor
      underLyingNodeOneActor.getBroadCast must equal(true)
      underLyingNodeOneActor.getAdjacent.contains(actorRef)
      underLyingNodeOneActor.getLocalMass must equal(0)
      underLyingNodeOneActor.getLevels.get(actorRef) must equal(Option(0))
      underLyingNodeOneActor.parent(underLyingNodeOneActor.getAdjacent, underLyingNodeOneActor.getLevels) must equal(Option(actorRef))

      // pass a value to Root Actor
      actorRef ! Local(2)

      underLyingRootActor.getLocalMass must equal(2)
      underLyingRootActor.getAggregateMass must equal(2)

      // pass a value to the child Actor


      node_one ! Local(5)
      underLyingNodeOneActor.getLocalMass must equal(5)
      within(200 millis) {

        underLyingRootActor.getAggregateMass must equal(2)
      }

      // send root node to child
      node_one ! New(actorRef)
      underLyingNodeOneActor.getSentMass(actorRef) must equal(0)
      node_one ! SendAggregate()
      within(200 millis) {
        underLyingRootActor.getAggregateMass must equal(7)
      }
      // make sure the root does not get aggregated again
      node_one ! SendAggregate()
      within(200 millis) {
        underLyingRootActor.getAggregateMass must equal(7)
      }

      // send a new value to child
      node_one ! Local(7)
      underLyingNodeOneActor.getLocalMass must equal(7)
      node_one ! SendAggregate()
      within(200 millis) {
        underLyingRootActor.getAggregateMass must equal(9)
      }
    }
  }

  "A Non Root actor" must {
    // Creation of the TestActorRef
    val actorRef = TestActorRef[NonRoot]

    "receive messages" in {
      // This call is synchronous. The actor receive() method will be called in the current thread
      val node_one = system.actorOf(Props(new NonRoot()), name="node_one")
      actorRef ! New(node_one)
      expectMsg(true)
      actorRef.underlyingActor.getAdjacent.contains(node_one)

    }
  }

}

