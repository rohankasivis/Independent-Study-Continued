import Group.IntPlus
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import org.scalatest.WordSpecLike
import org.scalatest.matchers.MustMatchers

import scala.concurrent.Await
import scala.concurrent.duration._

// use ScalaTest in order to test the root class individually and
// all nodes that are connected to it
class RootTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender
{
  "A Root actor" must {
    // Creation of the TestActorRef
    val actorRef = TestActorRef[Root]
    var neighbors : Map[ActorRef, Set[ActorRef]] = Map.empty

    "receive messages" in {
      // This call is synchronous. The actor receive() method will be called in the current thread
      val node_one = TestActorRef[NonRoot]
      val node_two = TestActorRef[NonRoot]
      val curr:IntPlus = new IntPlus
      actorRef ! New(node_one, curr)
      actorRef ! New(node_two, curr)
      val underLyingRootActor=actorRef.underlyingActor
      underLyingRootActor.isAdjacentTo(node_one) must equal(true)
      expectMsg(true)
      underLyingRootActor.isAdjacentTo(node_two) must equal(true)
      // check the properties of node one
      val underLyingNodeOneActor =node_one.underlyingActor
      val underLyingNodeTwoActor = node_two.underlyingActor
      underLyingNodeOneActor.getBroadcast must equal(true)
      underLyingNodeOneActor.isAdjacentTo(actorRef) must equal(true)
      underLyingNodeOneActor.getLocalMass must equal(0)
      underLyingNodeOneActor.getLevelFor(actorRef) must equal(Option(0))
      underLyingNodeOneActor.getParent must equal(Option(actorRef))

      // pass a value to Root Actor
      actorRef ! Local(2, curr)

      underLyingRootActor.getLocalMass must equal(2)
      underLyingRootActor.getAggregateMass must equal(2)

      // pass a value to the child Actor

      node_one ! Local(5, curr)
      underLyingNodeOneActor.getLocalMass must equal(5)

      within(200 millis) {

        underLyingRootActor.getAggregateMass must equal(2)
      }

      // send root node to child
      node_one ! New(actorRef, curr)
      underLyingNodeOneActor.getBalanceFor(actorRef) must equal(Some(0))
      node_one ! SendAggregate(curr)
      within(200 millis) {
        underLyingRootActor.getAggregateMass must equal(7)
      }
      // make sure the root does not get aggregated again
      node_one ! SendAggregate(curr)
      within(200 millis) {
        underLyingRootActor.getAggregateMass must equal(7)
      }

      // send a new value to child
      node_two ! Local(7, curr)
      underLyingNodeTwoActor.getLocalMass must equal(7)
    }
  }
}