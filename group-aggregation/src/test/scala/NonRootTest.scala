import Group.IntPlus
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import akka.util.Timeout
import org.scalatest.WordSpecLike
import org.scalatest.matchers.MustMatchers

import scala.concurrent.Await
import scala.concurrent.duration._

class NonRootTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender
{
  "A Non Root actor" must {
    // Creation of the TestActorRef
    val actorRef = TestActorRef[Root]
    var neighbors : Map[ActorRef, Set[ActorRef]] = Map.empty

    "receive messages" in {
      // This call is synchronous. The actor receive() method will be called in the current thread
      val node_one = TestActorRef[NonRoot]
      //val node_one = system.actorOf(Props(new NonRoot()), name = "node_one")
      val node_two = TestActorRef[NonRoot]
      //val node_two = system.actorOf(Props(new NonRoot()), name = "node_two")
      val node_three = TestActorRef[NonRoot]
      //val node_three = system.actorOf(Props(new NonRoot()), name = "node_three")
      val curr: IntPlus = new IntPlus
      implicit val timeout = Timeout(0 seconds)
      actorRef ! New(node_one, curr)

      val originalunderlyingActor = actorRef.underlyingActor
      within(200 milliseconds) {
        originalunderlyingActor.isAdjacentTo(node_one)
        expectMsg(true)
      }

      actorRef ! New(node_two, curr)

      within(200 milliseconds) {
        originalunderlyingActor.isAdjacentTo(node_two)
        expectMsg(true)
      }

      val underlyingonenode = node_one.underlyingActor
      val underlyingtwonode = node_two.underlyingActor
      val underlyingthreenode = node_three.underlyingActor

      node_one ! New(actorRef, curr)

      within(200 milliseconds) {
        underlyingonenode.isAdjacentTo(actorRef)
        expectMsg(true)
      }

      node_one ! New(node_three, curr)
      // val result_onethree = Await.ready(future_onethree, timeout.duration)
      within(200 milliseconds) {
        underlyingonenode.isAdjacentTo(node_three)
        expectMsg(true)
      }

      node_two ! New(actorRef, curr)
      within(200 milliseconds) {
        underlyingtwonode.isAdjacentTo(actorRef)
        expectMsg(true)
      }

      node_two ! New(node_two, curr)
      within(200 milliseconds) {
        underlyingtwonode.isAdjacentTo(node_three)
        expectMsg(true)
      }

      node_three ! New(node_one, curr)
      within(200 milliseconds) {
        underlyingthreenode.isAdjacentTo(node_one)
        expectMsg(true)
      }

      node_three ! New(node_two, curr)
      within(200 milliseconds) {
        underlyingthreenode.isAdjacentTo(node_two)
        expectMsg(true)
      }

      actorRef ! Local(2, curr)
      originalunderlyingActor.getLocalMass must equal (2)
      originalunderlyingActor.getAggregateMass must equal (2)

      node_one ! Local(5, curr)
      underlyingonenode.getLocalMass must equal (5)

      node_two ! Local(10, curr)
      underlyingtwonode.getLocalMass must equal(10)

      node_three ! Local(7, curr)
      underlyingthreenode.getLocalMass must equal(7)

      node_one ! sendToSelf(curr)
      node_two ! sendToSelf(curr)
      node_three ! sendToSelf(curr)

      node_one ! sendBroadcast(curr)
      node_two ! sendBroadcast(curr)
      node_three ! sendBroadcast(curr)

      // over here, set up the connection map
      neighbors = neighbors + (node_one -> Set(actorRef, node_three))
      neighbors = neighbors + (node_two -> Set(actorRef, node_two))
      neighbors = neighbors + (node_three -> Set(node_one, node_two))

      // over here, we go through all of the individual nodes and send the fail messages appropriately
      neighbors.get(node_one) match {
        case Some(s) => s.foreach { n => node_one ! Fail(n, curr) }
        case None => ()
      }
      //system stop node_one
      neighbors = neighbors - node_one
      neighbors.contains(node_one) must equal (false)

      neighbors.get(node_two) match {
        case Some(s) => s.foreach { n => node_two ! Fail(n, curr) }
        case None => ()
      }

      neighbors = neighbors - node_two
      neighbors.contains(node_two) must equal (false)

      neighbors.get(node_three) match {
        case Some(s) => s.foreach { n => node_three ! Fail(n, curr) }
        case None => ()
      }

      neighbors = neighbors - node_three
      neighbors.contains(node_three) must equal(false)
    }
  }
}