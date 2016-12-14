import Group.IntPlus
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import akka.util.Timeout
import org.scalatest.WordSpecLike
import org.scalatest.matchers.MustMatchers

import scala.concurrent.duration._

class NonRootTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender
{
  "A Non Root actor" must {
    val grp=new IntPlus
    // Creation of the TestActorRef
    val actorRef = TestActorRef(new Root[Int](grp))
    var neighbors : Map[ActorRef, Set[ActorRef]] = Map.empty

    "receive messages" in {
      // This call is synchronous. The actor receive() method will be called in the current thread
      val node_one = TestActorRef(new NonRoot[Int](grp))
      //val node_one = system.actorOf(Props(new NonRoot()), name = "node_one")
      val node_two = TestActorRef(new NonRoot[Int](grp))
      //val node_two = system.actorOf(Props(new NonRoot()), name = "node_two")
      val node_three = TestActorRef(new NonRoot[Int](grp))
      //val node_three = system.actorOf(Props(new NonRoot()), name = "node_three")
      val curr: IntPlus = new IntPlus
      implicit val timeout = Timeout(0 seconds)
      actorRef ! New(node_one)

      val originalunderlyingActor = actorRef.underlyingActor
      within(200 milliseconds) {
        originalunderlyingActor.isAdjacentTo(node_one)
        expectMsg(true)
      }

      actorRef ! New(node_two)

      within(200 milliseconds) {
        originalunderlyingActor.isAdjacentTo(node_two)
        expectMsg(true)
      }

      val underlyingonenode = node_one.underlyingActor
      val underlyingtwonode = node_two.underlyingActor
      val underlyingthreenode = node_three.underlyingActor

      node_one ! New(actorRef)

      within(200 milliseconds) {
        underlyingonenode.isAdjacentTo(actorRef)
        expectMsg(true)
      }

      node_one ! New(node_three)
      // val result_onethree = Await.ready(future_onethree, timeout.duration)
      within(200 milliseconds) {
        underlyingonenode.isAdjacentTo(node_three)
        expectMsg(true)
      }

      node_two ! New(actorRef)
      within(200 milliseconds) {
        underlyingtwonode.isAdjacentTo(actorRef)
        expectMsg(true)
      }

      node_two ! New(node_two)
      within(200 milliseconds) {
        underlyingtwonode.isAdjacentTo(node_three)
        expectMsg(true)
      }

      node_three ! New(node_one)
      within(200 milliseconds) {
        underlyingthreenode.isAdjacentTo(node_one)
        expectMsg(true)
      }

      node_three ! New(node_two)
      within(200 milliseconds) {
        underlyingthreenode.isAdjacentTo(node_two)
        expectMsg(true)
      }

      actorRef ! Local(2)
      originalunderlyingActor.getLocalMass must equal (2)
      originalunderlyingActor.getAggregateMass must equal (2)

      node_one ! Local(5)
      underlyingonenode.getLocalMass must equal (5)

      node_two ! Local(10)
      underlyingtwonode.getLocalMass must equal(10)

      node_three ! Local(7)
      underlyingthreenode.getLocalMass must equal(7)

      node_one ! sendToSelf()
      node_two ! sendToSelf()
      node_three ! sendToSelf()

      node_one ! sendBroadcast()
      node_two ! sendBroadcast()
      node_three ! sendBroadcast()

      // over here, set up the connection map
      neighbors = neighbors + (node_one -> Set(actorRef, node_three))
      neighbors = neighbors + (node_two -> Set(actorRef, node_two))
      neighbors = neighbors + (node_three -> Set(node_one, node_two))

      // over here, we go through all of the individual nodes and send the fail messages appropriately
      neighbors.get(node_one) match {
        case Some(s) => s.foreach { n => node_one ! Fail(n) }
        case None => ()
      }
      //system stop node_one
      neighbors = neighbors - node_one
      neighbors.contains(node_one) must equal (false)

      neighbors.get(node_two) match {
        case Some(s) => s.foreach { n => node_two ! Fail(n) }
        case None => ()
      }

      neighbors = neighbors - node_two
      neighbors.contains(node_two) must equal (false)

      neighbors.get(node_three) match {
        case Some(s) => s.foreach { n => node_three ! Fail(n) }
        case None => ()
      }

      neighbors = neighbors - node_three
      neighbors.contains(node_three) must equal(false)
    }
  }
}