import Group.IntPlus
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import akka.util.Timeout
import org.scalatest.WordSpecLike
import org.scalatest.matchers.MustMatchers

import scala.concurrent.duration._

class SimpleTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender {

  //override def registerIgnoredTest(testText: String, testTags: Tag*)(testFun: => Any)(implicit pos: Any): Unit = ???

  "A Root actor" must {
    val grp=new IntPlus
    // Creation of the TestActorRef
    val actorRef =TestActorRef(new Root[Int](grp))

    var neighbors : Map[ActorRef, Set[ActorRef]] = Map.empty

    val curr: IntPlus = new IntPlus

    "receive messages" in {
      // This call is synchronous. The actor receive() method will be called in the current thread
      val node_one = TestActorRef(new NonRoot[Int](grp))

      actorRef ! New(node_one)

      val underLyingRootActor=actorRef.underlyingActor
      underLyingRootActor.isAdjacentTo(node_one)
      expectMsg(true)
      // check the properties of node one
      val underLyingNodeOneActor =node_one.underlyingActor
      underLyingNodeOneActor.getBroadcast must equal(true)
      underLyingNodeOneActor.isAdjacentTo(actorRef)
      underLyingNodeOneActor.getLocalMass must equal(0)
      underLyingNodeOneActor.getLevelFor(actorRef) must equal(Option(0))
      underLyingNodeOneActor.getParent must equal(Option(actorRef))

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
      underLyingNodeOneActor.getBalanceFor(actorRef) must equal(Some(0))
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
    val grp=new IntPlus
    // Creation of the TestActorRef
    val actorRef =TestActorRef(new Root[Int](grp))
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

      Thread.sleep(5000)

      originalunderlyingActor.getAggregateMass must equal(24)

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