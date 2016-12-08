import Group.IntPlus
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import akka.util.Timeout
import org.scalatest.WordSpecLike
import org.scalatest.matchers.MustMatchers

import scala.concurrent.Await
import scala.concurrent.duration._

class AdvancedTest extends TestKit(ActorSystem("testSystem"))
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

      val curr:IntPlus = new IntPlus
      actorRef ! New(node_one, curr)

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
      node_one ! Local(7, curr)
      underLyingNodeOneActor.getLocalMass must equal(7)
      node_one ! SendAggregate(curr)
      within(200 millis) {
        underLyingRootActor.getAggregateMass must equal(9)
      }
    }
  }

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
      val node_four = TestActorRef[NonRoot]
      val node_five = TestActorRef[NonRoot]
      val node_six = TestActorRef[NonRoot]
      val node_seven = TestActorRef[NonRoot]
      val node_eight = TestActorRef[NonRoot]
      val node_nine = TestActorRef[NonRoot]
      val node_ten = TestActorRef[NonRoot]

      val curr:IntPlus = new IntPlus
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
      val underlyingfournode = node_four.underlyingActor
      val underlyingfivenode = node_five.underlyingActor
      val underlyingsixnode = node_six.underlyingActor
      val underlyingsevennode = node_seven.underlyingActor
      val underlyingeightnode = node_eight.underlyingActor
      val underlyingninenode = node_nine.underlyingActor
      val underlyingtennode = node_ten.underlyingActor

      actorRef ! New(node_one, curr)
      actorRef ! New(node_two, curr)
      node_one ! New(actorRef, curr)
      node_one ! New(node_three, curr)
      node_two ! New(actorRef, curr)
      node_two ! New(node_two, curr)
      node_two ! New(node_four, curr)
      node_three ! New(node_one, curr)
      node_three ! New(node_two, curr)
      node_three ! New(node_five, curr)
      node_four ! New(node_one, curr)
      node_four ! New(node_two, curr)
      node_four ! New(node_three, curr)
      node_four ! New(node_six, curr)
      node_four ! New (node_seven, curr)
      node_four ! New (node_eight, curr)
      node_five ! New(node_two, curr)
      node_five ! New(node_three, curr)
      node_five ! New(node_four, curr)
      node_five ! New(node_eight, curr)
      node_five ! New(node_nine, curr)
      node_six ! New(node_two, curr)
      node_six ! New(node_three, curr)
      node_six ! New(node_four, curr)
      node_six ! New(node_five, curr)
      node_six ! New(node_ten, curr)
      node_seven ! New(node_one, curr)
      node_seven ! New(node_three, curr)
      node_seven ! New(node_four, curr)
      node_seven !  New(node_five, curr)
      node_seven ! New(node_six, curr)
      node_eight ! New(node_one, curr)
      node_eight ! New(node_four, curr)
      node_eight ! New(node_five, curr)
      node_eight ! New(node_seven, curr)

      node_nine ! New(node_two, curr)
      node_nine ! New(node_five, curr)
      node_nine ! New(node_six, curr)
      node_nine ! New(node_seven, curr)
      node_ten  ! New(node_three, curr)
      node_ten  ! New(node_six, curr)
      node_ten  ! New(node_nine, curr)

      actorRef ! Local(2, curr)
      originalunderlyingActor.getLocalMass must equal (2)
      originalunderlyingActor.getAggregateMass must equal (2)

      node_one ! Local(5, curr)
      underlyingonenode.getLocalMass must equal (5)

      node_two ! Local(10, curr)
      underlyingtwonode.getLocalMass must equal(10)

      node_three ! Local(7, curr)
      underlyingthreenode.getLocalMass must equal(7)

      node_four ! Local(13, curr)

      node_five ! Local(4, curr)
      node_six ! Local(6, curr)
      node_seven ! Local(17, curr)
      node_eight ! Local(23, curr)
      node_nine ! Local(11, curr)
      node_ten ! Local(39, curr)

      node_one ! sendToSelf(curr)
      node_two ! sendToSelf(curr)
      node_three ! sendToSelf(curr)
      node_four ! sendToSelf(curr)
      node_five ! sendToSelf(curr)
      node_six ! sendToSelf(curr)
      node_seven ! sendToSelf(curr)
      node_eight ! sendToSelf(curr)
      node_nine ! sendToSelf(curr)
      node_ten ! sendToSelf(curr)

      Thread.sleep(12000)
      originalunderlyingActor.getAggregateMass must equal(137)
      /*within(60 seconds,80 seconds) {
       //  expectNoMsg
         originalunderlyingActor.getAggregateMass must equal(137)
       }*/

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