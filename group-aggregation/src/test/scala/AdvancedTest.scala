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
      underLyingNodeOneActor.getSentMassTo(actorRef) must equal(Some(0))
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
      val underlyingfournode = node_four.underlyingActor
      val underlyingfivenode = node_five.underlyingActor
      val underlyingsixnode = node_six.underlyingActor
      val underlyingsevennode = node_seven.underlyingActor
      val underlyingeightnode = node_eight.underlyingActor
      val underlyingninenode = node_nine.underlyingActor
      val underlyingtennode = node_ten.underlyingActor

      actorRef ! New(node_one)
      actorRef ! New(node_two)
      node_one ! New(actorRef)
      node_one ! New(node_three)
      node_two ! New(actorRef)
      node_two ! New(node_two)
      node_two ! New(node_four)
      node_three ! New(node_one)
      node_three ! New(node_two)
      node_three ! New(node_five)
      node_four ! New(node_one)
      node_four ! New(node_two)
      node_four ! New(node_three)
      node_four ! New(node_six)
      node_four ! New (node_seven)
      node_four ! New (node_eight)
      node_five ! New(node_two)
      node_five ! New(node_three)
      node_five ! New(node_four)
      node_five ! New(node_eight)
      node_five ! New(node_nine)
      node_six ! New(node_two)
      node_six ! New(node_three)
      node_six ! New(node_four)
      node_six ! New(node_five)
      node_six ! New(node_ten)
      node_seven ! New(node_one)
      node_seven ! New(node_three)
      node_seven ! New(node_four)
      node_seven !  New(node_five)
      node_seven ! New(node_six)
      node_eight ! New(node_one)
      node_eight ! New(node_four)
      node_eight ! New(node_five)
      node_eight ! New(node_seven)

      node_nine ! New(node_two)
      node_nine ! New(node_five)
      node_nine ! New(node_six)
      node_nine ! New(node_seven)
      node_ten  ! New(node_three)
      node_ten  ! New(node_six)
      node_ten  ! New(node_nine)

      actorRef ! Local(2)
      originalunderlyingActor.getLocalMass must equal (2)
      originalunderlyingActor.getAggregateMass must equal (2)

      node_one ! Local(5)
      underlyingonenode.getLocalMass must equal (5)

      node_two ! Local(10)
      underlyingtwonode.getLocalMass must equal(10)

      node_three ! Local(7)
      underlyingthreenode.getLocalMass must equal(7)

      node_four ! Local(13)

      node_five ! Local(4)
      node_six ! Local(6)
      node_seven ! Local(17)
      node_eight ! Local(23)
      node_nine ! Local(11)
      node_ten ! Local(39)

      node_one ! sendToSelf()
      node_two ! sendToSelf()
      node_three ! sendToSelf()
      node_four ! sendToSelf()
      node_five ! sendToSelf()
      node_six ! sendToSelf()
      node_seven ! sendToSelf()
      node_eight ! sendToSelf()
      node_nine ! sendToSelf()
      node_ten ! sendToSelf()

      node_one ! sendBroadcast()
      node_two ! sendBroadcast()
      node_three ! sendBroadcast()
      node_four ! sendBroadcast()
      node_five ! sendBroadcast()
      node_six ! sendBroadcast()
      node_seven ! sendBroadcast()
      node_eight ! sendBroadcast()
      node_nine ! sendBroadcast()
      node_ten ! sendBroadcast()

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