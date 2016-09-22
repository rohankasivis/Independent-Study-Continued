/*
* This is the base test class that makes sure that the basic aggregate/local/fail
* case classes work properly in order to produce the correct result.
* */

import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout
import akka.pattern.ask

import scala.concurrent.Await
import scala.concurrent.duration._

object NodeActorTest extends App
{
  var neighbors : Map[ActorRef, Set[ActorRef]] = Map.empty
  val system = ActorSystem("NeighborSetSystem")

  val root_node = system.actorOf(Props[Root], name="root_node")
  val node_one = system.actorOf(Props(new NonRoot()), name="node_one")
  val node_two = system.actorOf(Props(new NonRoot()), name="node_two")
  val node_three = system.actorOf(Props(new NonRoot()), name="node_three")

  implicit val timeout = Timeout(5 seconds)
  val future_rootone = root_node ? New(node_one)
  val result_rootone = Await.ready(future_rootone, timeout.duration)
  val future_roottwo = root_node ? New(node_two)
  val result_roottwo = Await.ready(future_roottwo, timeout.duration)

  val future_oneroot = node_one ? New(root_node)
  val result_oneroot = Await.ready(future_oneroot, timeout.duration)
  val future_onethree = node_one ? New(node_three)
  val result_onethree = Await.ready(future_onethree, timeout.duration)

  val future_tworoot = node_two ? New(root_node)
  val result_tworoot = Await.ready(future_tworoot, timeout.duration)
  val future_twothree = node_two ? New(node_two)
  val result_twothree = Await.ready(future_twothree, timeout.duration)

  val future_threeone = node_three ? New(node_one)
  val result_threeone = Await.ready(future_threeone, timeout.duration)
  val future_threetwo = node_three ? New(node_two)
  val result_threetwo = Await.ready(future_threetwo, timeout.duration)

  // just check with four local statements and make sure the summation takes place correctly
  root_node ! Local(2)
  node_one ! Local(5)
  node_two ! Local(10)
  node_three ! Local(7)

  import system.dispatcher
  val cancellable =
    system.scheduler.schedule(
      0 milliseconds,
      1 second,
      node_one,
      SendAggregate())
  val canc_two =
    system.scheduler.schedule(
      0 milliseconds,
      1 second,
      node_two,
      SendAggregate())
  val broad_two =
    system.scheduler.schedule(
      0 milliseconds,
      1 second,
      node_three,
      SendAggregate())

  cancellable.cancel()
  canc_two.cancel()
  //  broad_one.cancel()
  broad_two.cancel()
  // remove node two

  node_one ! sendBroadcast()
  node_two ! sendBroadcast()
  node_three ! sendBroadcast()

  // over here, set up the connection map
  neighbors = neighbors + (node_one -> Set(root_node, node_three))
  neighbors = neighbors + (node_two -> Set(root_node, node_two))
  neighbors = neighbors + (node_three -> Set(node_one, node_two))

  // over here, we go through all of the individual nodes and send the fail messages appropriately
  neighbors.get(node_one) match {
    case Some(s) => s.foreach { n => node_one ! Fail(n) }
    case None => ()
  }
  //system stop node_one
  neighbors = neighbors - node_one


  neighbors.get(node_two) match {
    case Some(s) => s.foreach { n => node_two ! Fail(n) }
    case None => ()
  }
  //system stop node_two
  neighbors = neighbors - node_two

  neighbors.get(node_three) match {
    case Some(s) => s.foreach { n => node_three ! Fail(n) }
    case None => ()
  }
  //system stop node_three
  neighbors = neighbors - node_three
}