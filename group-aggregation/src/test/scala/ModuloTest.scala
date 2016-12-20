import Group.MultiModulo11
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import akka.util.Timeout
import org.scalatest.WordSpecLike
import org.scalatest.matchers.MustMatchers

class ModuloTest extends TestKit(ActorSystem("testSystem"))
  with WordSpecLike
  with MustMatchers
  with ImplicitSender
{
  "Random modulo 11 tests" must {
    "receive messages" in {
      var x: MultiModulo11 = new MultiModulo11
      x.op(5, 5) must equal(3)
      x.id must equal(1)
      x.inverse(3) must equal(4)
      x.inverse(7) must equal(8)
      x.inverse(6) must equal(2)
      x.inverse(2) must equal(6)
    }
  }
}
