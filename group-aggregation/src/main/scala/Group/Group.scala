package Group

import scala.annotation.tailrec

trait Group[@(Byte, Short, Int, Long, Float, Double) A] extends Any
{
  // id function
  def id: A

  def combinen(a: A, n: Int): A =
    if (n == Int.MinValue) op(combinen(inverse(a), Int.MaxValue), inverse(a))
    else if (n < 0) combinen(inverse(a), -n)
    else if (n == 0) id
    else if (n == 1) a
    else combinenAboveOne(a, n)

  def combinenAboveOne(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) {
        op(b, extra)
      } else {
        val x = if ((k & 1) == 1) op(b, extra) else extra
        loop(op(b, b), k >>> 1, x)
      }
    loop(a, n - 1, a)
  }

  def inverse(a:A): A

  def op(x: A, y: A): A

  def opInverse(a: A, b: A): A = op(a, inverse(b))
}

object Group {
  @inline final def apply[A](implicit ev: Group[A]): Group[A] = ev
  @inline final def additive[A](implicit A: AdditiveGroup[A]): Group[A] =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeGroup[A]): Group[A] = A.multiplicative
}