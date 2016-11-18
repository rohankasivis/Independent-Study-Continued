package Group

import scala.annotation.tailrec

trait AdditiveGroup[@(Byte, Short, Int, Long, Float, Double) A] extends Any {
  def additive: Group[A] = new Group[A] {
    def id: A = zero
    def op(x: A, y: A): A = plus(x, y)
    def inverse(x: A): A = negate(x)
  }

  def zero: A
  def plus(x: A, y: A): A
  protected def sumnAboveOne(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) {
        plus(b, extra)
      } else {
        val x = if ((k & 1) == 1) plus(b, extra) else extra
        loop(plus(b, b), k >>> 1, x)
      }
    loop(a, n - 1, a)
  }

  def negate(x: A): A
  def minus(x: A, y: A): A = plus(x, negate(y))

  /**
    * Return `a` added with itself `n` times.
    */
  def sumn(a: A, n: Int): A =
  if (n == Int.MinValue) plus(sumn(negate(a), Int.MaxValue), negate(a))
  else if (n < 0) sumn(negate(a), -n)
  else if (n == 0) zero
  else if (n == 1) a
  else sumnAboveOne(a, n)
}