package Group

import scala.annotation.tailrec

trait MultiplicativeGroup[@(Byte, Short, Int, Long, Float, Double) A] extends Any {
  def multiplicative: Group[A] = new Group[A] {
    def id: A = one
    def op(x: A, y: A): A = times(x, y)
    def inverse(x: A): A = reciprocal(x)
  }

  def one: A
  def times(x: A, y: A): A

  def reciprocal(x: A): A = div(one, x)
  def div(x: A, y: A): A

  protected def prodnAboveOne(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) {
        times(b, extra)
      } else {
        val x = if ((k & 1) == 1) times(b, extra) else extra
        loop(times(b, b), k >>> 1, x)
      }
    loop(a, n - 1, a)
  }


  /**
    * Return `a` multiplicated with itself `n` times.
    */
  def prodn(a: A, n: Int): A =
  if (n == Int.MinValue) times(prodn(reciprocal(a), Int.MaxValue), reciprocal(a))
  else if (n < 0) prodn(reciprocal(a), -n)
  else if (n == 0) one
  else if (n == 1) a
  else prodnAboveOne(a, n)
}