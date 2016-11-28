package Group

import scala.annotation.tailrec

trait Group[@specialized(Byte, Short, Int, Long, Float, Double) A] extends Any
{
  // three main functions: id, inverse, and op
  def id: Int
  def inverse(a:A): Double
  def op(x: A, y: A): Double
}

object Group {
  @inline final def apply[A](implicit ev: Group[A]): Group[A] = ev
  @inline final def additive[A](implicit A: AdditiveGroup[A]): Group[A] = A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeGroup[A]): Group[A] = A.multiplicative
}