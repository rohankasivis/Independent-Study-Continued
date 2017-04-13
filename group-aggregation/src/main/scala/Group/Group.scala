package Group

import Monoid.Monoid

trait Group[A] extends Monoid[A]
{
  // three main functions: id, inverse, and op
  def inverse(a:A): A
}