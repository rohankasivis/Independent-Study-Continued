package Monoid

class MaxMonoid extends Monoid[Int]{
  def id:Int = Int.MaxValue
  def op(a: Int, b: Int): Int =
    if(a > b)
      a
    else b
}
