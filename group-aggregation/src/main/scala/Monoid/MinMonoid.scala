package Monoid

class MinMonoid {
  def id:Int = Int.MaxValue
  def op(a: Int, b: Int): Int =
    if(a < b)
      a
    else b
}
