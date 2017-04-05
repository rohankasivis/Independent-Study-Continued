package Monoid

class IntAddition extends Monoid[Int]{
  def id:Int = 0
  def op(a: Int, b: Int): Int = a + b
}
