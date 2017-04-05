package Monoid

class MultiModuloEleven extends Monoid[Int]{
  def id: Int = 1
  def op(a: Int, b: Int): Int = (a * b) % 11
}
