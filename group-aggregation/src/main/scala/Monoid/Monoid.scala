package Monoid

trait Monoid[A] extends Any {
  def id: A
  def op(x:A, y:A): A
}
