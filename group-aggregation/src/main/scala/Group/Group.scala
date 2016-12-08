package Group

trait Group[A] extends Any
{
  // three main functions: id, inverse, and op
  def id: A
  def inverse(a:A): A
  def op(x: A, y: A): A
}