package Group

// for now, only focus on ints
trait MultiplicativeGroup[@specialized(Byte, Short, Int, Long, Float, Double) A] extends Any {
  def multiplicative: Group[A] = new Group[A] {
    def id: Int = one
    def op(x: A, y: A): Double = times(x, y)
    def inverse(x: A): Double = reciprocal(x)
  }

  // all of these methods are implemented in multiplicative.scala
  def one: Int
  def times(x: A, y: A): Double
  def reciprocal(x: A): Double
  def div(x: A, y: A): Double
}