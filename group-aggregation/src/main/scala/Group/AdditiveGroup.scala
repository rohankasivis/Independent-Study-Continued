package Group

// right now only focus on handling ints
trait AdditiveGroup[@specialized(Byte, Short, Int, Long, Float, Double) A] extends Any {
  def additive: Group[A] = new Group[A] {
    def id: Int = zero
    def op(x: A, y: A): Double = plus(x, y)
    def inverse(x: A): Double = negate(x)
  }

  // all of these methods are implemented in additive.scala
  def zero: Int
  def plus(x: A, y: A): Double
  def minus(x: A, y: A): Double
  def negate(x: A): Double
}