package Group

// for now, only focus on ints
class Multiplicative {
  def apply[A](g: Group[A]): MultiplicativeGroup[A] = new MultiplicativeGroup[A] {
    def times(x: A, y: A): Double = x.asInstanceOf[Double] * y.asInstanceOf[Double]    // multiplying two ints by each other
    def div(x: A, y: A): Double = x.asInstanceOf[Double] * 1 / y.asInstanceOf[Double]
    def one: Int = 1    // id for multiplication is 1
    override def reciprocal(x: A): Double = 1 / x.asInstanceOf[Double]
  }
}
