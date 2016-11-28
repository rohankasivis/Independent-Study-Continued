package Group

// for now, only focus on ints
class Additive
{
  def apply[A](g: Group[A]): AdditiveGroup[A] = new AdditiveGroup[A] {
    def plus(x: A, y: A): Double = x.asInstanceOf[Double] + y.asInstanceOf[Double]
    def minus(x: A, y: A): Double = x.asInstanceOf[Double] - y.asInstanceOf[Double]
    def zero: Int = 0
    def negate(x: A): Double = x.asInstanceOf[Double] * -1
  }
}
