package Group

class Multiplicative {
  def apply[A](g: Group[A]): MultiplicativeGroup[A] = new MultiplicativeGroup[A] {
    def times(x: A, y: A): A = g.op(x, y)
    def div(x: A, y: A): A = g.op(x, g.inverse(y))
    def one: A = g.id
    override def reciprocal(x: A): A = g.inverse(x)
  }
}
