trait SemiGroup[A] {
  def op(x: A, y: A): A
}