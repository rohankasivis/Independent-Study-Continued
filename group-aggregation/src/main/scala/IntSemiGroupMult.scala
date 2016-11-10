class IntSemiGroupMult extends SemiGroup[Int] {
  override def op(x: Int, y: Int): Int = x * y
}
