class DoubleSemiGroupMult extends SemiGroup[Double]{
  override def op(x: Double, y: Double): Double = x + y
}
