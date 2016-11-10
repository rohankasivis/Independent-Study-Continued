class LongSemiGroupMult extends SemiGroup[Long] {
  override def op(x: Long, y: Long): Long = x * y
}