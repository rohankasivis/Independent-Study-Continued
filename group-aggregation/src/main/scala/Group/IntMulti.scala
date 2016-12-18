package Group

class IntMulti extends Group[Int] {
  def id: Int = 1
  def inverse(a: Int): Int  = {
  if (a > 0)
  {
    val si: BigInt = a
    val sr: BigInt = a + 1
    si.modInverse(sr).intValue()
  }
  else
  {
   1
  }
 }
 def op(a: Int, b: Int): Int = if (a == 0) { 1 * b } else if (b == 0) { 1 * a} else {a * b}
}

