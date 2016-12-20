package Group

class MultiModulo11 extends Group[Int]{
  def id: Int = 1

  def inverse(a: Int): Int = {
    var m:Int = 11
    var t:Int = 0
    var x0:Int = 0
    var x1:Int = 1
    var new_r:Int = a

    while(new_r > 1)
    {
      var quotient: Int = new_r / m
      t = m
      m = new_r % m
      new_r = t
      t = x0
      x0 = x1 - quotient * x0
      x1 = t
    }

    if(x1 < 0)
      x1 = x1 + 11
    return x1
  }

  def op(a: Int, b: Int): Int = (a * b) % 11
}
