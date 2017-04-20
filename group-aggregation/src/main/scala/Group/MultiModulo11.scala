package Group

import Monoid.MultiModuloEleven

class MultiModulo11 extends MultiModuloEleven with Group[Int]{
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
    x1
  }
}