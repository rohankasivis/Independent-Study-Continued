package Group

import Monoid.IntAddition

class IntPlus extends IntAddition with Group[Int] {
    def inverse(a: Int): Int = a * -1
}
