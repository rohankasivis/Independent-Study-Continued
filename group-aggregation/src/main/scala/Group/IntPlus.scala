package Group

class IntPlus extends Group[Int] {
    def id: Int = 0
    def inverse(a: Int): Int = a * -1
    def op(a: Int, b: Int): Int = a + b
}
