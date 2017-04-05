import Monoid.{IntAddition, MultiModuloEleven}

object MonoidTest extends App{
  // test intaddition
  val test_add = new IntAddition
  println("The id of intaddition is: " + test_add.id) // should be equal to 0

  val result:Int = test_add.op(5, 6)
  println("The result of adding 5 and 6 is: " + result) // should be equal to 11

  // test modulo
  val test_modulo = new MultiModuloEleven
  println("The id of multimodulo is: " + test_modulo.id) // should be equal to 1

  val first_res:Int = test_modulo.op(2, 4)
  println("The result of 2 * 4 modulo 11 equals: " + first_res) // should be 8

  val second_res:Int = test_modulo.op(first_res, 2)
  println("The result of 8 * 2 modulo 11 equals: " + second_res) // should be 5
}
