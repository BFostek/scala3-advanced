
object Currying{
  val supperAdder : Int => Int => Int = 
    x => y => x+y

  val add3 = supperAdder(3)
  def curriedAdder(x: Int)(y:Int): Int = 
    x+y


  // methods != function values

  def main(args: Array[String]): Unit = {
    println(add3(2))
  }
}
