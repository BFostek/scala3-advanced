object Lazy {

  lazy val x: Int = {
    println("Hello")
    42
  }
  def filterX(x: Int): Boolean = x < 30
  def filterY(x: Int): Boolean = x > 20

  def demoWithFilters(): Unit = {
    val myList = (1 to 80).toList
    val result = myList
      .withFilter(filterX)
      .withFilter(filterY)
    print(result.map(identity))
  }

  def main(args: Array[String]): Unit = {
    demoWithFilters()
  }
}
