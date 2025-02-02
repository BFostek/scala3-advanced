object Monads {

  def listStory(): Unit = {
    val aList = List(1, 2, 3)
    val listMultiply = for {
      x <- List(1, 2, 3)
      y <- List(4, 5, 6)
    } yield x * y
    // for comprehensions = chains of map + flaMap
    val listMultiply_v2 =
      List(1, 2, 3).flatMap(x => List(4, 5, 6).map(y => x * y))

    val f = (x: Int) => List(x, x + 1)
    val g = (x: Int) => List(x, 2 * x)
    val pure = (x: Int) => List(x)

    // prop 1 : left identity
    val leftIdentity = pure(42).flatMap(f) == f(42) // for every x, for every f

    // prop 2 : right identity
    val rightIdentity = aList.flatMap(pure) == aList

    // prop 3: associativity
    val associativity =
      aList.flatMap(f).flatMap(g) == aList.flatMap(x => f(x).flatMap(g))

  }
  def optionStory(): Unit = {
    val anOption = Option(42)
    val optionString = for {
      lang <- Option("Scala")
      ver <- Option(3)
    } yield s"$lang-$ver"
    val optionString_v2 =
      Option("Scala").flatMap(lang => Option(3).map(ver => s"$lang-$ver"))

    val f = (x: Int) => Option(x + 1)
    val g = (x: Int) => Option(2 * x)
    val pure = (x: Int) => Option(x)

    // prop 1 : left identity
    val leftIdentity = pure(42).flatMap(f) == f(42)

    // prop 2: right-identity
    val rightIdentity = anOption.flatMap(pure) == anOption

    // prop 3 : associativity
    val associativity =
      anOption.flatMap(f).flatMap(g) == anOption.flatMap(x => f(x).flatMap(g))
  }
  // Monads = important for chain dependent computations
  //
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}
