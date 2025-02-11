import cats.effect.IO
object Effects {
  // pure functional programming 
  //
  def combine(a: Int, b: Int): Int = a+b
  val five = combine(2,3)
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("A proper fail"))

  // IO: pure, delay, defer
  // create failed effects
  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("A Failure"))

  val dealWithIt = aFailure.handleError{
    case _: RuntimeException => IO.delay(println("I'm still here"))
  }
  val effectAsEither: IO[Either[Throwable,Int]] = aFailure.attempt
  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    println("Hello, world!")
    dealWithIt.unsafeRunSync()
  }
}
