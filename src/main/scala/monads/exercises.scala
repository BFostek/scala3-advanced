import scala.annotation.targetName
object MonadExercise {
  // interpretation: Any computation that might perform side effects
  case class PossiblyMonad[A](unsafeRun: ()=> A){
    def map[B](f: A=> B): PossiblyMonad[B] =
      PossiblyMonad(()=> f(unsafeRun()))
    def flatMap[B](f:A=>PossiblyMonad[B]): PossiblyMonad[B] =
      PossiblyMonad(()=>f(unsafeRun()).unsafeRun())
  }
  object PossiblyMonad {
    @targetName("pure")
    def apply[A](value: => A): PossiblyMonad[A] =
      new PossiblyMonad(()=>value)
  }


  val f = (x: Int) => PossiblyMonad(x+1)
  val g = (x: Int)=> PossiblyMonad(x+1)

  // left identity
  val isPossible = PossiblyMonad(()=>3).flatMap(f) == PossiblyMonad(()=>3)
  /*
   * 
   *
   *
   *
   * */

  // left identity
  // 
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}
