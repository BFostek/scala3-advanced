object CatTypeClasses{

  /*
   * - applicative
   *   functor
   *   flatmap
   *   monad
   *   apply
   *   applicativeError/monadError
   *   tranverse
   */


  // functor - "mappable" data structures
  trait MyFunctor[F[_]]{
    def map[A, B](initialValuee:F[A])(f: A=>B): F[B]
  }
  import cats.Functor
  import cats.instances.list.*
  val listFunctor = Functor[List]

  def increment[F[_]](container: F[Int])(using functor:Functor[F]): F[Int]=
    functor.map(container)(_+1)


  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}
