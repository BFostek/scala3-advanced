import scala.annotation.tailrec
abstract class FSet[A] extends (A => Boolean) {
  // main api
  def contains(elem: A): Boolean
  def apply(elem: A): Boolean = contains(elem)
  infix def +(elem: A): FSet[A]
  infix def ++(anotherSet: FSet[A]): FSet[A]

  // "classic"
  def map[B](f: A => B): FSet[B]
  def flatMap[B](f: A => FSet[B]): FSet[B]
  def filter(predicate: A => Boolean): FSet[A]
  def foreach(f: A => Unit): Unit

  // methods
  infix def -(elem: A): FSet[A]
  infix def --(anotherSet: FSet[A]): FSet[A]
  infix def &(anotherSet: FSet[A]): FSet[A]

  def unary_! : FSet[A] = new PBSet(x => !contains(x))
}

class PBSet[A](property: A => Boolean) extends FSet[A] {
  def contains(elem: A): Boolean = property(elem)
  infix def +(elem: A): FSet[A] = new PBSet(x => x == elem || property(x))
  infix def ++(anotherSet: FSet[A]): FSet[A] = new PBSet(x=> property(x) || anotherSet(x))

  // "classic"
  def map[B](f: A => B): FSet[B] = politelyFail()
  def flatMap[B](f: A => FSet[B]): FSet[B] = politelyFail()
  def filter(predicate: A => Boolean): FSet[A] = new PBSet(x=> property(x) && predicate(x))
  def foreach(f: A => Unit): Unit = politelyFail()

  // methods
  infix def -(elem: A): FSet[A] = filter(x=> x!= elem)
  infix def --(anotherSet: FSet[A]): FSet[A] = filter(!anotherSet)
  infix def &(anotherSet: FSet[A]): FSet[A] = filter(anotherSet)

  // extra utilities (internal)
  private def politelyFail() = throw new RuntimeException("I don't know if this set is iterable...")


}


case class Empty[A]() extends FSet[A] {
  // TODO
  override def contains(elem: A): Boolean = false

  infix def +(elem: A): FSet[A] = Cons(elem, this)
  infix def ++(anotherSet: FSet[A]): FSet[A] = anotherSet

  // "classic"
  def map[B](f: A => B): FSet[B] = Empty()
  def flatMap[B](f: A => FSet[B]): FSet[B] = Empty()
  def filter(predicate: A => Boolean): FSet[A] = this
  def foreach(f: A => Unit): Unit = ()

  infix def -(elem: A): FSet[A] = this
  infix def --(anotherSet: FSet[A]): FSet[A] = this
  infix def &(anotherSet: FSet[A]): FSet[A] = this


}
case class Cons[A](head: A, tail: FSet[A]) extends FSet[A] {
  def contains(elem: A): Boolean = elem == head || tail.contains(elem)

  infix def +(elem: A): FSet[A] =
    if (contains(elem)) this
    else Cons(elem, this)
  infix def ++(anotherSet: FSet[A]): FSet[A] = tail ++ anotherSet + head

  def map[B](f: A => B): FSet[B] = tail.map(f) + f(head)
  def flatMap[B](f: A => FSet[B]): FSet[B] = tail.flatMap(f) ++ f(head)
  def filter(predicate: A => Boolean): FSet[A] = {
    val filteredTail = tail.filter(predicate)
    if (predicate(head)) filteredTail + head
    else filteredTail
  }
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  infix def -(elem: A): FSet[A] = {
    if (head == elem) tail
    else tail - elem + head

  }
  infix def --(anotherSet: FSet[A]): FSet[A] = filter(x => !anotherSet(x))
  infix def &(anotherSet: FSet[A]): FSet[A] = filter(anotherSet)

}

object FSet {
  def apply[A](values: A*): FSet[A] = {
    @tailrec
    def buildSet(valuesSeq: Seq[A], acc: FSet[A]): FSet[A] =
      if (valuesSeq.isEmpty) acc
      else buildSet(valuesSeq.tail, acc + valuesSeq.head)
    buildSet(values, Empty())
  }

}
object FunctionalSetPlayground {

  def main(args: Array[String]): Unit = {
    val first5 = FSet(1, 2, 3, 4, 5)
    println(first5.contains(5))
    println(first5(6))
  }
}
