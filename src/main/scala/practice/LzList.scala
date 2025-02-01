import scala.annotation.tailrec

// Write a lazily evaluated, potentially INFINITE linked list
abstract class LzList[A] {
  def isEmpty: Boolean
  def head: A
  def tail: LzList[A]

  // utilities
  def #::(element: A): LzList[A] // prepending
  infix def ++(another: => LzList[A]): LzList[A] 

  // classics
  //
  def forearch(f: A => Unit): Unit
  def map[B](f: A => B): LzList[B]
  def flatMap[B](f: A => LzList[B]): LzList[B]
  def filter(predicate: A => Boolean): LzList[A]
  def withFilter(predicate: A => Boolean): LzList[A] = filter(predicate)

  def take(n: Int): LzList[A] // Takes the first n elements from tihs lazy list
  def takeAsList(n: Int): List[A] = take(n).toList
  def toList: List[A] = {
    @tailrec
    def toListAux(remaining: LzList[A], acc: List[A]): List[A] =
      if (remaining.isEmpty) acc.reverse
      else toListAux(remaining.tail, remaining.head :: acc)
    toListAux(this, List())
  }
}

case class LzEmpty[A]() extends LzList[A] {
  def isEmpty: Boolean = true
  def head: A = throw new NoSuchElementException
  def tail: LzList[A] = throw new NoSuchElementException

  // utilities
  def #::(element: A): LzList[A] = new LzCons(element, this)
  infix def ++(another: => LzList[A]): LzList[A] = another

  // classics
  //
  def forearch(f: A => Unit): Unit = ()
  def map[B](f: A => B): LzList[B] = LzEmpty()
  def flatMap[B](f: A => LzList[B]): LzList[B] = LzEmpty()
  def filter(predicate: A => Boolean): LzList[A] = this

  def take(n: Int): LzList[A] =
    if (n == 0) this
    else
      throw new RuntimeException(
        s"Cannot take $n elements from an empty lazy list."
      )
}

class LzCons[A](hd: => A, tl: => LzList[A]) extends LzList[A] {
  def isEmpty: Boolean = false
  override lazy val head: A = hd
  override lazy val tail: LzList[A] = tl

  // utilities
  def #::(element: A): LzList[A] = new LzCons(element, this)
  infix def ++(another: => LzList[A]): LzList[A] = new LzCons(head, tail ++ another)

  // classics
  //
  def forearch(f: A => Unit): Unit = {
    @tailrec
    def foreachTailRec(list: LzList[A]): Unit =
      if (list.isEmpty) ()
      else {
        f(list.head)
        foreachTailRec(list.tail)
      }
    foreachTailRec(this)
  }

  def map[B](f: A => B): LzList[B] = new LzCons(f(head), tail.map(f))
  def flatMap[B](f: A => LzList[B]): LzList[B] =
    f(head) ++ tail.flatMap(f)
  def filter(predicate: A => Boolean): LzList[A] =
    if (predicate(head)) new LzCons(head, tail.filter(predicate))
    else tail.filter(predicate) // TODO warning

  def take(n: Int): LzList[A] =
    if (n <= 0) LzEmpty()
    else if (n == 1) new LzCons(head, LzEmpty())
    else new LzCons(head, tail.take(n - 1)) // preservers lazy eval

}

object LzList {
  def empty[A]: LzList[A] = LzEmpty()
  def generate[A](start: A)(generator: A => A): LzList[A] =
    new LzCons(start, LzList.generate(generator(start))(generator))
  def from[A](list: List[A]): LzList[A] = list.reverse.foldLeft(LzList.empty) {
    (currentList, newElement) =>
      new LzCons(newElement, currentList)
  }
  def apply[A](values: A*) = LzList.from(values.toList)
}

object LzListPlayground {
  def main(args: Array[String]): Unit = {
    val naturals = LzList.generate(1)(n => n + 1) // infinite list of naturals
    //  println(naturals.take(10).forearch(x=>println(x)))
    val first500k = naturals.take(500000)
    val first500kList = first500k.toList
// print(first500kList)
    println(naturals.map(_ * 2).takeAsList(100))
    println(naturals.flatMap(x => LzList(x, x + 1)).takeAsList(100))
    print(naturals.filter(_ < 10).takeAsList(9))
//    print(naturals.filter(_ < 10).takeAsList(10)) // SO error

  }
}
