import scala.collection.parallel.*
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParVector

object ParallelCollections {
  val aList = (1 to 100).toList
  val anIncrementedList = aList.map(_ + 1)
  val parList: ParSeq[Int] = aList.par
  val aParallelizedIncrementedList =
    parList.map(_ + 1) // map, flatMap, filter, foreach, reduce, fold
  /*
   *
   *
   * Use-case: faster processing
   * */

  val aParVector = ParVector(1, 2, 3, 4, 5, 6)

  def measure[A](expression: => A): Long = {
    val start = System.currentTimeMillis()
    val result = expression // Ensure result is used
    val end = System.currentTimeMillis()
    end - start
  }
  def compare(): Unit = {
    val list = (1 to 1000000).toList


    // Measure after warmup
    val serialTime = measure(list.map(_ + 1))
    val parallelTime = measure(list.par.map(_ + 1))

    println(s"Serial: $serialTime ms, Parallel: $parallelTime ms")
  }
  def main(args: Array[String]): Unit = {
    compare()
    println("Hello, world!")
  }
}
