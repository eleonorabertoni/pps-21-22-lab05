package u05lab.ex1

import org.junit.Test
import org.junit.Assert.*

class ListTest {

  /** todo */
  val reference: List[Int] = List(1, 2, 3, 4)
  @Test
  def testZipRight(): Unit =

    println(reference)
    println(reference.zipRight)



  @Test
  def testPartition(): Unit =

    val v: (List[Int], List[Int])= reference.partition(_ >= 2)
    val v1: (List[Int], List[Int]) = reference.partition(_ % 2 == 0)
    println(v._1)
    println(v._2)

    println(v1._1)
    println(v1._2)

  @Test
  def testSpan(): Unit =
    import List.*
    //val noMatchList = List(1, 2, 3, 4)
    assertEquals((List(1), List(2, 3, 4)), reference.span(_ % 2 != 0))
    assertEquals((List(1, 2), List(3, 4)), reference.span(_ < 3))
    assertEquals((List(1, 2, 3), List(4)), reference.span(_ < 4))
    assertEquals((List(1, 2, 3, 4), Nil()), reference.span(_ < 5))

  @Test
  def testReduce(): Unit =
    assertEquals(10, reference.reduce(_ + _))
    assertEquals(10, List(10).reduce(_ + _))

  @Test
  def testReduceException(): Unit =
  try Nil.reduce[Int](_ + _)
  catch case ex: Exception => assertEquals("java.lang.UnsupportedOperationException: empty.reduceLeft", ex.toString())

  @Test
  def testTakeRight(): Unit =
    assertEquals(List(2, 3, 4), reference.takeRight(3))

  @Test
  def testCollectOnSlidesExample(): Unit =
    val listWithSomeNegatives = List(-2, -1, 0, 1, 2)
    def pf: PartialFunction[Int, String] = _ match
      case n if n > 0 => "pos"
      case 0 => "zero"
    assertEquals(List("zero", "pos", "pos"), listWithSomeNegatives.collect(pf))



}
