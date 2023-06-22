package oscar.cbls.test.algo.heap

import org.scalacheck.Gen
import org.scalatest.Suites
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.algo.heap._

import scala.util.Random

abstract class AbstractHeapTester {
  val typeOfHeap: String

  def mkHeap(size: Int): AbstractHeap[Int]
}

class BinaryHeapTester extends AbstractHeapTester {
  override val typeOfHeap: String = "BinaryHeap"

  override def mkHeap(size: Int) = new BinaryHeap(x => x, size)
}

class BinaryHeapWithMoveTester extends AbstractHeapTester {
  override val typeOfHeap: String = "BinaryHeapWithMove"

  override def mkHeap(size: Int) = new BinaryHeapWithMove(x => x, size)
}

class BinaryHeapWithMoveIntItemTester extends AbstractHeapTester {
  override val typeOfHeap: String = "BinaryHeapWithMoveIntItem"

  override def mkHeap(size: Int) = new BinaryHeapWithMoveIntItem(x => x, size, 200)
}

class AggregatedBinaryHeapTester extends AbstractHeapTester {
  override val typeOfHeap: String = "AggregatedBinaryHeap"

  override def mkHeap(size: Int) = new AggregatedBinaryHeap[Int](x => x, 200)
}

class HeapSuite(heapTester: AbstractHeapTester) extends AnyFunSuite {

  def genOperations(listOps: Gen[Operation]): Gen[List[Operation]] = for {
    size <- Gen.chooseNum(30, 100)
    list <- Gen.listOfN(size, listOps)
  } yield list

  test(s"${heapTester.typeOfHeap} : Batch operations keep expected heap") {
    val gen = genOperations(
      Gen.frequency(
        (100, Insert()),
        (5, Clear()),
        (20, GetFirst()),
        (10, GetFirsts()),
        (10, PopFirst()),
        (5, PopFirsts())
      )
    )

    forAll(gen, minSuccessful(100)) { operations: List[Operation] =>
      whenever(operations.size > 5) {
        val keys     = Random.shuffle(operations.indices.toList ::: operations.indices.toList)
        var values   = List[Int]()
        val heapSize = operations.count(_ == Insert()) + 1
        val heap     = heapTester.mkHeap(heapSize)

        for (operation <- operations) {
          operation match {
            case Insert() =>
              val value = Random.nextInt(keys.max)
              if (!values.contains(value)) {
                heap.insert(value)
                values = value :: values
              }

            case PopFirst() =>
              val value = heap.popFirst()
              if (value.nonEmpty) {
                values = removeElem(values, value.get)
              }

            case PopFirsts() =>
              val removed = heap.popFirsts
              if (removed.nonEmpty) {
                values = removeElems(values, removed)
              }

            case GetFirsts() =>
              val first = heap.getFirst
              if(first.nonEmpty) {
                first.get should be(values.min)
              }

            case GetFirsts() =>
              val firsts = heap.getFirsts
              for (elem <- firsts) {
                keys(elem) should be(keys(firsts.head))
              }

            case Clear() =>
              heap.dropAll()
              values = List[Int]()

            case _ => // Unreachable ?
          }
        }

        var list: List[Int] = List()

        while (heap.nonEmpty) {
          list = heap.popFirst().get :: list
        }

        list.sorted should be(values.sorted)
      }
    }
  }

  def removeElem(list: List[Int], elem: Int): List[Int] = {
    val indexMin = list.indexOf(elem)
    list.take(indexMin) ::: list.takeRight(list.size - indexMin - 1)
  }

  def removeElems(list: List[Int], elems: List[Int]): List[Int] = {
    var tempList = list
    for (i <- elems) {
      tempList = removeElem(tempList, i)
    }
    tempList
  }

  abstract sealed class Operation()
  case class Insert()    extends Operation
  case class GetFirst()  extends Operation
  case class GetFirsts() extends Operation
  case class PopFirst()  extends Operation
  case class PopFirsts() extends Operation
  case class Delete()    extends Operation
  case class Clear()     extends Operation
}

class HeapTestSuites
  extends Suites(
    new HeapSuite(new BinaryHeapTester),
    new HeapSuite(new BinaryHeapWithMoveTester),
    new HeapSuite(new BinaryHeapWithMoveIntItemTester),
    new HeapSuite(new AggregatedBinaryHeapTester)
  )
