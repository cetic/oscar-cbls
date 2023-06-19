package oscar.cbls.test.algo.heap

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.algo.heap.BinaryHeap

import scala.util.Random

class BinaryHeapSuite extends AnyFunSuite {

  def genOperations(listOps: Gen[Operation]): Gen[List[Operation]] = for {
    size <- Gen.chooseNum(30, 100)
    list <- Gen.listOfN(size, listOps)
  } yield list

  test("BinaryHeap : Batch operations keep expected heap") {
    val gen = genOperations(
      Gen.frequency(
        (100, Insert()),
        (10, Clear()),
        (20, GetFirst()),
        (10, GetFirsts()),
        (20, PopFirst()),
        (10, PopFirsts())
      )
    )

    forAll(gen, minSuccessful(100)) { operations: List[Operation] =>
      whenever(operations.size > 5) {
        val keys     = Random.shuffle(operations.indices.toList ::: operations.indices.toList)
        var values   = List[Int]()
        val heapSize = operations.count(_ == Insert()) + 1
        val heap     = new BinaryHeap[Int](keys(_), heapSize)

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

        while (!heap.isEmpty) {
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
