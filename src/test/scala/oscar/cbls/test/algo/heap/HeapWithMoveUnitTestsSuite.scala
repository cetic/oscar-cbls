package oscar.cbls.test.algo.heap

import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.algo.heap.{BinaryHeapWithMove, BinaryHeapWithMoveIntItem}

import scala.util.Random

/** This test class aims to test specific situations applicable to Heap with moves.
  * @param heapTester
  *   A class used to generate heaps and be pasted as parameter for the tests
  */
class HeapWithMoveUnitTestsSuite(heapTester: AbstractHeapTester) extends AnyFunSuite {

  private def generateArray(size: Int = 10) = Array.tabulate(size)(HeapItem)

  test(s"${heapTester.typeOfHeap} : Insert an item twice in the same heap raised an error") {
    val heap = heapTester.mkHeap(x => x, 10, 10)
    heap.insert(5)
    an[IllegalArgumentException] should be thrownBy heap.insert(5)
  }

  test(s"${heapTester.typeOfHeap} : Remove an item in an empty heap has the expected behaviour") {
    val heap = heapTester.mkHeap(x => x, 10, 10)
    heap match {
      case withMove: BinaryHeapWithMove[Int] =>
        withMove.removeElement(4) should be(false)
      case withMoveIntItem: BinaryHeapWithMoveIntItem =>
        withMoveIntItem.removeElement(4) should be(false)
    }
  }

  test(
    s"${heapTester.typeOfHeap} : Remove an item in a non-empty heap has the expected behaviour"
  ) {
    val array = generateArray()
    val heap  = heapTester.mkHeap(x => x, 10, 10)
    for (item <- array.indices) heap.insert(item)
    heap match {
      case withMove: BinaryHeapWithMove[Int] =>
        withMove.removeElement(4) should be(true)
      case withMoveIntItem: BinaryHeapWithMoveIntItem =>
        withMoveIntItem.removeElement(4) should be(true)
    }
  }

  test(
    s"${heapTester.typeOfHeap} : Changing item internal value on an empty heap has the expected behaviour"
  ) {
    val array = generateArray()
    val heap  = heapTester.mkHeap(x => array(x).priority(), 10, 10)
    array(0).changeValue(10)
    heap match {
      case withMove: BinaryHeapWithMove[Int] =>
        an[IllegalArgumentException] should be thrownBy (withMove.notifyChange(0))
      case withMoveIntItem: BinaryHeapWithMoveIntItem =>
        an[IllegalArgumentException] should be thrownBy (withMoveIntItem.notifyChange(0))
    }
  }

  test(
    s"${heapTester.typeOfHeap} : Changing item internal value on non-empty heap has the expected behaviour"
  ) {
    val array = generateArray()
    val heap  = heapTester.mkHeap(x => array(x).priority(), 10, 10)
    for (item <- array.indices) heap.insert(item)
    array(0).changeValue(10)
    heap match {
      case withMove: BinaryHeapWithMove[Int]          => withMove.notifyChange(0)
      case withMoveIntItem: BinaryHeapWithMoveIntItem => withMoveIntItem.notifyChange(0)
    }
    heap.getFirst should be(Some(1))
  }

  test(s"${heapTester.typeOfHeap} : CheckInternals on empty heap has the expected behaviour") {
    val array = generateArray()
    val heap  = heapTester.mkHeap(x => array(x).priority(), 10, 10)
    heap match {
      case withMove: BinaryHeapWithMove[Int]          => withMove.checkInternals()
      case withMoveIntItem: BinaryHeapWithMoveIntItem => withMoveIntItem.checkInternals()
    }
  }

  test(s"${heapTester.typeOfHeap} : CheckInternals on non-empty heap has the expected behaviour") {
    val array = generateArray()
    val heap  = heapTester.mkHeap(x => array(x).priority(), 10, 10)
    for (item <- array.indices) heap.insert(item)
    heap match {
      case withMove: BinaryHeapWithMove[Int]          => withMove.checkInternals()
      case withMoveIntItem: BinaryHeapWithMoveIntItem => withMoveIntItem.checkInternals()
    }
  }
}

/** A HeapItem is a specific mutable item where the priority is based on its value. So in the Heap
  * with move we can modify this value and update it's place in the heap.
  * @param initialValue
  *   The initial value of the item
  */
case class HeapItem(initialValue: Int) {
  private var value: Int               = initialValue
  def priority(): Long                 = value.toLong
  def changeValue(newValue: Int): Unit = value = newValue
}

class HeapWithMoveUnitTestSuites
    extends Suites(
      new HeapWithMoveUnitTestsSuite(new BinaryHeapWithMoveTester),
      new HeapWithMoveUnitTestsSuite(new BinaryHeapWithMoveIntItemTester)
    )
