package oscar.cbls.test.algo.heap

import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.Random

class HeapWithMoveSingleActionSuite(heapTester: AbstractHeapTester) extends AnyFunSuite {

  private def generateArray(size: Int = 10) = Array.tabulate(size)(HeapItem)

  test(s"${heapTester.typeOfHeap} : Insert an item twice in the same heap raised an error") {
    val heap = heapTester.mkHeap(x => x, 10, 10)
    heap.insert(5)
    an[IllegalArgumentException] should be thrownBy heap.insert(5)
  }

  test(s"${heapTester.typeOfHeap} : Remove an item in an empty heap has the expected behaviour") {
    heapTester match {
      case withMoveTester: BinaryHeapWithMoveTester =>
        val heap = withMoveTester.mkHeap(x => x, 10, 10)
        heap.removeElement(4) should be(false)
      case withMoveIntItemTester: BinaryHeapWithMoveIntItemTester =>
        val heap = withMoveIntItemTester.mkHeap(x => x, 10, 10)
        heap.removeElement(4) should be(false)
    }
  }

  test(
    s"${heapTester.typeOfHeap} : Remove an item in a non-empty heap has the expected behaviour"
  ) {
    val array = generateArray()
    heapTester match {
      case withMoveTester: BinaryHeapWithMoveTester =>
        val heap = withMoveTester.mkHeap(x => x, 10, 10)
        for (item <- array.indices) heap.insert(item)
        heap.removeElement(4) should be(true)
      case withMoveIntItemTester: BinaryHeapWithMoveIntItemTester =>
        val heap = withMoveIntItemTester.mkHeap(x => x, 10, 10)
        for (item <- array.indices) heap.insert(item)
        heap.removeElement(4) should be(true)
    }
  }

  test(
    s"${heapTester.typeOfHeap} : Changing item internal value on an empty heap has the expected behaviour"
  ) {
    val array = generateArray()
    heapTester match {
      case withMoveTester: BinaryHeapWithMoveTester =>
        val heap = withMoveTester.mkHeap(x => array(x).priority(), 10, 10)
        array(0).changeValue(10)
        an[IllegalArgumentException] should be thrownBy (heap.notifyChange(0))
      case withMoveIntItemTester: BinaryHeapWithMoveIntItemTester =>
        val heap = withMoveIntItemTester.mkHeap(x => array(x).priority(), 10, 10)
        array(0).changeValue(10)
        an[IllegalArgumentException] should be thrownBy (heap.notifyChange(0))
    }
  }

  test(
    s"${heapTester.typeOfHeap} : Changing item internal value on non-empty heap has the expected behaviour"
  ) {
    val array = generateArray()
    heapTester match {
      case withMoveTester: BinaryHeapWithMoveTester =>
        val heap = withMoveTester.mkHeap(x => array(x).priority(), 10, 10)
        for (item <- array.indices) heap.insert(item)
        array(0).changeValue(10)
        heap.notifyChange(0)
        heap.getFirst should be(Some(1))
      case withMoveIntItemTester: BinaryHeapWithMoveIntItemTester =>
        val heap = withMoveIntItemTester.mkHeap(x => array(x).priority(), 10, 10)
        for (item <- array.indices) heap.insert(item)
        array(0).changeValue(10)
        heap.notifyChange(0)
        heap.getFirst should be(Some(1))
    }
  }

  test(s"${heapTester.typeOfHeap} : CheckInternals on empty heap has the expected behaviour") {
    val array = generateArray()
    heapTester match {
      case withMoveTester: BinaryHeapWithMoveTester =>
        val heap = withMoveTester.mkHeap(x => array(x).priority(), 10, 10)
        heap.checkInternals()
      case withMoveIntItemTester: BinaryHeapWithMoveIntItemTester =>
        val heap = withMoveIntItemTester.mkHeap(x => array(x).priority(), 10, 10)
        heap.checkInternals()
    }
  }

  test(s"${heapTester.typeOfHeap} : CheckInternals on non-empty heap has the expected behaviour") {
    val array = generateArray()
    heapTester match {
      case withMoveTester: BinaryHeapWithMoveTester =>
        val heap = withMoveTester.mkHeap(x => array(x).priority(), 10, 10)
        for (item <- array.indices) heap.insert(item)
        heap.checkInternals()
      case withMoveIntItemTester: BinaryHeapWithMoveIntItemTester =>
        val heap = withMoveIntItemTester.mkHeap(x => array(x).priority(), 10, 10)
        for (item <- array.indices) heap.insert(item)
        heap.checkInternals()
    }
  }

  test(s"${heapTester.typeOfHeap} test") {
    val array = generateArray()
    heapTester match {
      case withMoveTester: BinaryHeapWithMoveTester =>
        val heap = withMoveTester.mkHeap(x => array(x).priority(), 10, 10)
        heap.insert(2)
        heap.insert(3)
        heap.checkInternals()
        array(3).changeValue(0)
        heap.notifyChange(3)
        heap.insert(0)
        heap.popFirsts().sorted should be(List(0, 3))
      case withMoveIntItemTester: BinaryHeapWithMoveIntItemTester =>
        val heap = withMoveIntItemTester.mkHeap(x => array(x).priority(), 10, 10)
        heap.insert(2)
        heap.insert(3)
        heap.checkInternals()
        array(3).changeValue(0)
        heap.notifyChange(3)
        heap.insert(0)
        heap.popFirsts().sorted should be(List(0, 3))
    }
  }
}

case class HeapItem(initialValue: Int) {
  private var value: Int               = initialValue
  def priority(): Long                 = value.toLong
  def changeValue(newValue: Int): Unit = value = newValue
}

class HeapWithMoveSingleActionTestSuites
    extends Suites(
      new HeapWithMoveSingleActionSuite(new BinaryHeapWithMoveTester),
      new HeapWithMoveSingleActionSuite(new BinaryHeapWithMoveIntItemTester)
    )
