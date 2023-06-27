package oscar.cbls.test.algo.heap

import org.scalactic.Requirements
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.algo.heap.{AbstractHeap, BinaryHeap}

import javax.swing.text.Position.Bias
import scala.util.Random

class HeapSingleActionSuite(heapTester: AbstractHeapTester) extends AnyFunSuite {

  @inline
  private def generateRandomIntIterator(size: Int = 100): Iterator[Int] =
    Random.shuffle(Array.tabulate(size)(identity)).iterator

  test(s"${heapTester.typeOfHeap} : Adding an item in an empty heap has the expected behaviour") {
    val heap = heapTester.mkHeap(x => x, 10, 10)
    heap.insert(4)
    heap.size should be(1)
    heap.getFirst.get should be(4)
  }

  test(
    s"${heapTester.typeOfHeap} : Adding an item in a non-empty heap has the expected behaviour"
  ) {
    val heap = heapTester.mkHeap(x => x, 10, 10)
    for (i <- 0 until 5) heap.insert(i)
    heap.insert(8)
    heap.size should be(6)
    heap.getFirst.get should be(0)
  }

  test(s"${heapTester.typeOfHeap} : Adding an item in a full heap has the expected behaviour") {
    // For this test we exclude heapTester since his maximum size is maxPriority * Int.MaxValue
    // The only restriction is it's  maxPriorityValue
    if (!heapTester.isInstanceOf[AggregatedBinaryHeapTester]) {
      val heap = heapTester.mkHeap(x => x, 10, 10)
      for (i <- 0 until 10) heap.insert(i)
      an[IllegalArgumentException] should be thrownBy heap.insert(11)
    }
  }

  test(s"${heapTester.typeOfHeap} : Popping item on an empty heap has the expected behaviour") {
    val heap = heapTester.mkHeap(x => x, 10, 10)
    heap.popFirst() should be(None)
  }

  test(s"${heapTester.typeOfHeap} : Popping items one by one has the expected behaviour") {
    val heap = heapTester.mkHeap(x => x, 1000, 1000)
    for (i <- 0 until 1000) heap.insert(i)
    for (i <- 0 until 1000) heap.popFirst() should be(Some(i))
  }

  test(
    s"${heapTester.typeOfHeap} : Popping items priority by priority has the expected behaviour"
  ) {
    // The priority is the item value divided by 20 so that we have 50 diff priority with 20 items
    val heap = heapTester.mkHeap(x => x / 20, 1000, 1000)
    for (i <- 0 until 1000) heap.insert(i)
    for (i <- 0 until 1000 / 20) {
      val firsts = heap.popFirsts()
      firsts.sorted should be(List.tabulate(20)(j => i * 20 + j))
    }
  }

  test(s"${heapTester.typeOfHeap} : Popping items on an empty heap has the expected behaviour") {
    val heap = heapTester.mkHeap(x => x, 10, 10)
    heap.popFirsts() should be(List.empty[Int])
  }

  test(s"${heapTester.typeOfHeap} : Get prior item on empty heap has the expected behaviour") {
    val heap = heapTester.mkHeap(x => x, 10, 10)
    heap.getFirst should be(None)
  }

  test(s"${heapTester.typeOfHeap} : Get prior item has the expected behaviour") {
    val heap        = heapTester.mkHeap(x => x, 1000, 1000)
    val intIterator = generateRandomIntIterator()
    var priorValue  = Int.MaxValue
    for (_ <- 0 until 20) {
      val valueToInsert = intIterator.next
      priorValue = Math.min(priorValue, valueToInsert)
      heap.insert(valueToInsert)
    }
    heap.getFirst should be(Some(priorValue))
  }

  test(s"${heapTester.typeOfHeap} : Get prior items has the expected behaviour") {
    val heap        = heapTester.mkHeap(x => x / 10, 1000, 1000)
    val priorValues = Array.fill(10)(List.empty[Int])
    val intIterator = generateRandomIntIterator()
    for (_ <- 0 until 20) {
      val valueToInsert = intIterator.next()
      priorValues(valueToInsert / 10) = valueToInsert :: priorValues(valueToInsert / 10)
      heap.insert(valueToInsert)
    }
    heap.getFirsts.sorted should be(priorValues.filter(_.nonEmpty).head.sorted)
  }

  test(s"${heapTester.typeOfHeap} : Get prior items on empty heap has the expected behaviour") {
    val heap = heapTester.mkHeap(x => x, 10, 10)
    heap.getFirsts should be(List.empty[Int])
  }

  test(s"${heapTester.typeOfHeap} : Drop all on empty heap has the expected behaviour") {
    val heap = heapTester.mkHeap(x => x, 10, 10)
    heap.size should be(0)
    heap.dropAll()
    heap.size should be(0)
  }

  test(s"${heapTester.typeOfHeap} : Drop all on non-empty heap has the expected behaviour") {
    val heap        = heapTester.mkHeap(x => x, 20, 20)
    val intIterator = generateRandomIntIterator(20)
    for (_ <- 0 until 20) heap.insert(intIterator.next())
    heap.size should be(20)
    heap.dropAll()
    heap.size should be(0)
  }

  test(s"${heapTester.typeOfHeap} : Copying a heap keeps the heap order") {
    def populate(heap: AbstractHeap[Int]): Unit = {
      val intIterator = generateRandomIntIterator(200)
      for (_ <- 0 until 100) heap.insert(intIterator.next())
    }

    def assertEquality(heap: AbstractHeap[Int], copy: AbstractHeap[Int]): Unit = {
      heap.size should be(copy.size)
      while (heap.nonEmpty) heap.popFirst() should be(copy.popFirst())
    }

    heapTester match {
      case abht: AggregatedBinaryHeapTester =>
        val heap = abht.mkHeap(x => x, 200, 200)
        populate(heap)
        val copy = heap.withPriorityFunction((x: Int) => x)
        assertEquality(heap, copy)
      case _ =>
        val heap = heapTester.mkHeap(x => x, 200, 200).asInstanceOf[BinaryHeap[Int]]
        populate(heap)
        val copy = heap.withPriorityFunction((x: Int) => x)
        assertEquality(heap, copy)
    }
  }

  test(s"${heapTester.typeOfHeap} : Reversing the priority order of a heap reverses the order of the heap") {
    def populate(heap: AbstractHeap[Int]): Unit = {
      val intIterator = generateRandomIntIterator(200)
      for (_ <- 0 until 100) heap.insert(intIterator.next())
    }

    def assertReverse(heap: AbstractHeap[Int], reversed: AbstractHeap[Int]): Unit = {
      heap.size should be(reversed.size)
      val heapListPrior: List[Int] = List.fill(heap.size)(heap.popFirst().get)
      val reversedListPrior: List[Int] = List.fill(reversed.size)(reversed.popFirst().get)
      heapListPrior should be(reversedListPrior.reverse)
    }

    heapTester match {
      case abht: AggregatedBinaryHeapTester =>
        val heap = abht.mkHeap(x => x, 200, 200)
        populate(heap)
        val reversed = heap.withPriorityFunction((x: Int) => 200-x-1)
        assertReverse(heap, reversed)
      case _ =>
        val heap = heapTester.mkHeap(x => x, 200, 200).asInstanceOf[BinaryHeap[Int]]
        populate(heap)
        val reversed = heap.withPriorityFunction((x: Int) => 200-x-1)
        assertReverse(heap, reversed)
    }
  }
}

class HeapSingleActionTestSuites
    extends Suites(
      new HeapSingleActionSuite(new BinaryHeapTester),
      new HeapSingleActionSuite(new BinaryHeapWithMoveTester),
      new HeapSingleActionSuite(new BinaryHeapWithMoveIntItemTester),
      new HeapSingleActionSuite(new AggregatedBinaryHeapTester)
    )
