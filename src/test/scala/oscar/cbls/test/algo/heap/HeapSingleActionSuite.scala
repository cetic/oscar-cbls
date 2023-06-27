package oscar.cbls.test.algo.heap

import org.scalactic.Requirements
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.Random

class HeapSingleActionSuite(heapTester: AbstractHeapTester) extends AnyFunSuite{

  test(s"${heapTester.typeOfHeap} : Adding an item in an empty heap has the expected behaviour"){
    val heap = heapTester.mkHeap(x => x, 10, 10)
    heap.insert(4)
    heap.size should be(1)
    heap.getFirst.get should be(4)
  }

  test(s"${heapTester.typeOfHeap} : Adding an item in a non-empty heap has the expected behaviour") {
    val heap = heapTester.mkHeap(x => x, 10, 10)
    for(i <- 0 until 5) heap.insert(i)
    heap.insert(8)
    heap.size should be(6)
    heap.getFirst.get should be(0)
  }

  test(s"${heapTester.typeOfHeap} : Adding an item in a full heap has the expected behaviour") {
    // For this test we exclude heapTester since his maximum size is maxPriority * Int.MaxValue
    // The only restriction is it's  maxPriorityValue
    if(!heapTester.isInstanceOf[AggregatedBinaryHeapTester]) {
      val heap = heapTester.mkHeap(x => x, 10, 10)
      for (i <- 0 until 10) heap.insert(i)
      an[IllegalArgumentException] should be thrownBy heap.insert(11)
    }
  }

  test(s"${heapTester.typeOfHeap} : Popping item on an empty heap has the expected behaviour") {
    val heap = heapTester.mkHeap(x => x, 10, 10)
    heap.popFirst() should be(None)
  }

  test(s"${heapTester.typeOfHeap} : Popping items one by one has the expected behaviour"){
    val heap = heapTester.mkHeap(x => x, 1000, 1000)
    for(i <- 0 until 1000) heap.insert(i)
    for(i <- 0 until 1000) heap.popFirst() should be(Some(i))
  }

  test(s"${heapTester.typeOfHeap} : Popping items priority by priority has the expected behaviour") {
    // The priority is the item value divided by 20 so that we have 50 diff priority with 20 items
    val heap = heapTester.mkHeap(x => x/20, 1000, 1000)
    for (i <- 0 until 1000) heap.insert(i)
    for (i <- 0 until 1000/20) {
      val firsts = heap.popFirsts()
      firsts.sorted should be(List.tabulate(20)(j => i*20 + j))
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
    val heap = heapTester.mkHeap(x => x, 1000, 1000)
    var priorValue = Int.MaxValue
    for (_ <- 0 until 20) {
      val valueToInsert = Random.nextInt(100)
      priorValue = Math.min(priorValue, valueToInsert)
      heap.insert(valueToInsert)
    }
    heap.getFirst should be(Some(priorValue))
  }

  test(s"${heapTester.typeOfHeap} : Get prior items has the expected behaviour") {
    val heap = heapTester.mkHeap(x => x/10, 1000, 1000)
    val priorValues = Array.fill(10)(List.empty[Int])
    for (_ <- 0 until 20) {
      val valueToInsert = Random.nextInt(100)
      priorValues(valueToInsert/10) = valueToInsert :: priorValues(valueToInsert/10)
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
    val heap = heapTester.mkHeap(x => x, 20, 20)
    for(_ <- 0 until 20)
      heap.insert(Random.nextInt(20))
    heap.size should be(20)
    heap.dropAll()
    heap.size should be(0)
  }
}


class HeapSingleActionTestSuites
  extends Suites(
    new HeapSingleActionSuite(new BinaryHeapTester),
    new HeapSingleActionSuite(new BinaryHeapWithMoveTester),
    new HeapSingleActionSuite(new BinaryHeapWithMoveIntItemTester),
    new HeapSingleActionSuite(new AggregatedBinaryHeapTester)
  )