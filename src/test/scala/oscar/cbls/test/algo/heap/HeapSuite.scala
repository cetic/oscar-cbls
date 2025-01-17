package oscar.cbls.test.algo.heap

import org.scalacheck.Gen
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
import oscar.cbls.algo.heap._

import scala.util.Random

abstract class AbstractHeapTester {
  val typeOfHeap: String

  def mkHeap(priorityFct: Int => Long, size: Int, maxItemValue: Int): Heap[Int]
}

class BinaryHeapTester extends AbstractHeapTester {
  override val typeOfHeap: String = "BinaryHeap"

  override def mkHeap(priorityFct: Int => Long, size: Int, maxItemValue: Int): BinaryHeap[Int] =
    new BinaryHeap(priorityFct, size)
}

class BinaryHeapWithMoveTester extends AbstractHeapTester {
  override val typeOfHeap: String = "BinaryHeapWithMove"

  override def mkHeap(
    priorityFct: Int => Long,
    size: Int,
    maxItemValue: Int
  ): BinaryHeapWithMove[Int] =
    new BinaryHeapWithMove(priorityFct, size)
}

class BinaryHeapWithMoveIntItemTester extends AbstractHeapTester {
  override val typeOfHeap: String = "BinaryHeapWithMoveIntItem"

  override def mkHeap(
    priorityFct: Int => Long,
    size: Int,
    maxItemValue: Int
  ): BinaryHeapWithMoveIntItem =
    new BinaryHeapWithMoveIntItem(priorityFct, size, maxItemValue)
}

class AggregatedBinaryHeapTester extends AbstractHeapTester {
  override val typeOfHeap: String = "AggregatedBinaryHeap"

  // Transform a Int => Long into a Int => Int since the priority of the items is used as indices
  // in the AggregatedBinaryHeap
  override def mkHeap(
    priorityFct: Int => Long,
    size: Int,
    maxItemValue: Int
  ): AggregatedBinaryHeap[Int] = {
    new AggregatedBinaryHeap[Int](x => priorityFct(x).toInt, maxItemValue)
  }
}

class HeapSuite(heapTester: AbstractHeapTester) extends AnyFunSuite {

  // We want to store [[HeapItem]]. Since we also have to test BinaryHeapWithMoveIntItem,
  // the actual items that we will add in the heap have to be Integer.
  //    - The actual object stored in the heap will be the indices of this Array.
  //    - The used priority function will be HeapItem.priority(...)
  //    - That way we can modify the priority of the item (hence priority of the indices)
  private val itemsToStore: Array[HeapItem] = Array.tabulate(100)(x => HeapItem(x))
  private def priority(item: Int): Long     = itemsToStore(item).priority()
  // Whether or not we authorise the test to change item priority
  private val withMoves: Boolean = heapTester.isInstanceOf[BinaryHeapWithMoveTester] ||
    heapTester.isInstanceOf[BinaryHeapWithMoveIntItemTester]

  // The witness heap. It's basically a list, to get the first item we sort it by priority()
  var witnessHeapAsList: List[Int] = List()

  // Operations generator
  val genOperations: Gen[List[Operation]] = {
    val genInsertion = {
      // Insert one of the item to store
      Gen.oneOf(itemsToStore.indices).map(value => Insert(value))
    }
    val genClear     = Clear()
    val genGetFirst  = GetFirst()
    val genGetFirsts = GetFirsts()
    val genPopFirst  = PopFirst()
    val genPopFirsts = PopFirsts()
    // Change the value of an item to store
    val genNotifyChanges  = Gen.choose(0, itemsToStore.length - 1).map(x => NotifyChanges(x))
    val genRemoveElem     = RemoveElem()
    val genCheckInternals = CheckInternals()
    val weightedOps = Gen.frequency(
      (100, genInsertion),
      (5, genClear),
      (20, genGetFirst),
      (10, genGetFirsts),
      (10, genPopFirst),
      (5, genPopFirsts),
      // Operation available only for "WithMove" heaps
      (if (withMoves) 40 else 0, genNotifyChanges),
      (if (withMoves) 10 else 0, genRemoveElem),
      (if (withMoves) 20 else 0, genCheckInternals)
    )
    // Generate a random number of operations based on the associated weight
    Gen.listOf(weightedOps)
  }

  test(s"${heapTester.typeOfHeap} : Batch operations keep expected heap") {
    // Generate a list of operations, the test is successful when having 100 successful runs
    forAll(genOperations, minSuccessful(100)) { operations: List[Operation] =>
      // Only apply operations (and test) when we have at least 10 operations
      whenever(operations.size > 10) {
        // Create heap + reset witness heap and item to store values
        val heapMaxSize      = operations.count(_.isInstanceOf[Insert]) + 1
        val heapMaxItemValue = itemsToStore.length
        val heap             = heapTester.mkHeap(priority, heapMaxSize, heapMaxItemValue)
        witnessHeapAsList = List.empty
        itemsToStore.foreach(_.reset())

        def sortedWitnessHeapAsList: List[Int] =
          witnessHeapAsList.sortBy(priority)

        for (operation <- operations) {
          operation match {
            case i: Insert =>
              try {
                heap.insert(i.value)
                witnessHeapAsList = i.value :: witnessHeapAsList
              } catch {
                // If the same element is already present in the heap, it should throw an error
                // Here it's ok so we just catch it and move to the next operation
                case e: IllegalArgumentException =>
                  if (e.getMessage.contains("Can't add the same element twice"))
                    witnessHeapAsList.contains(i.value) should be(true)
                  else
                    throw e
              }

            case p: PopFirst =>
              p.heap = heap.popFirst()
              if (witnessHeapAsList.nonEmpty) {
                // If we have multiple element with the same priority we must ensure
                // to remove the same from both the heap and the witnessHeapAsList
                val heapAndWitnessSameFirstPriority =
                  p.heap.nonEmpty && priority(p.heap.get) == priority(
                    witnessHeapAsList.minBy(priority)
                  )
                val removeElemFromWitness =
                  if (heapAndWitnessSameFirstPriority) p.heap.get
                  else witnessHeapAsList.minBy(priority)
                p.witness = Some(removeElemFromWitness)
                witnessHeapAsList = removeElem(witnessHeapAsList, removeElemFromWitness)
              } else {
                p.witness = None
              }

            case ps: PopFirsts =>
              ps.heap = heap.popFirsts()
              if (witnessHeapAsList.nonEmpty) {
                val newListAndRemovedItems = removeAllItemsWithPriority(
                  witnessHeapAsList,
                  priority(witnessHeapAsList.minBy(priority)),
                  priority
                )
                witnessHeapAsList = newListAndRemovedItems._1
                ps.witness = newListAndRemovedItems._2
              } else {
                ps.witness = List(-1)
              }

            case g: GetFirst =>
              val first = heap.getFirst
              g.first = first
              if (first.nonEmpty) {
                checkPriority(first.get, witnessHeapAsList.minBy(priority))
              } else {
                first.isEmpty should be(witnessHeapAsList.isEmpty)
              }

            case gs: GetFirsts =>
              val firsts = heap.getFirsts
              gs.firsts = firsts
              for (elem <- firsts) {
                checkPriority(elem, firsts.head)
              }
              for (
                (heapElem, witnessElem) <- firsts zip sortedWitnessHeapAsList.take(firsts.size)
              ) {
                checkPriority(heapElem, witnessElem)
              }

            case _: Clear =>
              heap.dropAll()
              witnessHeapAsList = List[Int]()

            case nc: NotifyChanges =>
              val first = heap.getFirst
              if (first.nonEmpty) {
                val newValue = nc.value
                nc.itemId = witnessHeapAsList.head
                itemsToStore(nc.itemId).changeValue(newValue)
                heap match {
                  case withMove: BinaryHeapWithMove[Int] => withMove.notifyChange(nc.itemId)
                  case withMoveInt: BinaryHeapWithMoveIntItem =>
                    withMoveInt.notifyChange(nc.itemId)
                }
              }

            case r: RemoveElem =>
              if (heap.nonEmpty) {
                val indexOfItemToRemove = Random.nextInt(heap.size)
                val itemToRemove        = heap.iterator.toList.drop(indexOfItemToRemove).head
                r.removed = itemToRemove
                heap match {
                  case withMove: BinaryHeapWithMove[Int] => withMove.removeElement(itemToRemove)
                  case withMoveInt: BinaryHeapWithMoveIntItem =>
                    withMoveInt.removeElement(itemToRemove)
                }
                witnessHeapAsList = removeElem(witnessHeapAsList, itemToRemove)
              }

            case _: CheckInternals =>
              heap match {
                case withMove: BinaryHeapWithMove[Int]      => withMove.checkInternals()
                case withMoveInt: BinaryHeapWithMoveIntItem => withMoveInt.checkInternals()
              }
          }
        }

        // Checks after each list of operations
        heap.size should be(witnessHeapAsList.size)
        heap.isEmpty should be(witnessHeapAsList.isEmpty)

        var testList: List[Int] = List()
        while (heap.nonEmpty) {
          testList = heap.popFirst().get :: testList
        }
        val groupedTestList          = testList.groupBy(priority)
        val groupedWitnessHeapAsList = witnessHeapAsList.groupBy(priority)

        // Should have same keys (priorities)
        groupedTestList.keys.toList.sorted should be(groupedWitnessHeapAsList.keys.toList.sorted)

        for (k <- groupedTestList.keys) {
          // Each priority key should hold the same elements
          groupedTestList(k).toSet should be(groupedWitnessHeapAsList(k).toSet)
        }
      }
    }
  }

  private def checkPriority(heapElem: Int, witnessElem: Int): Unit = {
    priority(heapElem) should be(priority(witnessElem))
  }

  def removeElem(list: List[Int], elem: Int): List[Int] = {
    val indexMin = list.indexOf(elem)
    val res      = list.take(indexMin) ::: list.takeRight(list.size - indexMin - 1)
    res
  }

  def removeElems(list: List[Int], elems: List[Int]): List[Int] = {
    var tempList = list
    for (i <- elems) tempList = removeElem(tempList, i)
    tempList
  }

  def removeAllItemsWithPriority(
    list: List[Int],
    priority: Long,
    priorityFct: Int => Long
  ): (List[Int], List[Int]) = {
    (
      list.sortBy(priorityFct).dropWhile(x => priorityFct(x) == priority),
      list.sortBy(priorityFct).takeWhile(x => priorityFct(x) == priority)
    )
  }

  /** All the operations available
    */
  abstract sealed class Operation
  case class Insert(value: Int) extends Operation {
    override def toString: String = s"Insert : $value"
  }
  case class GetFirst() extends Operation {
    var first: Option[Int] = Some(-1)

    override def toString: String = s"Get first : $first"
  }
  case class GetFirsts() extends Operation {
    var firsts: List[Int] = List.empty

    override def toString: String = s"Get firsts : $firsts"
  }
  case class PopFirst() extends Operation {
    var heap: Option[Int]    = Some(-1)
    var witness: Option[Int] = Some(-1)

    override def toString: String = s"Pop first : $heap should be $witness"
  }
  case class PopFirsts() extends Operation {
    var heap: List[Int]    = List.empty
    var witness: List[Int] = List.empty

    override def toString: String = s"Pop firsts : $heap should be $witness"
  }
  case class Clear() extends Operation
  case class NotifyChanges(value: Int) extends Operation {
    var itemId: Int = -1
    override def toString: String = {
      s"NotifyChanges : itemId $itemId | value $value"
    }
  }
  case class RemoveElem() extends Operation {
    var removed: Int = -1

    override def toString: String = s"RemoveElem : $removed"
  }
  case class CheckInternals() extends Operation

  /** A HeapItem is a specific mutable item where the priority is based on its value. So in the Heap
    * with move we can modify this value and update it's place in the heap.
    *
    * @param initialValue
    *   The initial value of the item
    */
  case class HeapItem(initialValue: Int) {
    private var value: Int               = initialValue
    def priority(): Long                 = value.toLong
    def changeValue(newValue: Int): Unit = value = newValue
    def reset(): Unit                    = value = initialValue
  }
}

class HeapTestSuites
    extends Suites(
      new HeapSuite(new BinaryHeapTester),
      new HeapSuite(new BinaryHeapWithMoveTester),
      new HeapSuite(new BinaryHeapWithMoveIntItemTester),
      new HeapSuite(new AggregatedBinaryHeapTester)
    )
