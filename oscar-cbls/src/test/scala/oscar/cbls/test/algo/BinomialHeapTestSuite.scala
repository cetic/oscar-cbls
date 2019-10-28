package oscar.cbls.test.algo

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.heap._

import scala.util.Random

class BinomialHeapTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  def genOperations(listOps :Gen[Operation]): Gen[List[Operation]] = for {
    size <- Gen.chooseNum(30, 100)
    list <- Gen.listOfN(size, listOps)
  } yield list

  test("BinomialHeapWithMoveLong : Batch operations keep expected heap") {
    val gen = genOperations(Gen.frequency((10,Insert()), (9, GetFirsts()), (6, RemoveFirst()), (4, Delete())))

    forAll(gen,minSuccessful(100)) { operations: List[Operation] =>
      whenever(operations.size > 5) {
        val keys = Random.shuffle(operations.indices.toList ::: operations.indices.toList) // Just to have duplicated keys
        var values = List[Int]()
        val maxcount = operations.count(_ == Insert()) + 1
        val heap = new BinomialHeapWithMoveLong(keys(_), maxcount, keys.max)

        for (operation <- operations) {
          operation match {
            case Insert() =>
              val value = Random.nextInt(keys.max)
              if (!values.contains(value)){
                heap.insert(value)
                values = value :: values
              }

            case Delete() =>
              if (!heap.isEmpty) {
                val value = Gen.oneOf(values).sample.get
                heap.delete(value)
                values = removeElem(values, value)
              }

            case RemoveFirst() =>
              if (!heap.isEmpty) {
                val value = heap.removeFirst()
                values = removeElem(values, value)
              }

            case GetFirsts() =>
              val firsts = heap.getFirsts
              for(elem <- firsts){
                keys(elem) should be (keys(firsts.head))
              }
          }

          heap.checkInternals()
        }

        var list: List[Int] = List()

        while (!heap.isEmpty) {
          list = heap.removeFirst() :: list
        }

        list.sorted should be(values.sorted)
      }
    }
  }

  test("BinomialHeapWithMoveInt : Batch operations keep expected heap") {
    val gen = genOperations(Gen.frequency((10,Insert()), (9, GetFirsts()), (6, RemoveFirst()), (4, Delete())))

    forAll(gen,minSuccessful(100)) { operations: List[Operation] =>
      whenever(operations.size > 5) {
        val keys = Random.shuffle(operations.indices.toList ::: operations.indices.toList) // Just to have duplicated keys
        var values = List[Int]()
        val maxcount = operations.count(_ == Insert()) + 1
        val heap = new BinomialHeapWithMoveInt(keys(_), maxcount, keys.max)

        for (operation <- operations) {
          operation match {
            case Insert() =>
              val value = Random.nextInt(keys.max)
              if (!values.contains(value)){
                heap.insert(value)
                values = value :: values
              }

            case Delete() =>
              if (!heap.isEmpty) {
                val value = Gen.oneOf(values).sample.get
                heap.delete(value)
                values = removeElem(values, value)
              }

            case RemoveFirst() =>
              if (!heap.isEmpty) {
                val value = heap.removeFirst()
                values = removeElem(values, value)
              }

            case GetFirsts() =>
              val firsts = heap.getFirsts
              for(elem <- firsts){
                keys(elem) should be (keys(firsts.head))
              }
          }
        }

        var list: List[Int] = List()

        while (!heap.isEmpty) {
          list = heap.removeFirst() :: list
        }

        list.sorted should be(values.sorted)
      }
    }
  }

  test("BinomialHeap : Batch operations keep expected heap") {
    val gen = genOperations(Gen.frequency((10,Insert()), (6, Clear()), (4, GetFirsts()), (4, RemoveFirst()), (4, RemoveFirsts())))

    forAll(gen,minSuccessful(100)) { operations: List[Operation] =>
      whenever(operations.size > 5) {
        val keys = Random.shuffle(operations.indices.toList ::: operations.indices.toList)
        var values = List[Int]()
        val maxcount = operations.count(_ == Insert()) + 1
        val heap = new BinomialHeap[Int](keys(_), maxcount)

        for (operation <- operations) {
          operation match {
            case Insert() =>
              val value = Random.nextInt(keys.max)
              if (!values.contains(value)){
                heap.insert(value)
                values = value :: values
              }

            case RemoveFirst() =>
              if (!heap.isEmpty) {
                val value = heap.popFirst()
                values = removeElem(values, value)
              }

            case RemoveFirsts() =>
              if(!heap.isEmpty){
                val removed = heap.popFirsts
                values = removeElems(values, removed)
              }

            case GetFirsts() =>
              val firsts = heap.getFirsts
              for(elem <- firsts){
                keys(elem) should be (keys(firsts.head))
              }

            case Clear() =>
              heap.dropAll()
              values = List[Int]()
          }
        }

        var list: List[Int] = List()

        while (!heap.isEmpty) {
          list = heap.popFirst() :: list
        }

        list.sorted should be(values.sorted)
      }
    }
  }

  test("AggregatedBinomialHeapQList : Batch operations keep expected heap"){
    val gen = genOperations(Gen.frequency((10, Insert()), (6, Clear())))

    forAll(gen,minSuccessful(100)) { operations: List[Operation] =>
      whenever(operations.size > 5) {

        val keys = operations.indices
        var values = List[Int]()
        val maxcount = keys.size
        val heap = new AggregatedBinomialHeapQList[Int](keys(_),maxcount)

        for (operation <- operations) {
          operation match {
            case Insert() =>
              val value = Random.nextInt(keys.max)
              if (!values.contains(value)){
                heap.insert(value)
                values = value :: values
              }


            case Clear() =>
              heap.dropAll()
              values = List[Int]()
          }
        }

        var list: List[Int] = List()
        var listIterator = heap.iterator.toList

        while (!heap.isEmpty) {
          list = heap.popFirst() :: list
        }

        list.sorted should be(listIterator.sorted)
        list.sorted should be(values.sorted)
      }
    }
  }

  test("AggregatedBinomialHeapArrayList : Batch operations keep expected heap"){
    val gen = genOperations(Gen.frequency((10, Insert()), (6, Clear())))

    forAll(gen,minSuccessful(100)) { operations: List[Operation] =>
      whenever(operations.size > 5) {

        val keys = operations.indices
        var values = List[Int]()
        val maxcount = keys.size
        val heap = new AggregatedBinomialHeapArrayList[Int](keys(_),maxcount)

        for (operation <- operations) {
          operation match {
            case Insert() =>
              val value = Random.nextInt(keys.max)
              if (!values.contains(value)){
                heap.insert(value)
                values = value :: values
              }


            case Clear() =>
              heap.dropAll()
              values = List[Int]()
          }
        }

        var list: List[Int] = List()
        while (!heap.isEmpty) {
          list = heap.popFirst() :: list
        }

        list.sorted should be(values.sorted)
      }
    }
  }

  test("BinomialHeapWithMove[Long] : Batch operations keep expected heap"){
    val gen = genOperations(Gen.frequency((10, Insert()), (4,GetFirsts()), (4,Delete()), (4, RemoveFirst())))

    forAll(gen,minSuccessful(100)) { operations: List[Operation] =>
      whenever(operations.size > 5) {

        val keys = Random.shuffle(operations.indices.toList ::: operations.indices.toList)
        var values = List[Int]()
        val maxcount = operations.count(_ == Insert()) + 1
        val heap = new BinomialHeapWithMove[Int](keys(_),maxcount)

        for (operation <- operations) {
          operation match {
            case Insert() =>
              val value = Random.nextInt(keys.max)
              if (!values.contains(value)) {
                heap.insert(value)
                values = value :: values
              }

            case RemoveFirst() =>
              if (!heap.isEmpty) {
                val removed = heap.removeFirst()
                values = removeElem(values, removed)
              }

            case GetFirsts() =>
              val firsts = heap.getFirsts
              for (elem <- firsts) {
                keys(elem) should be(keys(firsts.head))
              }

            case Delete() =>
              if (!heap.isEmpty) {
                val value = Gen.oneOf(values).sample.get
                heap.delete(value)
                values = removeElem(values, value)
              }
          }
        }

        var list: List[Int] = List()

        while (!heap.isEmpty) {
          list = heap.removeFirst() :: list
        }

        list.sorted should be(values.sorted)
      }
    }
  }

  test("BinomialHeapWithMoveExtMem : Batch operations keep expected heap"){
    val gen = genOperations(Gen.frequency((10, Insert()), (4,GetFirsts()), (4,Delete()), (4, RemoveFirst())))

    forAll(gen,minSuccessful(100)) { operations: List[Operation] =>
      whenever(operations.size > 5) {
        val keys = Random.shuffle(operations.indices.toList ::: operations.indices.toList)
        var values = List[Int]()
        val maxcount = operations.count(_ == Insert()) + 1
        val map = new scala.collection.mutable.HashMap[Int,Int]()
        val heap = new BinomialHeapWithMoveExtMem[Int](keys(_),maxcount,map)

        for (operation <- operations) {
          operation match {
            case Insert() =>
              val value = Random.nextInt(keys.max)
              if (!values.contains(value)) {
                heap.insert(value)
                values = value :: values
              }

            case RemoveFirst() =>
              if (!heap.isEmpty) {
                val removed = heap.removeFirst()
                values = removeElem(values, removed)
              }

            case GetFirsts() =>
              val firsts = heap.getFirsts
              for (elem <- firsts) {
                keys(elem) should be(keys(firsts.head))
              }

            case Delete() =>
              if (!heap.isEmpty) {
                val value = Gen.oneOf(values).sample.get
                heap.delete(value)
                values = removeElem(values, value)
              }
          }
        }

        var list: List[Int] = List()

        while (!heap.isEmpty) {
          list = heap.removeFirst() :: list
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
    for(i <- elems){
      tempList = removeElem(tempList,i)
    }
    tempList
  }

  abstract sealed class Operation()
  case class Insert() extends Operation
  case class GetFirsts() extends Operation
  case class RemoveFirst() extends Operation
  case class RemoveFirsts() extends Operation
  case class Delete() extends Operation
  case class Clear() extends Operation
}