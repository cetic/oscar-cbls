package oscar.cbls.test.algo

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.dll.DoublyLinkedList

import scala.util.Random

class DLLTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  test("Dequeuing doesn't set the size negative"){

    val dll = new DoublyLinkedList[Int]()

    dll.enqueue(1)
    dll.dequeue()
    dll.dequeue()
    dll.dequeue()

    dll.size should be >= 0
  }

  test("Iterator gives the same list after enqueuing"){

    forAll((intList: List[Int]) => {

      val dll = new DoublyLinkedList[Int]()
      intList.foreach(e => dll.enqueue(e))
      intList should be (dll.iterator.toList)
    })
  }

  test("Iterator gives the same list reversed after addElem"){

    forAll((intList: List[Int]) => {

      val dll = new DoublyLinkedList[Int]()
      intList.foreach(e => dll.addElem(e))
      dll.iterator.toList should be (intList.reverseIterator.toList)
    })
  }

  test("DropAll sets the size to 0 and sets isEmpty"){
    forAll((intList: List[Int]) => {

      val dll = new DoublyLinkedList[Int]()
      intList.foreach(e => dll.enqueue(e))
      dll.dropAll()

      dll.size should be (0)
      dll.isEmpty should be (true)
    })
  }

  test("foreach iterates on all elements"){
    forAll((intList: List[Int]) => {

      val dll = new DoublyLinkedList[Int]()
      intList.foreach(e => dll.addElem(e))

      var i = 0
      dll.foreach(elem => {
        intList should contain (elem)
      })
    })
  }

  test("addElem, dequeue and enqueue keep the correct size (batch)"){

    var helper = new CRUDHelpers[Int]()
    val dll = new DoublyLinkedList[Int]()

    val genActions = Gen.oneOf(helper.operationList)

    val gen = for{
      size <- Gen.chooseNum(1,100)
      list <- Gen.listOfN(size,genActions)
    } yield list

    forAll(gen){ actionsList =>
      whenever(actionsList.nonEmpty){
        val dll = new DoublyLinkedList[Int]()
        var size = 0
        for(action <- actionsList){
          val i = Random.nextInt(dll.size + 1)
          size = action(dll,i,size)
        }
        size should be (dll.size)
      }
    }
  }

  test("Delete one DLLStorageElement removes the item"){
    val dll = new DoublyLinkedList[Int]()

    val storageElem = dll.enqueue(1)
    dll.enqueue(2)
    dll.enqueue(3)
    val storageElem3 = dll.enqueue(4)
    val storageElem2 = dll.enqueue(5)
    dll.enqueue(6)

    storageElem.delete()
    dll.size should be(5)
  }

  test("Delete several DLLStorageElement removes the items"){
    val dll = new DoublyLinkedList[Int]()

    val storageElem = dll.enqueue(1)
    dll.enqueue(2)
    dll.enqueue(3)
    val storageElem3 = dll.enqueue(4)
    val storageElem2 = dll.enqueue(5)
    dll.enqueue(6)

    storageElem.delete()
    storageElem2.delete()
    storageElem3.delete()

    dll.size should be(3)
  }

  // TODO Known to fail. Don't know if it will be corrected, or if it is by design
//  test("Delete several time the same element should fail"){
//    val dll = new DoublyLinkedList[Int]()
//
//    val storageElem = dll.enqueue(1)
//    storageElem.delete()
//
//    an [Error] should be thrownBy storageElem.delete()
//  }

  class CRUDHelpers[A]{
    val add: (DoublyLinkedList[A], A, Int) => Int = (dll: DoublyLinkedList[A], i: A, size: Int) => {
      dll.addElem(i)
      size+1
    }
    val enqueue: (DoublyLinkedList[A], A, Int) => Int = (dll: DoublyLinkedList[A], i: A, size:Int) => {
      dll.enqueue(i)
      size+1
    }
    val dequeue: (DoublyLinkedList[A], A, Int) => Int = (dll: DoublyLinkedList[A], i: A, size:Int) => {
      if(size == 0)
        size
      else
      {
        dll.dequeue()
        size-1
      }
    }
    val operationList = List(add,enqueue,dequeue)
  }
}
