package oscar.cbls.test.algo.dll

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.dll.DoublyLinkedList
import org.scalatest.matchers.should.Matchers

class DLLUnitTestSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {

  test("Inserting an element at the end and removing at the end gives the same element") {
    val dll = new DoublyLinkedList[Int]()

    dll.insertEnd(1)
    dll.insertEnd(2)

    val elem1 = dll.popEnd()
    val elem2 = dll.popEnd()

    elem1 should be(2)
    elem2 should be(1)
  }

  test("Inserting an element at the start and removing at the start gives the same element") {
    val dll = new DoublyLinkedList[Int]()

    dll.insertStart(1)
    dll.insertStart(2)

    val elem1 = dll.popStart()
    val elem2 = dll.popStart()

    elem1 should be(2)
    elem2 should be(1)
  }

  test(
    "The next element of the container used in the call of insertAfter is the inserted element"
  ) {
    val dll = new DoublyLinkedList[Int]()

    dll.insertEnd(1)
    dll.insertStart(2)
    val container = dll.insertEnd(3)

    dll.insertAfter(4, container)

    val iterator = dll.iterator

    iterator.next() should be (2)
    iterator.next() should be (1)
    iterator.next() should be (3)
    iterator.next() should be (4)
  }

  test("Popping an element of an empty list throws an exception") {
    val dll = new DoublyLinkedList[Int]()

    a[java.lang.IllegalArgumentException] should be thrownBy (dll.popEnd())
    a[java.lang.IllegalArgumentException] should be thrownBy (dll.popStart())
  }

  test("DropAll empties the list") {
    val dll = new DoublyLinkedList[Int]()

    dll.insertStart(1)
    dll.insertStart(2)
    dll.insertEnd(3)

    dll.size should be(3)

    dll.dropAll(0)
    dll.size should be(0)

  }
}
