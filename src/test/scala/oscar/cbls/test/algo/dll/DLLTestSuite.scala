package oscar.cbls.test.algo.dll

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Prop.collect
import oscar.cbls.algo.dll.DoublyLinkedList

import scala.util.Random
import scala.annotation.tailrec
import java.util.concurrent.atomic.AtomicInteger
import oscar.cbls.algo.dll.DLLStorageElement
import oscar.cbls.algo.dll.DLLIterator

class DLLTestSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {

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

  test("The next element of the container of insert after is the inserted element") {
    val dll = new DoublyLinkedList[Int]()

    dll.insertEnd(1)
    dll.insertStart(2)
    val container = dll.insertEnd(3)

    dll.insertAfter(4, container)

    container.next.elem should be(4)
  }

  test("Pop an element of an empty list throws an exception") {
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

    dll.dropAll()
    dll.size should be(0)

  }

  abstract class Operation

  abstract class AddOperation(value: Int) extends Operation

  case class AddStart(value: Int) extends AddOperation(value)

  case class AddEnd(value: Int) extends AddOperation(value)

  case class AddAfter(value: Int, afterPos: Int) extends AddOperation(value)

  abstract class RemoveOperation extends Operation

  case class RemoveStart() extends RemoveOperation

  case class RemoveEnd() extends RemoveOperation

  case class RemovePos(pos: Int) extends RemoveOperation

  case class DropAll() extends RemoveOperation

  class TestData {
    private var witnessList: List[Int]     = List()
    private val dll: DoublyLinkedList[Int] = new DoublyLinkedList()

    @tailrec
    private def getContainerAtPos(
      afterPos: Int,
      dllIt: DLLIterator[Int]
    ): DLLStorageElement[Int] = {
      if (afterPos == 0) {
        dllIt.next()
        dllIt.currentKey
      } else {
        dllIt.next()
        getContainerAtPos(afterPos - 1, dllIt)
      }
    }

    @tailrec
    final def mkOps(ops: List[Operation]): Unit = {
      ops match {
        case Nil =>
        case h :: t =>
          mkOp(h)
          mkOps(t)
      }
    }

    private def mkOp(op: Operation) = {
      op match {
        case AddStart(value) =>
          witnessList = value :: witnessList
          dll.insertStart(value)
        case AddEnd(value) =>
          witnessList = witnessList.appended(value)
          dll.insertEnd(value)
        case AddAfter(value, afterPos) =>
          val (l1, l2) = witnessList.splitAt(afterPos + 1)
          witnessList = l1 ::: value :: l2
          dll.insertAfter(value, getContainerAtPos(afterPos, dll.iterator))
        case RemoveStart() =>
          witnessList = witnessList.tail
          dll.popStart()
        case RemoveEnd() =>
          witnessList = witnessList.reverse.tail.reverse
          dll.popEnd()
        case RemovePos(pos) =>
          val (l1, l2) = witnessList.splitAt(pos)
          witnessList = l1 ::: l2.tail
          val container = getContainerAtPos(pos, dll.iterator)
          container.delete()
        case DropAll() =>
          witnessList = Nil
          dll.dropAll()
      }
    }

    def compareSize: Boolean = {
      witnessList.size == dll.size
    }

    def compareLists: Boolean = {
      var res          = true
      val dllIt        = dll.iterator
      val listIterator = witnessList.iterator
      while (dllIt.hasNext) {
        if (listIterator.hasNext) {
          res = dllIt.next() == listIterator.next()
        } else {
          dllIt.next()
          res = false
        }
      }
      res && !listIterator.hasNext
    }

  }

  val addStartGen: Gen[AddStart] = for {
    v <- Gen.choose(1, 100)
  } yield AddStart(v)
  val addEndGen: Gen[AddEnd] = for {
    v <- Gen.choose(1, 100)
  } yield AddEnd(v)
  def addAfterGen(maxPos: Int): Gen[AddAfter] = for {
    v   <- Gen.choose(1, 100)
    pos <- Gen.choose(0, maxPos - 1)
  } yield AddAfter(v, pos)

  val removeStartGen: Gen[RemoveStart] = Gen.const(RemoveStart())
  val removeEndGen: Gen[RemoveEnd]     = Gen.const(RemoveEnd())
  def removePosGen(maxPos: Int): Gen[RemovePos] = for {
    pos <- Gen.choose(0, maxPos - 1)
  } yield RemovePos(pos)

  val dropAllGen : Gen[DropAll] = Gen.const(DropAll())

  def genOperation: Gen[(Int,Int,Operation)] = {
    val size = new AtomicInteger(0)
    for {
      value <- {      println(s"chose value:${size.get()}");Gen.const(size.get())}
      value2 <- Gen.const(size.get())
      opGen <- {println(size.get);Gen.oneOf({
        (if (size.get() == 0)
           Nil
         else
           List(
             dropAllGen,
             removeStartGen,
             removeEndGen,
             removePosGen(size.get()),
             addAfterGen(size.get())
           )) ::: List(addStartGen, addEndGen)

      })
      }
      op <- opGen
    } yield {
      op match {
        case op: DropAll         => size.set(0)
        case op: AddOperation    => size.getAndIncrement()
        case op: RemoveOperation => size.getAndDecrement()
      }
      (value,value2,op)
    }
  }
  val genOp = genOperation

  // val operationsGen: Gen[List[Operation]] = for {
  //   ops <- Gen.listOfN(100, genOp)
  // } yield ops

  // test(
  //   "Making a set of operation (adding and remove operations) on a dll and on a witness list gives the same result"
  // ) {
  //   val testData = new TestData
  //   forAll(operationsGen) { ops =>
  //     println(ops)
  //     testData.mkOps(ops)
  //     testData.compareLists should be(true)
  //     testData.compareSize should be(true)
  //     testData.compareLists && testData.compareSize
  //   }
  // }

  test("toto") {
    println(genOp.sample)
    println(genOp.sample)
    println(genOp.sample)
    println(genOp.sample)
    println(genOp.sample)
    println(genOp.sample)
    println(genOp.sample)
    println(genOp.sample)
    println(genOp.sample)
    println(genOp.sample)
    println(genOp.sample)
    println(genOp.sample)
    println(genOp.sample)
  }


//   class DLLTester() {
//     private val dll  = new DoublyLinkedList[Int]()
//     private var list : List[Int] = List()

//     abstract sealed class Operation {
//       def mkOp: Unit
//     }

//     case class AddStart(value: Int) extends Operation {
//       override def mkOp: Unit = {
//         dll.insertStart(value)
//         list = value :: list
//       }
//     }

//     case class AddEnd(value: Int) extends Operation {
//       override def mkOp: Unit = {
//         dll.insertEnd(value)
//         list = list.appended(value)
//       }
//     }

//     case class AddAfter(value: Int, pos: Int) extends Operation{
//       override def mkOp: Unit = {
//         @tailrec
//         def addAfter(l : List[Int],pos : Int,res : List[Int]) : Unit = {
//           l match {
//             case Nil => throw new Error
//             case h :: t =>
//               if (pos == 0)

//           }
//         }
//       }
//     }

//     case class PopStart() extends Operation{
//       override def mkOp: Unit = {
//       }
//     }

//     case class PopEnd() extends Operation{
//       override def mkOp: Unit = {
//       }
//     }

//     case class dropAll() extends Operation {
//       override def mkOp: Unit = {
//       }
//     }

//     def applyOperation(op: Operation): Unit = {}

//   }
}
