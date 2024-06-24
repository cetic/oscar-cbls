package oscar.cbls.test.algo.dll

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.dll.DoublyLinkedList

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.util.ConsoleReporter
import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalatest.prop.Configuration
import org.scalacheck.Test

// class toto extends 
//   // Defining operations on the dll
//   abstract class Operation

//   // The sub set of adding operations
//   abstract class AddOperation(value: Int) extends Operation

//   case class AddStart(value: Int) extends AddOperation(value)

//   case class AddEnd(value: Int) extends AddOperation(value)

//   case class AddAfter(value: Int, afterPos: Int) extends AddOperation(value)

//   // The subset of removing operation
//   abstract class RemoveOperation extends Operation

//   case class RemoveStart() extends RemoveOperation

//   case class RemoveEnd() extends RemoveOperation

//   case class RemovePos(pos: Int) extends RemoveOperation

//   // The dropAll operation
//   case class DropAll() extends Operation

//   class TestData {
//     // Defining the reference list and the dll
//     private var referenceList: List[Int]   = List()
//     private val dll: DoublyLinkedList[Int] = new DoublyLinkedList()
//     private var containerList : List[dll.DLLStorageElement] = List()
//     // Defining the size of the dll (it is used to generate the operations)
//     var size = new AtomicInteger(0)

//     // make a list of operations on the dll and its witness list
//     @tailrec
//     final def mkOps(ops: List[Operation]): Unit = {
//       ops match {
//         case Nil =>
//         case h :: t =>
//           mkOp(h)
//           mkOps(t)
//       }
//     }

//     // make one operation on the dll and its witness list
//     private def mkOp(op: Operation) = {
//       op match {
//         case AddStart(value) =>
//           referenceList = value :: referenceList
//           val container = dll.insertStart(value)
//           containerList = container :: containerList
//         case AddEnd(value) =>
//           referenceList = referenceList.appended(value)
//           val container = dll.insertEnd(value)
//           containerList = containerList.appended(container)
//         case AddAfter(value, afterPos) =>
//           val (l1, l2) = referenceList.splitAt(afterPos + 1)
//           referenceList = l1 ::: value :: l2
//           val container = dll.insertAfter(value, containerList(afterPos))
//           val (c1, c2) = containerList.splitAt(afterPos + 1)
//           containerList = c1 ::: container :: c2
//         case RemoveStart() =>
//           referenceList = referenceList.tail
//           containerList = containerList.tail
//           dll.popStart()
//         case RemoveEnd() =>
//           referenceList = referenceList.reverse.tail.reverse
//           containerList = containerList.reverse.tail.reverse
//           dll.popEnd()
//         case RemovePos(pos) =>
//           val (l1, l2) = referenceList.splitAt(pos)
//           referenceList = l1 ::: l2.tail
//           val (c1,c2) = containerList.splitAt(pos)
//           containerList = c1 ::: c2.tail
//           c2.head.delete()
//         case DropAll() =>
//           referenceList = Nil
//           containerList = Nil
//           dll.dropAll()
//       }
//     }


//   }

//   // Generate an AddStart operation
//   val addStartGen: Gen[AddStart] = for {
//     v <- Gen.choose(1, 100)
//   } yield AddStart(v)
//   // Generate an AddEnd operation
//   val addEndGen: Gen[AddEnd] = for {
//     v <- Gen.choose(1, 100)
//   } yield AddEnd(v)
//   // Generate an AddAfter operation
//   def addAfterGen(maxPos: Int): Gen[AddAfter] = for {
//     v   <- Gen.choose(1, 100)
//     pos <- Gen.choose(0, maxPos - 1)
//   } yield AddAfter(v, pos)

//   // Generate a RemoveStart operation
//   val removeStartGen: Gen[RemoveStart] = Gen.const(RemoveStart())
//   // Generate a RemoveEnd operation
//   val removeEndGen: Gen[RemoveEnd] = Gen.const(RemoveEnd())
//   // Generate a Remove at a given position operation
//   def removePosGen(maxPos: Int): Gen[RemovePos] = for {
//     pos <- Gen.choose(0, maxPos - 1)
//   } yield RemovePos(pos)

//   // Generate a dropAll operation
//   val dropAllGen: Gen[DropAll] = Gen.const(DropAll())

//   // Generate an operation. The operation shall always be a doable operation:
//   // We cannot remove elements on a empty list, so remove operations are only available
//   // if the size of list is greater than 0.
//   def genOperation(data: TestData): Gen[Operation] = {
//     val size = data.size
//     for {
//       // BEWARE: Don't remove the next line
//       // The value is not used, but the line is mandatory to make the generation
//       // take size into account (It is a weird behaviour of scala test generator)
//       _ <- Gen.const(size.get())
//       opGen <- {
//         Gen.oneOf({
//           (if (size.get() == 0)
//              Nil
//            else
//              List(
//                dropAllGen,
//                removeStartGen,
//                removeEndGen,
//                removePosGen(size.get()),
//                addAfterGen(size.get())
//              )) ::: List(addStartGen, addEndGen)

//         })
//       }
//       op <- opGen
//     } yield {
//       // update the size of the list
//       op match {
//         case op: DropAll         => size.set(0)
//         case op: AddOperation    => size.getAndIncrement()
//         case op: RemoveOperation => size.getAndDecrement()
//       }
//       op
//     }
//   }
//   // Generate a list of operations
//   val operationsGen: Gen[List[Operation]] = Gen.listOfN(100, genOperation(new TestData))

//   test(
//     "Making a set of operation (generated adding and remove operations) on a dll" +
//       " and on a reference list gives the same result"
//   ) {
//     val testData = new TestData
//     forAll(operationsGen) { ops =>
//       testData.mkOps(ops)
//       testData.compareLists should be(true)
//       testData.compareSize should be(true)
//       testData.compareLists && testData.compareSize
//     }
//   }
// }


class DLLProperties extends Properties("DLL Properties") {

  val seedList = List("OA5iYY0IknAd0ztukxU_GjVq6Am8i_CImCnRYr5vHYH=",
    "0R2Vp7iQ7dzDrp6KaoPfmqLZB0yGCAelv6ujE_h-fJD="
  )

  seedList.distinct.foreach(s => {
    propertyWithSeed(s"DLL command list works (seed:  $s)",Some(s)) = DLLTestCommands.property()
  })

  override def overrideParameters(p: Test.Parameters): Test.Parameters = {
    p.withTestCallback(new Test.TestCallback {
        override def onPropEval(
          name: String,
          threadIdx: Int,
          succeeded: Int,
          discarded: Int
        ): Unit = {
          println(s"toto: $name")
        }
      })
  }

  propertyWithSeed("Commands work, no Explicit seed",None) =
    DLLTestCommands.property()

}



class OperationTest extends AnyFunSuite with Checkers{

  import org.scalacheck.Test

  val dllProperty = new DLLProperties

  // dllProperty.properties.foreach(nameAndProp => {
  //   test(nameAndProp._1) {
  //     check(nameAndProp._2,Test.Parameters.default.withTestCallback(new Test.TestCallback {
  //       override def onTestResult(name : String,result : org.scalacheck.Test.Result) : Unit = {
  //         if (result.status.isInstanceOf[Test.Failed])
  //           println(name)
  //       }
  //     }))
  //   }
  // })
  dllProperty.properties.foreach(nameAndProp => {
    test(nameAndProp._1) {
      check(nameAndProp._2,Test.Parameters.default.withMinSuccessfulTests(1000)
        .withTestCallback(new Test.TestCallback {
        override def onTestResult(name : String,result : org.scalacheck.Test.Result) : Unit = {
          if (result.status.isInstanceOf[Test.Failed])
            println(name)
        }
      }))
    }
  })

}
