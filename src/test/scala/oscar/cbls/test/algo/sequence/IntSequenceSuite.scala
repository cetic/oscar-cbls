package oscar.cbls.test.algo.sequence

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import SequenceTestUtils._
import oscar.cbls.algo.sequence._
import oscar.cbls.algo.sequence.stackedUpdate._

import scala.util.Random

class IntSequenceSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {
  val elem: Gen[Int] = for (n <- Gen.choose(0, 100)) yield n * 4 // Sparse elements
  val gen: Gen[Operation] =
    Gen.oneOf(List(MoveAfter(), Insert(), Delete(), Flip(), Regularize(), Commit()))

  val testBenchGen: Gen[(List[Int], List[Operation])] = for {
    numElems   <- Gen.choose(20, 200)
    numActions <- Gen.choose(20, 100)
    elems      <- Gen.listOfN(numElems, elem)
    actions    <- Gen.listOfN(numActions, gen)
  } yield (elems, actions)

  test("ConcreteIntSequence : batch queries keep expected list") {
    forAll(testBenchGen, minSuccessful(20)) { testBench =>
      {
        whenever(testBench._1.size > 5) {

          val actionsList   = testBench._2
          val referenceList = testBench._1
          var seq           = IntSequence(referenceList)
          var modifiedList  = referenceList

          for (action <- actionsList) {
            action match {
              case MoveAfter() =>
                val (indexFrom, indexTo, destination) =
                  getRandomParametersForMoveAfter(modifiedList)
                seq = seq.moveAfter(indexFrom, indexTo, destination, flip = true)
                modifiedList = flipListManually(modifiedList, indexFrom, indexTo, destination)

              case Insert() =>
                val (value, pos) = getRandomParametersForInsert(modifiedList)
                seq = seq.insertAtPosition(value, pos)

                val (front, back) = modifiedList.splitAt(pos)
                modifiedList = front ++ List(value) ++ back

              case Delete() =>
                if (referenceList.nonEmpty) {
                  val index = Random.nextInt(seq.size)
                  seq = seq.delete(index)
                  modifiedList = modifiedList.take(index) ++ modifiedList.drop(index + 1)
                }

              case Flip() =>
                seq = seq.flip()
                modifiedList = modifiedList.reverse

              case Regularize() =>
                seq = seq.regularize()

              case Commit() =>
                seq = seq.commitPendingMoves

              case _ =>
            }
          }
          compareAllAttributes(seq, modifiedList)
        }
      }
    }
  }

  test("MovedIntSequence : batch queries keep expected list") {
    forAll(testBenchGen, minSuccessful(20)) { testBench =>
      {
        whenever(testBench._1.size > 5) {
          val actionsList                       = testBench._2
          val referenceList                     = testBench._1
          val (indexFrom, indexTo, destination) = getRandomParametersForMoveAfter(referenceList)
          var seq: IntSequence = new MovedIntSequence(
            IntSequence(referenceList),
            indexFrom,
            indexTo,
            destination,
            true,
            1
          )
          var modifiedList = flipListManually(referenceList, indexFrom, indexTo, destination)

          for (action <- actionsList) {
            action match {
              case MoveAfter() =>
                val (indexFrom, indexTo, destination) =
                  getRandomParametersForMoveAfter(modifiedList)
                seq = seq.moveAfter(indexFrom, indexTo, destination, flip = true)
                modifiedList = flipListManually(modifiedList, indexFrom, indexTo, destination)

              case Flip() =>
                seq = seq.flip()
                modifiedList = modifiedList.reverse

              case _ =>
            }
          }
          compareAllAttributes(seq, modifiedList)
        }
      }
    }
  }

  test("RemovedIntSequence : batch queries keep expected list") {
    forAll(testBenchGen, minSuccessful(20)) { testBench =>
      {
        whenever(testBench._1.size > 5) {
          val actionsList   = testBench._2
          val referenceList = testBench._1

          val i                = Random.nextInt(referenceList.size)
          var seq: IntSequence = new RemovedIntSequence(IntSequence(referenceList), i, 1)
          var modifiedList     = referenceList.take(i) ++ referenceList.drop(i + 1)

          for (_ <- actionsList) {
            if (modifiedList.size > 1) {
              val index = Random.nextInt(seq.size)
              seq = seq.delete(index)
              modifiedList = modifiedList.take(index) ++ modifiedList.drop(index + 1)
            }
          }
          compareAllAttributes(seq, modifiedList)
        }
      }
    }
  }

  test("InsertedIntSequence : batch queries keep expected list") {
    forAll(testBenchGen, minSuccessful(20)) { testBench =>
      {
        whenever(testBench._1.size > 5) {
          val actionsList   = testBench._2
          val referenceList = testBench._1

          val (value, pos)     = getRandomParametersForInsert(referenceList)
          var seq: IntSequence = new InsertedIntSequence(IntSequence(referenceList), value, pos, 1)
          val (front, back)    = referenceList.splitAt(pos)
          var modifiedList     = front ++ List(value) ++ back

          for (_ <- actionsList) {
            val (value, pos) = getRandomParametersForInsert(modifiedList)
            seq = seq.insertAtPosition(value, pos)

            val (front, back) = modifiedList.splitAt(pos)
            modifiedList = front ++ List(value) ++ back
          }
          compareAllAttributes(seq, modifiedList)
        }
      }
    }
  }

  test("Mixed IntSequence types : batch queries keep expected list") {
    forAll(testBenchGen, minSuccessful(20)) { testBench =>
      {
        whenever(testBench._1.size > 5) {

          val actionsList   = testBench._2
          val referenceList = testBench._1
          var seq           = IntSequence(referenceList)
          var modifiedList  = referenceList

          for (action <- actionsList) {
            action match {
              case MoveAfter() =>
                val (indexFrom, indexTo, destination) =
                  getRandomParametersForMoveAfter(modifiedList)
                seq = seq.moveAfter(indexFrom, indexTo, destination, flip = true, fast = true)
                modifiedList = flipListManually(modifiedList, indexFrom, indexTo, destination)

              case Insert() =>
                val (value, pos) = getRandomParametersForInsert(modifiedList)
                seq = seq.insertAtPosition(value, pos, fast = true)

                val (front, back) = modifiedList.splitAt(pos)
                modifiedList = front ++ List(value) ++ back

              case Delete() =>
                if (referenceList.nonEmpty) {
                  val index = Random.nextInt(seq.size)
                  seq = seq.delete(index, fast = true)
                  modifiedList = modifiedList.take(index) ++ modifiedList.drop(index + 1)
                }

              case Flip() =>
                seq = seq.flip(fast = true)
                modifiedList = modifiedList.reverse

              case Regularize() =>
                seq.regularizeToMaxPivot(4)

              case Commit() =>
                seq = seq.commitPendingMoves

              case _ =>
            }
          }
          compareAllAttributes(seq, modifiedList)
        }
      }
    }
  }

  abstract sealed class Operation()
  case class MoveAfter()  extends Operation
  case class Insert()     extends Operation
  case class Delete()     extends Operation
  case class Flip()       extends Operation
  case class Regularize() extends Operation
  case class Commit()     extends Operation
}
