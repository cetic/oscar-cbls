package oscar.cbls.test.algo.sequence

import org.scalacheck.{Gen, Shrink}
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import SequenceTestUtils._
import oscar.cbls.algo.sequence._
import oscar.cbls.algo.sequence.concrete.ConcreteIntSequence
import oscar.cbls.algo.sequence.stackedUpdate._

import java.util.concurrent.atomic.AtomicInteger

class IntSequenceSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {
  private val maxConsideredSize = 200

  //implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  def elem: Gen[Int] = for (n <- Gen.choose(0, 100)) yield n * 4 // Sparse elements

  private var seq: IntSequence          = IntSequence(List.empty)
  private val genSeqSize: AtomicInteger = new AtomicInteger(0)

  def opInsert(size: Int): Gen[Insert] = for {
    value    <- Gen.choose(0, 1000)
    position <- Gen.choose(0, size - 1)
  } yield {
    Insert(value, position)
  }

  def opMoveAfter(size: Int): Gen[MoveAfter] =
    for {
      from  <- Gen.choose(0, size - 1)
      to    <- Gen.choose(from, size - 1)
      after <- Gen.oneOf((-1 until from).toList ::: (to until size - 1).drop(1).toList)
    } yield {
      MoveAfter(from, to, after)
    }

  def opDelete(size: Int): Gen[Delete] = {
    // if the list is empty, Delete(0) will be ignored, keeping it to avoid usage of Gen.Option
    if (size <= 1) Delete(0)
    else
      for {
        position <- Gen.choose(0, size - 1)
      } yield {
        Delete(position)
      }
  }

  def opFlip: Gen[Flip]             = Gen.const(Flip())
  def opRegularize: Gen[Regularize] = Gen.const(Regularize())
  def opCommit: Gen[Commit]         = Gen.const(Commit())

  def genAction(
    onlyInsert: Boolean = false,
    onlyMove: Boolean = false,
    onlyRemove: Boolean = false
  ): Gen[Operation] = {
    for {
      // Seems mandatory to keep it, otherwise it fails constantly
      x <- Gen.const(genSeqSize.get())
      action <- {
        val size = genSeqSize.get()
        if (onlyInsert) opInsert(size)
        else if (onlyMove) Gen.oneOf(opMoveAfter(size), opFlip)
        else if (onlyRemove) opDelete(size)
        else
          Gen.oneOf(
            opInsert(size),
            opMoveAfter(size),
            opDelete(size),
            opFlip,
            opRegularize,
            opCommit
          )
      }
    } yield {
      action match {
        case _: Insert => genSeqSize.getAndIncrement()
        case _: Delete => genSeqSize.decrementAndGet()
        case _         =>
      }
      action
    }
  }

  def testBenchGen(
    onlyInsert: Boolean = false,
    onlyMove: Boolean = false,
    onlyRemove: Boolean = false
  ): Gen[(List[Int], List[Operation])] = for {
    numElems   <- Gen.choose(20, maxConsideredSize)
    numActions <- Gen.choose(20, 100)
    elems <- {
      genSeqSize.set(numElems)
      Gen.listOfN(numElems, elem)
    }
    actions <- Gen.listOfN(numActions, genAction(onlyInsert, onlyMove, onlyRemove))
  } yield (elems, actions)

  test("ConcreteIntSequence : batch queries keep expected list") {
    forAll(testBenchGen(), minSuccessful(100)) { testBench =>
      {
        whenever(testBench._1.size > 5) {
          val actionsList   = testBench._2
          val referenceList = testBench._1
          seq = IntSequence(referenceList)
          var modifiedList = referenceList

          for (action <- actionsList) {
            action match {
              case MoveAfter(from, to, after) =>
                seq = seq.moveAfter(
                  seq.explorerAtPosition(from).get,
                  seq.explorerAtPosition(to).get,
                  seq.explorerAtPosition(after),
                  flip = true
                )
                modifiedList = flipListManually(modifiedList, from, to, after)

              case Insert(value, position) =>
                seq = seq.insertAfterPosition(value, seq.explorerAtPosition(position))

                val (front, back) = modifiedList.splitAt(position)
                modifiedList = front ++ List(value) ++ back

              case Delete(position) =>
                if (referenceList.nonEmpty) {
                  seq = seq.delete(seq.explorerAtPosition(position).get)
                  modifiedList = modifiedList.take(position) ++ modifiedList.drop(position + 1)
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
            seq.isInstanceOf[ConcreteIntSequence] should be(true)
          }
          compareAllAttributes(seq, modifiedList)
        }
      }
    }
  }

  test("MovedIntSequence : batch queries keep expected list") {
    forAll(testBenchGen(onlyMove = true), minSuccessful(20)) { testBench =>
      {
        whenever(testBench._1.size > 5) {
          val actionsList   = testBench._2
          val referenceList = testBench._1
          // Creating a MovedIntSequence without changing the generated sequence
          seq = new MovedIntSequence(
            IntSequence(referenceList),
            seq.explorerAtPosition(0).get,
            seq.explorerAtPosition(0).get,
            seq.explorerAtPosition(-1),
            false,
            1
          )
          var modifiedList = referenceList

          for (action <- actionsList) {
            action match {
              case MoveAfter(from, to, after) =>
                seq = seq.moveAfter(
                  seq.explorerAtPosition(from).get,
                  seq.explorerAtPosition(to).get,
                  seq.explorerAtPosition(after),
                  flip = true
                )
                modifiedList = flipListManually(modifiedList, from, to, after)
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
    forAll(testBenchGen(onlyRemove = true), minSuccessful(20)) { testBench =>
      {
        whenever(testBench._1.size > 5) {
          val actionsList   = testBench._2
          val referenceList = testBench._1
          // Creating a RemovedIntSequence without changing the generated sequenceµ
          seq = IntSequence(referenceList :+ 0)
          seq = new RemovedIntSequence(seq, seq.explorerAtPosition(referenceList.size).get, 1)
          var modifiedList = referenceList

          for (action <- actionsList) {
            action match {
              case Delete(pos) if modifiedList.nonEmpty =>
                seq = seq.delete(seq.explorerAtPosition(pos).get)
                modifiedList = modifiedList.take(pos) ++ modifiedList.drop(pos + 1)
              case _ =>
            }
          }
          compareAllAttributes(seq, modifiedList)
        }
      }
    }
  }

  test("InsertedIntSequence : batch queries keep expected list") {
    forAll(testBenchGen(onlyInsert = true), minSuccessful(20)) { testBench =>
      {
        whenever(testBench._1.size > 5) {
          val actionsList   = testBench._2
          val referenceList = testBench._1

          // Creating a InsertedIntSequence without changing the generated sequence
          val value                   = referenceList.head
          val referenceListMinusFirst = referenceList.drop(1)
          seq = new InsertedIntSequence(
            IntSequence(referenceListMinusFirst),
            value,
            seq.explorerAtPosition(0),
            1
          )
          var modifiedList = referenceList

          for (action <- actionsList) {
            action match {
              case Insert(value, at) =>
                seq = seq.insertAfterPosition(value, seq.explorerAtPosition(at))
                val (front, back) = modifiedList.splitAt(at)
                modifiedList = front ++ List(value) ++ back
            }
          }
          compareAllAttributes(seq, modifiedList)
        }
      }
    }
  }

  test("Mixed IntSequence types : batch queries keep expected list") {
    forAll(testBenchGen(), minSuccessful(20)) { testBench =>
      {
        whenever(testBench._1.size > 5) {

          val actionsList   = testBench._2
          val referenceList = testBench._1
          seq = IntSequence(referenceList)
          var modifiedList = referenceList

          for (action <- actionsList) {
            action match {
              case MoveAfter(from, to, after) =>
                seq = seq.moveAfter(
                  seq.explorerAtPosition(from).get,
                  seq.explorerAtPosition(to).get,
                  seq.explorerAtPosition(after),
                  flip = true,
                  fast = true
                )
                modifiedList = flipListManually(modifiedList, from, to, after)

              case Insert(value, position) =>
                seq = seq.insertAfterPosition(value, seq.explorerAtPosition(position), fast = true)
                val (front, back) = modifiedList.splitAt(position)
                modifiedList = front ++ List(value) ++ back

              case Delete(position) if referenceList.nonEmpty =>
                seq = seq.delete(seq.explorerAtPosition(position).get, fast = true)
                modifiedList = modifiedList.take(position) ++ modifiedList.drop(position + 1)

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
  case class MoveAfter(fromIncl: Int, toIncl: Int, after: Int) extends Operation
  case class Insert(value: Int, at: Int)                       extends Operation
  case class Delete(pos: Int)                                  extends Operation
  case class Flip()                                            extends Operation
  case class Regularize()                                      extends Operation
  case class Commit()                                          extends Operation
}
