package oscar.cbls.test.algo.sequence

import org.scalacheck.Shrink
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.sequence._
import oscar.cbls.algo.sequence.concrete.ConcreteIntSequence
import oscar.cbls.algo.sequence.stackedUpdate._

import java.util.concurrent.atomic.AtomicInteger

class IntSequenceSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {
  private val maxConsideredSize = 200

  implicit def noShrink[T]: Shrink[T]   = Shrink.shrinkAny
  private var seq: IntSequence          = IntSequence(List.empty)
  private val genSeqSize: AtomicInteger = new AtomicInteger(0)

  test("ConcreteIntSequence : batch queries keep expected list") {
    forAll(
      IntSequenceOperationsGenerator.testBenchGen(maxConsideredSize, genSeqSize),
      minSuccessful(5)
    ) { testBench =>
      {
        whenever(testBench._1.size >= 5) {
          val actionsList   = testBench._2
          val referenceList = testBench._1
          seq = IntSequence(referenceList)
          var modifiedList = referenceList

          for (action <- actionsList) {
            val newValues = action.perform(seq, modifiedList, fast = false)
            seq = newValues._1
            modifiedList = newValues._2
            seq.isInstanceOf[ConcreteIntSequence] should be(true)
          }
          SequenceTester.compare(seq, modifiedList)
        }
      }
    }
  }

  test("MovedIntSequence : batch queries keep expected list") {
    forAll(
      IntSequenceOperationsGenerator.testBenchGen(maxConsideredSize, genSeqSize, onlyMove = true),
      minSuccessful(20)
    ) { testBench =>
      {
        whenever(testBench._1.size > 5) {
          val actionsList   = testBench._2
          val referenceList = testBench._1
          // Creating a MovedIntSequence without changing the generated sequence
          seq = new MovedIntSequence(
            IntSequence(referenceList),
            seq.explorerAtPosition(0).get,
            seq.explorerAtPosition(0).get,
            seq.explorerAtPosition(-1).get,
            false,
            1
          )
          var modifiedList = referenceList

          for (action <- actionsList) {
            val newValues = action.perform(seq, modifiedList)
            seq = newValues._1
            modifiedList = newValues._2
          }
          SequenceTester.compare(seq, modifiedList)
          testExplorer(seq, modifiedList)
        }
      }
    }
  }

  test("RemovedIntSequence : batch queries keep expected list") {
    forAll(
      IntSequenceOperationsGenerator.testBenchGen(maxConsideredSize, genSeqSize, onlyRemove = true),
      minSuccessful(20)
    ) { testBench =>
      {
        whenever(testBench._1.size > 5) {
          val actionsList   = testBench._2
          val referenceList = testBench._1
          // Creating a RemovedIntSequence without changing the generated sequence
          seq = IntSequence(referenceList :+ 0)
          seq = new RemovedIntSequence(seq, seq.explorerAtPosition(referenceList.size).get, 1)
          var modifiedList = referenceList

          for (action <- actionsList) {
            val newValues = action.perform(seq, modifiedList)
            seq = newValues._1
            modifiedList = newValues._2
          }
          SequenceTester.compare(seq, modifiedList)
          testExplorer(seq, modifiedList)

        }
      }
    }
  }

  test("InsertedIntSequence : batch queries keep expected list") {
    forAll(
      IntSequenceOperationsGenerator.testBenchGen(maxConsideredSize, genSeqSize, onlyInsert = true),
      minSuccessful(20)
    ) { testBench =>
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
            seq.explorerAtPosition(-1).get,
            1
          )
          var modifiedList = referenceList

          for (action <- actionsList) {
            val newValues = action.perform(seq, modifiedList)
            seq = newValues._1
            modifiedList = newValues._2
          }
          SequenceTester.compare(seq, modifiedList)
          testExplorer(seq, modifiedList)

        }
      }
    }
  }

  test("Mixed IntSequence types : batch queries keep expected list") {
    forAll(
      IntSequenceOperationsGenerator.testBenchGen(maxConsideredSize, genSeqSize),
      minSuccessful(20)
    ) { testBench =>
      {
        whenever(testBench._1.size > 5) {

          val actionsList   = testBench._2
          val referenceList = testBench._1
          seq = IntSequence(referenceList)
          var modifiedList = referenceList

          for (action <- actionsList) {
            val newValues = action.perform(seq, modifiedList)
            seq = newValues._1
            modifiedList = newValues._2
          }
          SequenceTester.compare(seq, modifiedList)
          testExplorer(seq, modifiedList)
        }
      }
    }
  }

  private def testExplorer(seq: IntSequence, modifiedList: List[Int]): Unit = {
    if (modifiedList.nonEmpty) {
      ExplorerTestUtils.compareAllAttributes(seq.explorerAtPosition(0), 0, modifiedList)
      ExplorerTestUtils.compareAllAttributes(
        seq.explorerAtPosition(modifiedList.size - 1),
        modifiedList.size - 1,
        modifiedList
      )
      ExplorerTestUtils.compareAllAttributes(
        seq.explorerAtPosition((modifiedList.size - 1) / 2),
        (modifiedList.size - 1) / 2,
        modifiedList
      )
    }
  }

}
