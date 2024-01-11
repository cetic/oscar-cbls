package oscar.cbls.test.algo.sequence

import org.scalacheck.Shrink
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.sequence._

import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random

class IntSequenceSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {
  private val maxConsideredSize = 200

  implicit def noShrink[T]: Shrink[T]   = Shrink.shrinkAny
  private val genSeqSize: AtomicInteger = new AtomicInteger(0)

  test("Empty IntSequence behave as expected") {
    val emptyIntSequence = IntSequence.empty()
    emptyIntSequence.isInstanceOf[EmptyIntSequence] should be(true)

    assert(
      intercept[IllegalArgumentException](
        emptyIntSequence.remove(emptyIntSequence.explorerAtPosition(-1).get)
      ).getMessage.contains("Can't remove a value if the sequence is empty")
    )

    assert(
      intercept[IllegalArgumentException](
        emptyIntSequence.moveAfter(
          emptyIntSequence.explorerAtPosition(-1).get,
          emptyIntSequence.explorerAtPosition(-1).get,
          emptyIntSequence.explorerAtPosition(-1).get,
          flip = false
        )
      ).getMessage.contains("Can't move values if the sequence is empty")
    )

    val nonEmptyConcreteIntSequence = emptyIntSequence.insertAfterPosition(1, emptyIntSequence.explorerAtPosition(-1).get)
    nonEmptyConcreteIntSequence.isEmpty should be(false)
    nonEmptyConcreteIntSequence.size should be(1)
    nonEmptyConcreteIntSequence.explorerAtPosition(0).get.value should be(1)

    val emptyIntSequence2 = nonEmptyConcreteIntSequence.remove(nonEmptyConcreteIntSequence.explorerAtPosition(0).get)
    emptyIntSequence2.isInstanceOf[EmptyIntSequence] should be(true)

  }

  test("ConcreteIntSequence : batch queries keep expected list") {
    forAll(
      IntSequenceOperationsGenerator.testBenchGen(maxConsideredSize, genSeqSize),
      minSuccessful(20)
    ) { testBench =>
      {
        whenever(testBench._1.size >= 5) {
          val actionsList   = testBench._2
          val referenceList = testBench._1
          var seq           = IntSequence(referenceList)
          var modifiedList  = referenceList

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
          var seq = IntSequence(referenceList)
          seq = new MovedIntSequence(
            seq,
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
          var seq = IntSequence(referenceList :+ 0)
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
          var seq                     = IntSequence(referenceListMinusFirst)
          seq = new InsertedIntSequence(seq, value, seq.explorerAtPosition(-1).get, 1)
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
          var seq           = IntSequence(referenceList)
          var modifiedList  = referenceList

          for (action <- actionsList) {
            val newValues = action.perform(seq, modifiedList, fast = Random.nextBoolean())
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
      ExplorerTester.testExplorers(seq.explorerAtPosition(-1).get, modifiedList)
    }
  }

}
