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

  test("RootIntSequenceExplorer behave as expected") {

    val referenceList: List[Int]      = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val intSequence: IntSequence      = IntSequence.apply(referenceList)
    val explorer: IntSequenceExplorer = intSequence.explorerAtPosition(0).get

    // Check values of RootIntSequenceExplorer
    val explorerAtStartRoot: IntSequenceExplorer = explorer.exploreToPosition(-1).get
    explorerAtStartRoot.isInstanceOf[RootIntSequenceExplorer] should be(true)
    intercept[NoSuchElementException](explorerAtStartRoot.value)
    explorerAtStartRoot.position should be(-1)
    explorerAtStartRoot.prev should be(explorerAtStartRoot)
    val explorerAtEndRoot: IntSequenceExplorer = explorer.exploreToPosition(referenceList.size).get
    explorerAtEndRoot.isInstanceOf[RootIntSequenceExplorer] should be(true)
    intercept[NoSuchElementException](explorerAtEndRoot.value)
    explorerAtEndRoot.position should be(referenceList.size)
    explorerAtEndRoot.next should be(explorerAtEndRoot)

    // Next/prev returns expected values
    // Backward
    val explorerAtStart = explorer.exploreToPosition(0)
    explorerAtStart.get.value should be(referenceList.head)
    val fromStartToRoot = explorerAtStart.get.prev
    fromStartToRoot.isInstanceOf[RootIntSequenceExplorer] should be(true)
    fromStartToRoot.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(true)
    val fromRootToStart = fromStartToRoot.next
    fromRootToStart.value should be(referenceList.head)
    // Forward
    val explorerAtEnd = explorer.exploreToPosition(referenceList.size - 1)
    explorerAtEnd.get.value should be(referenceList.last)
    val fromEndToRoot = explorerAtEnd.get.next
    fromEndToRoot.isInstanceOf[RootIntSequenceExplorer] should be(true)
    fromEndToRoot.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(false)
    val fromRootToEnd = fromEndToRoot.prev
    fromRootToEnd.value should be(referenceList.last)

    // Iterator, startRoot in backward mode is an empty iterator and throw an error on next()
    val rootIteratorStartBackward = explorerAtStartRoot.backward
    rootIteratorStartBackward.toValue(4).isEmpty should be(true)
    rootIteratorStartBackward.to(_ => true).isEmpty should be(true)
    rootIteratorStartBackward.untilValue(4).isEmpty should be(true)
    rootIteratorStartBackward.until(_ => true).isEmpty should be(true)
    intercept[NoSuchElementException](rootIteratorStartBackward.toValue(4).next())
    // Iterator, endRoot in forward mode is an empty iterator and throw an error on next()
    val rootIteratorEndForward = explorerAtEndRoot.forward
    rootIteratorEndForward.toValue(4).isEmpty should be(true)
    rootIteratorEndForward.to(_ => true).isEmpty should be(true)
    rootIteratorEndForward.untilValue(4).isEmpty should be(true)
    rootIteratorEndForward.until(_ => true).isEmpty should be(true)
    intercept[NoSuchElementException](rootIteratorEndForward.toValue(4).next())

    // Iterator, startRoot in forward mode is not empty and return the root iterator on next
    val rootIteratorStartForward = explorerAtStartRoot.forward
    rootIteratorStartForward.toValue(4).isEmpty should be(false)
    rootIteratorStartForward.to(_ => true).isEmpty should be(false)
    rootIteratorStartForward.untilValue(4).isEmpty should be(false)
    rootIteratorStartForward.until(_ => true).isEmpty should be(false)
    rootIteratorStartForward.toValue(4).next().isInstanceOf[RootIntSequenceExplorer] should be(true)
    rootIteratorStartForward.toValue(4).next().position should be(-1)
    val fullSeqIteratorStartRoot = rootIteratorStartForward.iterator
    fullSeqIteratorStartRoot.map(_.position).toList should be(
      List(-1) ::: referenceList.indices.toList
    )
    // Iterator, endRoot in forward mode is an empty iterator and throw an error on next()
    val rootIteratorEndBackward = explorerAtEndRoot.backward
    rootIteratorEndBackward.toValue(4).isEmpty should be(false)
    rootIteratorEndBackward.to(_ => true).isEmpty should be(false)
    rootIteratorEndBackward.untilValue(4).isEmpty should be(false)
    rootIteratorEndBackward.until(_ => true).isEmpty should be(false)
    rootIteratorEndBackward.toValue(4).next().isInstanceOf[RootIntSequenceExplorer] should be(true)
    rootIteratorEndBackward.toValue(4).next().position should be(referenceList.size)
    val fullSeqReverseIteratorEndRoot = rootIteratorEndBackward.iterator
    fullSeqReverseIteratorEndRoot.map(_.position).toList should be(
      (referenceList ::: List(referenceList.size)).reverse
    )
  }

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

    val nonEmptyConcreteIntSequence =
      emptyIntSequence.insertAfterPosition(1, emptyIntSequence.explorerAtPosition(-1).get)
    nonEmptyConcreteIntSequence.isEmpty should be(false)
    nonEmptyConcreteIntSequence.size should be(1)
    nonEmptyConcreteIntSequence.explorerAtPosition(0).get.value should be(1)

    val emptyIntSequence2 =
      nonEmptyConcreteIntSequence.remove(nonEmptyConcreteIntSequence.explorerAtPosition(0).get)
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
          SequenceTester.testIntSequence(seq, modifiedList)
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
          SequenceTester.testIntSequence(seq, modifiedList)
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
          SequenceTester.testIntSequence(seq, modifiedList)
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
          SequenceTester.testIntSequence(seq, modifiedList)
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
          SequenceTester.testIntSequence(seq, modifiedList)
          testExplorer(seq, modifiedList)
        }
      }
    }
  }

  private def testExplorer(seq: IntSequence, modifiedList: List[Int]): Unit = {
    if (modifiedList.nonEmpty) {
      seq.explorerAtPosition(-1).get.isInstanceOf[RootIntSequenceExplorer] should be(true)
      seq.explorerAtPosition(-1).get.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(
        true
      )
      ExplorerTester.testExplorers(seq.explorerAtPosition(-1).get, modifiedList)
    }
  }

}
