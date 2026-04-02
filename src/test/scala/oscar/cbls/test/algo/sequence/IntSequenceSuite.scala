package oscar.cbls.test.algo.sequence

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.algo.sequence._

import scala.util.Random

class IntSequenceSuite extends AnyFunSuite with Matchers {

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

  test("Two IntSequences with same values but different Token does not share the same identity") {
    val intSequence1 = IntSequence(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0))
    val intSequence2 = IntSequence(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0))

    intSequence1 sameIdentity intSequence2 should be(false)
    intSequence1 equals intSequence2 should be(true)
  }

  test("IntSequence's cache behaves as expected") {
    val intSequence = IntSequence(List.from(0 until 20))
    val seed        = Random.nextLong()
    val rng         = new Random(seed)

    withClue(s"With seed == $seed, ") {
      for (_ <- 0 until 1000) {
        val pos    = rng.nextInt(20)
        val method = rng.nextInt(3)
        if (method == 2)
          intSequence.positionOfAnyOccurrence(pos).get should be(pos)
        else {
          val explorer = method match {
            case 0 => intSequence.explorerAtPosition(pos)
            case 1 => intSequence.explorerAtAnyOccurrence(pos)
          }
          explorer.get.value should be(pos)
          explorer.get.position should be(pos)
        }
      }
    }
  }

  test("IntSequence test bench") {
    val testBench = new IntSeqTestBench(30)

    testBench.test()
  }

}
