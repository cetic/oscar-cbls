package oscar.cbls.test.algo.sequence

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.sequence._
import oscar.cbls.algo.sequence.stackedUpdate._
import oscar.cbls.test.algo.sequence.SequenceTestUtils._

import scala.util.Random

class IntSequenceExplorerSuite
    extends AnyFunSuite
    with ScalaCheckDrivenPropertyChecks
    with Matchers {
  val elem: Gen[Int] = for (n <- Gen.choose(0, 100)) yield n * 4 // Sparse elements
  val testBenchGen: Gen[List[Int]] = for {
    numElems <- Gen.choose(20, 200)
    elems    <- Gen.listOfN(numElems, elem)
  } yield elems

  test("ConcreteIntSequenceExplorer is coherent") {
    forAll(testBenchGen, minSuccessful(20)) { testBench =>
      whenever(testBench.size > 5) {
        val seq = IntSequence(testBench)

        seq.zipWithIndex.foreach { case (e, i) =>
          ExplorerTestUtils.compareAllAttributes(seq.explorerAtPosition(i), i, testBench)
          ExplorerTestUtils.compareAllAttributes(
            seq.explorerAtLastOccurrence(e),
            testBench.lastIndexOf(e),
            testBench
          )
          ExplorerTestUtils.compareAllAttributes(
            seq.explorerAtFirstOccurrence(e),
            testBench.indexOf(e),
            testBench
          )
        }
      }
    }
  }

  test("MovedIntSequenceExplorer is coherent") {
    forAll(testBenchGen, minSuccessful(20)) { testBench =>
      whenever(testBench.size > 5) {
        val (indexFrom, indexTo, destination) = getRandomParametersForMoveAfter(testBench)
        var seq                               = IntSequence(testBench)
        seq = new MovedIntSequence(
          seq,
          seq.explorerAtPosition(indexFrom).get,
          seq.explorerAtPosition(indexTo).get,
          seq.explorerAtPosition(destination).get,
          true,
          1
        )
        val modifiedList = flipListManually(testBench, indexFrom, indexTo, destination)

        seq.zipWithIndex.foreach { case (e, i) =>
          ExplorerTestUtils.compareAllAttributes(seq.explorerAtPosition(i), i, modifiedList)
          ExplorerTestUtils.compareAllAttributes(
            seq.explorerAtLastOccurrence(e),
            modifiedList.lastIndexOf(e),
            modifiedList
          )
          ExplorerTestUtils.compareAllAttributes(
            seq.explorerAtFirstOccurrence(e),
            modifiedList.indexOf(e),
            modifiedList
          )
        }
      }
    }
  }

  test("RemovedIntSequenceExplorer is coherent") {
    forAll(testBenchGen, minSuccessful(50)) { testBench =>
      whenever(testBench.size > 5) {

        val i                = Random.nextInt(testBench.size)
        var seq: IntSequence = IntSequence(testBench)
        seq = new RemovedIntSequence(seq, seq.explorerAtPosition(i).get, 1)
        val modifiedList = testBench.take(i) ++ testBench.drop(i + 1)

        seq.zipWithIndex.foreach { case (e, i) =>
          ExplorerTestUtils.compareAllAttributes(seq.explorerAtPosition(i), i, modifiedList)
          ExplorerTestUtils.compareAllAttributes(
            seq.explorerAtLastOccurrence(e),
            modifiedList.lastIndexOf(e),
            modifiedList
          )
          ExplorerTestUtils.compareAllAttributes(
            seq.explorerAtFirstOccurrence(e),
            modifiedList.indexOf(e),
            modifiedList
          )
        }
      }
    }
  }
}
