package oscar.cbls.test.algo.magicArray

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalacheck.Gen
import oscar.cbls.algo.magicArray.ResettableArray
import scala.util.Random
import scala.annotation.tailrec

class ResettableArrayTestSuite
    extends AnyFunSuite
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  val arraySize = 1_000
  val rand      = new Random(1_000)

  // The idea of the test is the following:
  // We generate two arrays: one ResettableArray and one classical array that is used as the witness
  // We generate a list of updates, and after applying the list of update, we check that both arrays
  // are the same. After applying the list of updates, we reset the array and check that the arrays
  // contain the same values.

  /** The case class that presents an update
    *
    * @param id
    *   the id to update
    * @param value
    *   the new value
    */
  case class Update(id: Int, value: Int)

  /** The test data case class. It contains the arrays (the resettable array and the witness array)
    * and a set of method to apply updates on the arrays and to check the arrays
    *
    * @param arraySize
    *   The size of the arrays
    */
  case class TestData(arraySize: Int) {
    private val defaultValues: Array[Int] = Array.fill(arraySize)(rand.nextInt())
    private val resettableArray           = new ResettableArray(arraySize, defaultValues(_))
    private val witnessArray              = Array.tabulate(arraySize)(defaultValues)

    def checkArray: Boolean = {
      var res = true
      for (i <- (0 until arraySize)) {
        res = res && resettableArray(i) == witnessArray(i)
      }
      res
    }

    private def applyUpdate(u: Update): Unit = {
      resettableArray(u.id) = u.value
      witnessArray(u.id) = u.value
    }

    @tailrec
    private def applyUpdateListRec(l: List[Update]): Unit = {
      l match {
        case Nil =>
        case h :: t =>
          applyUpdate(h)
          applyUpdateListRec(t)
      }
    }

    def resetArrays: Unit = {
      resettableArray.reset()
      for (i <- (0 until arraySize)) witnessArray(i) = defaultValues(i)
    }

    def applyUpdates(l: List[Update]): Unit = applyUpdateListRec(l)
  }

  // An update generator
  def genUpdate(arraySize: Int): Gen[Update] = for {
    newValue <- Gen.choose(Int.MinValue, Int.MaxValue)
    id       <- Gen.choose(0, arraySize - 1)
  } yield Update(id, newValue)

  // A list of update generator
  def genUpdateList(arraySize: Int): Gen[List[Update]] = for {
    nbElems <- Gen.choose(10, 1_000)
    lst     <- Gen.listOfN(nbElems, genUpdate(arraySize))
  } yield lst

  test("Array is correct at initialisation") {
    val testData = TestData(arraySize)

    testData.checkArray should be(true)
  }

  test("Array is correct after a list of moves and reset") {
    val testData = TestData(arraySize)
    forAll(genUpdateList(arraySize)) { list =>
      testData.applyUpdates(list)
      testData.checkArray should be(true)
      // val checkAfterUpdates = testData.checkArray
      testData.resetArrays
      testData.checkArray should be(true)

    }

  }

}
