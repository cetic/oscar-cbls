package oscar.cbls.test.algo.magicArray

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.magicArray.{IterableMagicBoolArray, MagicBoolArray}
import scala.annotation.tailrec
import org.scalatest.Suites

abstract class AbstractMagicArrayTester {
  val typeOfArray: String

  def mkArray(size: Int, initValue: Boolean = false): MagicBoolArray
}

class MagicBoolArrayTester extends AbstractMagicArrayTester {
  override val typeOfArray: String = "MagicBoolArray"

  override def mkArray(size: Int, initValue: Boolean) = MagicBoolArray(size, initValue)
}

class IterableMagicBoolArrayTester extends AbstractMagicArrayTester {
  override val typeOfArray: String = "IterableMagicBoolArray"

  override def mkArray(size: Int, initValue: Boolean): MagicBoolArray =
    IterableMagicBoolArray(size, initValue)
}

class MagicBoolArrayTestSuite(magicArrayTester: AbstractMagicArrayTester)
    extends AnyFunSuite
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  test(s"${magicArrayTester.typeOfArray} : Initial array is correct when initiated at false") {
    val array = magicArrayTester.mkArray(10)

    array.indicesAtTrue.size should equal(0)
  }

  test(s"${magicArrayTester.typeOfArray} : Initial array is correct when initiated at true") {
    val array = magicArrayTester.mkArray(10, true)

    assert(array.indicesAtTrue.size == 10)
  }

  test(s"${magicArrayTester.typeOfArray} : all_(true) sets the whole array to the new value") {
    val array = magicArrayTester.mkArray(10)

    array.all = true
    array.indicesAtTrue.size should be(10)
  }

  test(s"${magicArrayTester.typeOfArray} : all_(false) sets the whole array to the new value") {
    val array = magicArrayTester.mkArray(10)

    array.all = false
    array.indicesAtTrue.size should be(0)
  }

  // The idea of the test will be the following:
  // We will make a list of changes on the array (meaning setting variable to true or false)
  // After this list of changes, we will set all the values to true or false, and start over again
  // with a new list of changes.

  case class Update(id: Int, value: Boolean)

  /** The definition of a list of moves
    *
    * @param updates
    *   the list of ids and value to assign
    * @param setAll
    *   the value (true or false) we will set to the entire array
    */
  case class ListOfUpdates(updates: List[Update], setAll: Boolean)

  /** The test data. It contains an array. Methods allow to test if the array is correct
    *
    * @param arraySize
    *   the size of the array
    */
  case class TestData(arraySize: Int) {

    private val magicArray: MagicBoolArray   = magicArrayTester.mkArray(arraySize)
    private val witnessArray: Array[Boolean] = Array.fill(arraySize)(false)

    def updateAll(value: Boolean) = {
      magicArray.all = value
      for (i <- (0 until arraySize)) witnessArray(i) = value
    }

    // Update one value in the arrays
    private def updateValue(m: Update) = {
      magicArray(m.id) = m.value
      witnessArray(m.id) = m.value
    }

    // Apply a list of updates
    @tailrec
    private def applyUpdatesRec(l: List[Update]): Unit = {
      l match {
        case Nil =>
        case h :: t =>
          updateValue(h)
          applyUpdatesRec(t)
      }
    }

    def applyUpdates(l: List[Update]): Unit = applyUpdatesRec(l)

    // Check if the array are identical
    def checkArrays: Boolean = {
      var res = true
      for (i <- 0 until arraySize) {
        res = res && (magicArray(i) == witnessArray(i))
      }
      res
    }

    def checkTrueValues(iterator: Boolean = false): Boolean = {
      val magicArrayTrueValues: List[Int] =
        if (iterator)
          magicArray.indicesAtTrue.toList.sortBy(identity)
        else
          magicArray.indicesAtTrueAsList.sortBy(identity)
      val witnessArrayTrueValues: List[Int] =
        List.tabulate(arraySize)(identity).filter(witnessArray(_)).sortBy(identity)
      @tailrec
      def check(
        list1: List[Int] = magicArrayTrueValues,
        list2: List[Int] = witnessArrayTrueValues
      ): Boolean = {
        (list1, list2) match {
          case (Nil, Nil)    => true
          case (Nil, _ :: _) => false
          case (_ :: _, Nil) => false
          case (h1 :: t1, h2 :: t2) =>
            if (h1 != h2) false else check(t1, t2)
        }
      }
      check()
    }
  }

  val arraySize = 1000
  val moveGen: Gen[Update] =
    for {
      e  <- Gen.choose(0, 1)
      id <- Gen.choose(0, arraySize - 1)
    } yield Update(id, (e == 0))
  val updateList: Gen[ListOfUpdates] = for {
    numElems <- Gen.choose(10, 1000)
    elems    <- Gen.listOfN(numElems, moveGen)
    lstValue <- Gen.choose(0, 1)
  } yield ListOfUpdates(elems, lstValue == 0)

  test(
    s"${magicArrayTester.typeOfArray} : Making a list of update and setting all values still make the array consistent"
  ) {

    val testData = TestData(arraySize)

    forAll(updateList) { list =>
      testData.applyUpdates(list.updates)
      val afterUpdateCheck = testData.checkArrays
      testData.updateAll(list.setAll)
      afterUpdateCheck && testData.checkArrays
    }

  }

  test(
    s"${magicArrayTester.typeOfArray} : IndicesAtTrue (iterator) works when making a list of update and setting all values"
  ) {

    val testData = TestData(arraySize)

    forAll(updateList) { list =>
      testData.applyUpdates(list.updates)
      val afterUpdateCheck = testData.checkTrueValues(true)
      testData.updateAll(list.setAll)
      afterUpdateCheck && testData.checkTrueValues(true)
    }

  }

  test(
    s"${magicArrayTester.typeOfArray} : IndicesAtTrueAsList works when making a list of update and setting all values"
  ) {

    val testData = TestData(arraySize)

    forAll(updateList) { list =>
      testData.applyUpdates(list.updates)
      val afterUpdateCheck = testData.checkTrueValues()
      testData.updateAll(list.setAll)
      afterUpdateCheck && testData.checkTrueValues()
    }

  }

}

class MagicBoolArrayTestSuites
    extends Suites(
      new MagicBoolArrayTestSuite(new MagicBoolArrayTester),
      new MagicBoolArrayTestSuite(new IterableMagicBoolArrayTester)
    )
