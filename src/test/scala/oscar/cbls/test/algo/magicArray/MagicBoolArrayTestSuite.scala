package oscar.cbls.test.algo

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.magicArray.MagicBoolArray
import scala.annotation.tailrec

class MagicBoolArrayTestSuite
    extends AnyFunSuite
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  test("Initial array is correct when initiate at false") {
    val array = MagicBoolArray(10)

    assert(array.indicesAtTrue.size == 0)
  }

  test("Initial array is correct when initiate at true") {
    val array = MagicBoolArray(10, true)

    assert(array.indicesAtTrue.size == 10)
  }

  test("all_(true) sets the whole array to the new value") {
    val array = MagicBoolArray(10)

    array.all = true
    array.indicesAtTrue.size should be(10)
  }

  test("all_(false) sets the whole array to the new value") {
    val array = MagicBoolArray(10)

    array.all = false
    array.indicesAtTrue.size should be(0)
  }

  // The idea of the test will be the following:
  // We will make a list of changes on the array (meaning setting variable to true or false)
  // After this list of changes, we will set all the values to true or false, and start over again
  // with a new list of changes.

  case class Update(id : Int,
    value : Boolean)

  /** The definition of a list of moves
    *
    * @param updates
    *   the list of ids and value to assign
    * @param setAll
    *   the value (true or false) we will set to the entire array
    */
  case class ListOfUpdates(updates: List[Update], setAll: Boolean)

  val arraySize = 1000
  val moveGen: Gen[Update] =
    for {
      e <- Gen.choose(0, 1)
      id <- Gen.choose(0,arraySize - 1)
    } yield Update(id,(e == 0))
  val updateList: Gen[ListOfUpdates] = for {
    numElems <- Gen.choose(10,1000)
    elems <- Gen.listOfN(numElems,moveGen)
    lstValue <- Gen.choose(0,1)
  } yield ListOfUpdates(elems,lstValue == 0)

  test("Making a list of update and seting all values still make the array consistent") {
    // We use two arrays: one magic array and one classical array for validation
    val array : MagicBoolArray = MagicBoolArray(arraySize)
    val witnessArray : Array[Boolean] = Array.fill(arraySize)(false)
    // Update the values in the array
    def updateValue(m : Update) = {
      array(m.id) = m.value
      witnessArray(m.id) = m.value
    }
    // Apply a list of updates
    @tailrec
    def applyUpdates(l : List[Update]) : Unit = {
      l match {
        case Nil =>
        case h::t =>
          updateValue(h)
          applyUpdates(t)
      }
    }
    // Check if the array are identical
    def checkArrays : Boolean = {
      var res = true
      for (i <- 0 until arraySize) {
        res = res && (array(i) == witnessArray(i))
      }
      res
    }

    forAll(updateList) { list =>
      applyUpdates(list.updates)
      val afterUpdateCheck = checkArrays
      array.all = list.setAll
      for (i <- (0 until arraySize)) witnessArray(i) = list.setAll
      afterUpdateCheck && checkArrays
    }

  }

  test("The array is still consistent even if the threshold is reached") {
    val array = MagicBoolArray(10)
    var i = 0L
    while (i < Long.MaxValue) {
      i += 1
      array.all = false
    }
    array.indicesAtTrueAsList.size should be(0)
  }

  // test(".all_ after threshold has expected  size and content (false to true)"){

  //   val array = MagicBoolArray(10)

  //   array.pivot = Long.MaxValue-10L

  //   array.all = true
  //   array.indicesAtTrue.size should be (10)
  // }

  // test(".all_ after threshold has expected  size and content (true to false)"){

  //   val array = MagicBoolArray(10, initVal = true)

  //   array.pivot = Long.MaxValue-10L

  //   array.all = false
  //   array.indicesAtTrue.size should be (0)
  // }
}
