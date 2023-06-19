package oscar.cbls.test.algo.magicArray

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.magicArray.MagicIntArrayStacked
import scala.util.Random
import org.scalacheck.Gen
import scala.annotation.tailrec

class MagicIntArrayStackedTestSuite
    extends AnyFunSuite
    with ScalaCheckDrivenPropertyChecks
    with Matchers {
  test("pop level below zero should fail") {
    val array = new MagicIntArrayStacked(10, int => int, 10)
    array.pushLevel()
    array.pushLevel()
    array.pushLevel()

    array.popLevel(false)
    array.popLevel(false)
    array.popLevel(false)

    an[Exception] should be thrownBy array.popLevel(false)
  }

  test("push level above limit should fail") {
    val array = new MagicIntArrayStacked(2, int => int, 10)

    array.pushLevel()
    array.pushLevel()

    an[Exception] should be thrownBy array.pushLevel()
  }

  // This test is based on a bug discovered in MagicIntArrayStacked. The bug is described in issue #32
  test("Array works when poping and saving the changes (checking bug #32)") {
    val array = new MagicIntArrayStacked(10, identity, 10)
    array(0) should not be (10)
    array.pushLevel()
    array.pushLevel()
    array.pushLevel()
    array(0) = 10
    array.popLevel(false)
    array(0) should be(10)

  }

  test("popLevel(false) keeps the changes") {
    val array = new MagicIntArrayStacked(10, identity, 10)

    array(0) = -1
    array.pushLevel()
    array.pushLevel()
    array(0) = 100
    array(1) = 200
    array(2) = 300
    array.popLevel(false)

    array(0) should be(100)
    array(1) should be(200)
    array(2) should be(300)
    array(4) should be(4)
  }

  test("popLevel(true) discards the changes") {
    val array = new MagicIntArrayStacked(10, e => e, 10)

    array(0) = -1
    array.pushLevel()
    array.pushLevel()
    array(0) = 100
    array(1) = 200
    array(2) = 300
    array.popLevel(true)

    array(0) should be(-1)
    array(1) should be(1)
    array(2) should be(2)
  }

  test("update indice, then push then pop retrieves the expected element") {
    val array = new MagicIntArrayStacked(10, e => e, 10)

    array.pushLevel()
    array(0) = 100
    array(1) = 200
    array(2) = 300
    array.pushLevel()
    array.popLevel(true)

    array(0) should be(100)
    array(1) should be(200)
    array(2) should be(300)
  }

  test("cloneTopArray returns expected array") {
    val array = new MagicIntArrayStacked(10, e => e, 10)

    array.pushLevel()
    array(0) = 100
    array(1) = 200
    array(2) = 300
    array.pushLevel()
    array.popLevel(true)

    array.cloneTopArray should be(Array(100, 200, 300, 3, 4, 5, 6, 7, 8, 9))
  }

  test("iterator yields expected array") {
    val array = new MagicIntArrayStacked(10, e => e, 10)

    array.pushLevel()
    array(0) = 100
    array(1) = 200
    array(2) = 300
    array.pushLevel()
    array.popLevel(true)

    array.iterator.toList should be(List(100, 200, 300, 3, 4, 5, 6, 7, 8, 9))

  }

  val rand = new Random(1_000)

  /** A structure representing an update
    *
    * @param id
    *   The id to update
    * @param value
    *   The new value to update
    */

  case class Update(id: Int, value: Long)

  /** A structure representing the pop or push operation to do after the list of update
    */

  abstract sealed class PopPush

  /** The pop operation
    *
    * @param dropChanges
    *   A boolean to say if the changes are discarded or not
    */
  case class Pop(dropChanges: Boolean) extends PopPush

  /** The push operation
    */
  case class Push() extends PopPush

  case class TestData(size: Int) {
    // The max level of checkpoints
    private val maxLevel = 5
    // The Magic Array
    private val magicArray = new MagicIntArrayStacked(maxLevel, _ => rand.nextInt(), size)
    // The witness array to check if the magic array works
    private val witnessArrays: Array[Array[Long]] = Array.tabulate(maxLevel)(levelId => {
      if (levelId == 0)
        Array.tabulate(size)(i => magicArray(i))
      else
        Array.fill(size)(0)
    })
    // The level of checkpoint
    private var currentLevel = 0

    def printArrays(onlyDiff: Boolean = true) = {
      println(
        Array
          .tabulate(size)(i =>
            if (witnessArrays(currentLevel)(i) != magicArray(i) || !onlyDiff)
              Some(s"$i : ${witnessArrays(currentLevel)(i)} -- ${magicArray(i)}")
            else
              None
          )
          .flatten
          .mkString("\n")
      )

    }

    /** Check the array
      *
      * @return
      *   magicArray == witnessArray (both array contains the same value)
      */

    def checkArrays: Boolean = {
      val topArray = magicArray.cloneTopArray
      for (i <- (0 until size)) {
        if (
          witnessArrays(currentLevel)(i) != topArray(i) ||
          witnessArrays(currentLevel)(i) != magicArray(i)
        )
          return false
      }
      true
    }

    private def updateValue(update: Update) = {
      magicArray(update.id) = update.value
      witnessArrays(currentLevel)(update.id) = update.value
    }

    @tailrec
    private def applyUpdateListInternal(l: List[Update]): Unit = {
      l match {
        case Nil =>
        case h :: t =>
          updateValue(h)
          applyUpdateListInternal(t)
      }
    }

    def applyUpdateList(l: List[Update]) = applyUpdateListInternal(l)

    private def canPush: Boolean = currentLevel < maxLevel

    private def canPop: Boolean = currentLevel > 0

    private def generatePopPush: PopPush = {
      if (canPop) {
        if (canPush) {
          if (rand.nextBoolean()) {
            Pop(rand.nextBoolean())
          } else
            Push()
        } else {
          Pop(rand.nextBoolean())
        }
      } else {
        Push()
      }
    }

    private def doPopOrPush(popPush: PopPush) = {
      popPush match {
        case p: Pop  => popLevel(p.dropChanges)
        case p: Push => pushLevel
      }
    }

    def mkPopPush: PopPush = {
      val popOrPush = generatePopPush
      doPopOrPush(popOrPush)
      popOrPush
    }

    private def pushLevel = {
      magicArray.pushLevel()
      currentLevel += 1
      for (i <- 0 until size) witnessArrays(currentLevel)(i) = witnessArrays(currentLevel - 1)(i)
    }

    private def popLevel(dropChanges: Boolean) = {
      magicArray.popLevel(dropChanges)
      currentLevel -= 1
      if (!dropChanges) {
        for (i <- 0 until size) witnessArrays(currentLevel)(i) = witnessArrays(currentLevel + 1)(i)
      }
    }

  }

  def genUpdate(size: Int): Gen[Update] = for {
    id    <- Gen.choose(0, size - 1)
    value <- Gen.choose(Long.MinValue, Long.MaxValue)
  } yield Update(id, value)
  def updateList(size: Int): Gen[List[Update]] = for {
    nbValues <- Gen.choose(10, 1_000)
    update   <- Gen.listOfN(nbValues, genUpdate(size))
  } yield update

  test("array works with a bunch of updates and a pop/push after the bunch of updates") {
    val arraySize = 1_000
    val testData  = TestData(arraySize)
    forAll(updateList(arraySize)) { list =>
      testData.applyUpdateList(list)
      testData.checkArrays should be(true)
      testData.mkPopPush
      testData.checkArrays should be(true)
    }
  }

}
