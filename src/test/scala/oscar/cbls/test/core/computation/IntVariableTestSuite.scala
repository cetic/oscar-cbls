package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.{
  IdentityIntInvariant,
  IntConstant,
  IntVariable,
  SavedIntValue
}

import scala.util.Random

class IntVariableTestSuite extends AnyFunSuite {

  test("Identity invariant works as expected") {
    val store                   = new Store()
    var randomValues: List[Int] = List.fill(8)(Random.between(-1000, 1000))
    var referenceInt: Long      = randomValues.head
    val input                   = new IntVariable(store, randomValues.head)
    val output                  = new IntVariable(store, randomValues.head)
    randomValues = randomValues.tail
    new IdentityIntInvariant(store, input, output)
    store.setupPropagationStructure()

    input :+= randomValues.head
    referenceInt += randomValues.head
    randomValues = randomValues.tail

    input :-= randomValues.head
    referenceInt -= randomValues.head
    randomValues = randomValues.tail

    input := randomValues.head
    referenceInt = randomValues.head
    randomValues = randomValues.tail

    input :*= randomValues.head
    referenceInt *= randomValues.head
    randomValues = randomValues.tail

    input :/= randomValues.head
    referenceInt /= randomValues.head
    randomValues = randomValues.tail

    input :*= randomValues.head
    referenceInt *= randomValues.head
    randomValues = randomValues.tail

    input :+= randomValues.head
    referenceInt += randomValues.head
    randomValues = randomValues.tail

    input.:--()
    referenceInt -= 1

    input.:++()
    referenceInt += 1

    output.value() should be(referenceInt)
  }

  test("Save and restore value works as expected") {
    val store             = new Store()
    val startValue        = Random.nextInt(10)
    val input             = new IntVariable(store, startValue)
    val output            = new IntVariable(store, startValue)
    var referenceInt      = startValue
    val savedReverenceInt = startValue
    new IdentityIntInvariant(store, input, output)
    store.setupPropagationStructure()

    val savedValue = input.save()
    for (_ <- 0 until 10) {
      val value = Random.nextInt(100)
      input :+= value
      referenceInt += value
    }
    output.value() should be(referenceInt)
    savedValue.restoreValue()
    output.value() should be(savedReverenceInt)
  }

  test("An IntConstant cannot be modified") {
    val store = new Store()
    val const = new IntConstant(store, 15)
    store.setupPropagationStructure()

    val exception = intercept[IllegalArgumentException](const :+= 15)
    assert(exception.getMessage.contains("The value of a constant variable can not be changed"))
  }

}
