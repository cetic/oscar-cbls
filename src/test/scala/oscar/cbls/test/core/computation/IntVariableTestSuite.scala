package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.{IntConstant, IntIdentityInvariant, IntVariable}

import scala.util.Random

class IntVariableTestSuite extends AnyFunSuite {

  val random: Random     = Random
  private val seed: Long = random.nextLong()
  random.setSeed(seed)

  test(s"Identity invariant and IntVariable operations work as expected. (seed: $seed)") {
    val store                   = new Store(debugLevel = 3)
    var randomValues: List[Int] = List.fill(8)(random.between(-1000, 1000))
    var referenceInt: Long      = randomValues.head
    val input                   = IntVariable(store, randomValues.head)
    val output                  = IntVariable(store, randomValues.head)
    randomValues = randomValues.tail
    new IntIdentityInvariant(store, input, output)
    store.close()

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

  test(s"Save and restore value works as expected. (seed: $seed)") {
    val store             = new Store()
    val startValue        = random.nextInt(10)
    val input             = IntVariable(store, startValue)
    val output            = IntVariable(store, startValue)
    var referenceInt      = startValue
    val savedReferenceInt = startValue
    new IntIdentityInvariant(store, input, output)
    store.close()

    val savedValue = input.save()
    for (_ <- 0 until 10) {
      val value = random.nextInt(100)
      input :+= value
      referenceInt += value
    }
    output.value() should be(referenceInt)
    savedValue.restoreValue()
    output.value() should be(savedReferenceInt)
  }

  test(s"An IntConstant cannot be modified. (seed: $seed)") {
    val store = new Store()
    val const = new IntConstant(store, 15)
    store.close()

    val exception = intercept[IllegalArgumentException](const :+= 15)
    assert(exception.getMessage.contains("The value of a constant variable can not be changed"))
  }

}
