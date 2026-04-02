package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}
import oscar.cbls.core.computation.set.SetConstant
import oscar.cbls.core.computation.{IncredibleBulk, Invariant, KeyForRemoval, Store}
import oscar.cbls.core.propagation.PropagationElement
import oscar.cbls.lib.invariant.numeric.Sum

// Test class for an Invariant that maintains the sum of its input variables.
case class SumBulkTestInvariant(
  st: Store,
  inputVariables: List[IntVariable],
  outputVariable: IntVariable
) extends Invariant(st)
    with IntNotificationTarget {
  // Register output variable as output
  outputVariable.setDefiningInvariant(this)

  // Registers input variables statically with bulk and then dynamically
  // Voluntarily removing head of input variables from the bulk
  val bulk: IncredibleBulk = IncredibleBulk.bulkRegistering(inputVariables.tail, store)
  this.addIncredibleBulk(bulk)
  var keysForRemoval: Array[KeyForRemoval[_]] =
    inputVariables.zipWithIndex
      .map(inputVariableAndId =>
        inputVariableAndId._1.registerDynamicallyListeningElement(this, inputVariableAndId._2)
      )
      .toArray

  outputVariable := inputVariables.map(_.value()).sum

  /** This is the debug procedure through which propagation element can redundantly check that the
    * incremental computation they perform through the performPropagation method is correct
    * overriding this method is optional, so an empty body is provided by default
    */
  override def checkInternals(): Unit = {}

  /** Notifies the listening [[oscar.cbls.core.propagation.PropagationElement]] that the listened
    * [[IntVariable]] has changed.
    *
    * Implemented by the listening [[oscar.cbls.core.propagation.PropagationElement]]. Called by the
    * listened [[IntVariable]]
    *
    * @param intVariable
    *   The listened IntVariable
    * @param contextualVarIndex
    *   The index of the IntVariable in the context of the listening Invariant
    * @param oldVal
    *   The previous value of the variable
    * @param newVal
    *   The new value of the variable
    */
  override def notifyIntChanges(
    intVariable: IntVariable,
    contextualVarIndex: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = {
    require(contextualVarIndex != -1)
    outputVariable :+= (newVal - oldVal)
  }
}

class IncredibleBulkTestSuite extends AnyFunSuite with Matchers {

  test("Bulk registering properly registers a list of Variable") {
    val store               = new Store()
    val input1: IntVariable = IntVariable(store, 0L)
    val input2: IntVariable = IntVariable(store, 1L)
    val input3: IntVariable = IntVariable(store, 2L)
    val input4: IntVariable = IntVariable(store, 3L)
    val input5: IntVariable = IntVariable(store, 4L)
    val input6: IntVariable = IntVariable(store, 5L)
    val output: IntVariable = IntVariable(store, 0L)

    // Only statically registered variable should be able to notify output when calling output.value
    store.registerForPartialPropagation(output)

    // The first input variable is voluntarily NOT registered with the bulk neither on its own
    // It's only dynamically registered. Therefore, due to partial propagation, it should not propagate it's value to the Invariant
    SumBulkTestInvariant(store, List(input1, input2, input3, input4, input5, input6), output)
    store.close()

    output.value() should be(15)

    // No changes expected
    input1 := 10
    output.value() should be(15)

    // Changes expected
    input2 := 10
    output.value() should be(24)
    input3 := 10
    output.value() should be(32)
    input4 := 10
    output.value() should be(39)
    input5 := 10
    output.value() should be(45)
  }

  test("Creating two bulks on the same iterable does not create a second one") {
    val store  = new Store()
    val inputs = List.fill(5)(IntVariable(store, 0L))
    val output = IntVariable(store, 0L)

    val inv1 = SumBulkTestInvariant(store, inputs, output)
    val inv2 = SumBulkTestInvariant(store, inputs, output)
    store.close()

    inv1.bulk should equal(inv2.bulk)
  }

  test("Creating two bulks on equal iterables does not create a second one") {
    val store                = new Store()
    val input1: IntVariable  = IntVariable(store, 0L)
    val input2: IntVariable  = IntVariable(store, 1L)
    val input3: IntVariable  = IntVariable(store, 2L)
    val input4: IntVariable  = IntVariable(store, 3L)
    val input5: IntVariable  = IntVariable(store, 4L)
    val input6: IntVariable  = IntVariable(store, 5L)
    val output1: IntVariable = IntVariable(store, 0L)
    val output2: IntVariable = IntVariable(store, 0L)

    val list1 = List(input1, input2, input3, input4, input5, input6)
    val list2 = List(input1, input2, input3, input4, input5, input6)

    val inv1 = SumBulkTestInvariant(store, list1, output1)
    val inv2 = SumBulkTestInvariant(store, list2, output2)
    store.close()

    inv1.bulk should equal(inv2.bulk)
  }

  /** Store with public access to its propagation elements */
  private class StoreWithAccess extends Store(0) {

    override def getPropagationElements: List[PropagationElement] = super.getPropagationElements
  }

  test("A bulk is automatically created when enabled") {
    val store                     = new StoreWithAccess
    val input: Array[IntVariable] = Array.tabulate(5)(i => IntVariable(store, i))
    val indices: SetConstant      = SetConstant(store, Set.from(input.indices))
    val output: IntVariable       = IntVariable(store, 0L)

    Sum(store, input, indices, output, bulkUsed = true)
    store.close()

    store.getPropagationElements.exists(_.isInstanceOf[IncredibleBulk]) should be(true)
  }

  test("No bulk is created when disabled") {
    val store                     = new StoreWithAccess
    val input: Array[IntVariable] = Array.tabulate(5)(i => IntVariable(store, i))
    val indices: SetConstant      = SetConstant(store, Set.from(input.indices))
    val output: IntVariable       = IntVariable(store, 0L)

    Sum(store, input, indices, output, bulkUsed = false)
    store.close()
    store.getPropagationElements.exists(_.isInstanceOf[IncredibleBulk]) should be(false)
  }
}
