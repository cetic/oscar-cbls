package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.{IncredibleBulk, Invariant, KeyForRemoval, Store}
import oscar.cbls.core.computation.integer.{IntNotificationTarget, IntVariable}

// Test class for an Invariant that maintains the sum of its input variables.
case class SumBulkTestInvariant(
  store: Store,
  inputVariables: List[IntVariable],
  outputVariable: IntVariable,
  bulkName: String
) extends Invariant(store)
    with IntNotificationTarget {
  // Register output variable as output
  outputVariable.setDefiningInvariant(this)

  // Registers input variables statically with bulk and then dynamically
  // Voluntarily removing head of input variables from the bulk
  val bulk: IncredibleBulk = IncredibleBulk.bulkRegistering(inputVariables.tail, bulkName, store)
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
    * @param index
    *   The index of the IntVariable in the context of the listening Invariant
    * @param oldVal
    *   The previous value of the variable
    * @param newVal
    *   The new value of the variable
    */
  override def notifyIntChanges(
    intVariable: IntVariable,
    index: Int,
    oldVal: Long,
    newVal: Long
  ): Unit = {
    require(index != -1)
    outputVariable :+= (newVal - oldVal)
  }
}

class IncredibleBulkTestSuite extends AnyFunSuite {

  test("Bulk registering properly registers a list of Variable") {
    val store  = new Store()
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
    SumBulkTestInvariant(store, List(input1, input2, input3, input4, input5, input6), output, "Bulk properly")
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

  test("Creating two bulks with same identifier does not create a second one"){
    val store  = new Store()
    val inputs = List.fill(5)(IntVariable(store, 0L))
    val output = IntVariable(store, 0L)

    val inv1 = SumBulkTestInvariant(store, inputs, output, "Bulk two times")
    val inv2 = SumBulkTestInvariant(store, inputs, output, "Bulk two times")
    store.close()

    inv1.bulk equals inv2.bulk should be(true)
  }

}
