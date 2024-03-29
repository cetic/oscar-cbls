package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.{KeyForRemoval, Store}

class AbstractComputationTestSuite extends AnyFunSuite {

  test("Constant variable can not have dynamically listening elements") {
    val store = new Store()
    val constantVariable = TestConstantVariable(store)
    val invariant        = TestInvariant(store)
    constantVariable.getTestDynamicallyListeningElements should be(null)
    val exception = intercept[IllegalArgumentException](
      invariant.registerDynamicallyAndStaticallyListenedElement(constantVariable)
    )
    assert(
      exception.getMessage.contains(
        "Constant variable does not propagate, no need to keep track of listening element."
      )
    )
  }

  test("Invariants keep track and can remove dynamically listening elements") {
    val store = new Store()
    val testInvariant = TestInvariant(store)
    val variable1     = TestVaryingVariable(store)
    val variable2     = TestVaryingVariable(store)
    val variable3     = TestVaryingVariable(store)
    val dynamicallyListenedVariables: Array[KeyForRemoval] = Array(
      testInvariant.registerDynamicallyAndStaticallyListenedElement(variable1, 0),
      testInvariant.registerDynamicallyAndStaticallyListenedElement(variable2, 1),
      testInvariant.registerDynamicallyAndStaticallyListenedElement(variable3, 2)
    )
    variable1.getTestDynamicallyListeningElements.size should be(1)
    variable1.getTestDynamicallyListeningElements.head should be(testInvariant)
    variable2.getTestDynamicallyListeningElements.size should be(1)
    variable2.getTestDynamicallyListeningElements.head should be(testInvariant)
    variable3.getTestDynamicallyListeningElements.size should be(1)
    variable3.getTestDynamicallyListeningElements.head should be(testInvariant)
    testInvariant.getDynamicallyListenedElements.size should be(3)

    store.setupPropagationStructure()

    dynamicallyListenedVariables(1).performRemove()
    variable2.getTestDynamicallyListeningElements.size should be(0)
    testInvariant.getDynamicallyListenedElements.size should be(2)
    dynamicallyListenedVariables(2).performRemove()
    variable3.getTestDynamicallyListeningElements.size should be(0)
    testInvariant.getDynamicallyListenedElements.size should be(1)
    dynamicallyListenedVariables(1) = testInvariant.registerDynamicallyListenedElement(variable2,1)
    variable2.getTestDynamicallyListeningElements.size should be(1)
    testInvariant.getDynamicallyListenedElements.size should be(2)
  }

  test("Domain definition works as expected") {
    val store = new Store()
    val variable = TestVaryingVariable(store)
    variable.domain should be(None)
    variable.checkValueWithinDomain(Long.MinValue) should be(true)
    variable.checkValueWithinDomain(Long.MaxValue) should be(true)
    variable.setDomain(47, 251)
    variable.checkValueWithinDomain(0) should be(false)
    variable.checkValueWithinDomain(487) should be(false)
    variable.checkValueWithinDomain(47) should be(true)
    variable.checkValueWithinDomain(251) should be(true)
    variable.checkValueWithinDomain(100) should be(true)
  }
}
