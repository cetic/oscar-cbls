package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store

class AbstractComputationTestSuite extends AnyFunSuite{

  val store = new Store()

  test("Constant variable can not have dynamically listening elements"){
    val constantVariable = TestConstantVariable(store)
    val varyingVariable = TestVaryingVariable(store)
    constantVariable.getTestDynamicallyListeningElements should be(null)
    val exception = intercept[IllegalArgumentException](constantVariable.registerDynamicallyListeningElement(varyingVariable))
    assert(exception.getMessage.contains("Constant variable does not propagate, no need to keep track of listening element."))
  }

  test("Varying variable keep track and can remove dynamically listening elements"){
    val varyingVariable = TestVaryingVariable(store)
    val variable1 = TestConstantVariable(store)
    val variable2 = TestVaryingVariable(store)
    val variable3 = TestInvariant(store)
    varyingVariable.getTestDynamicallyListeningElements.isEmpty should be(true)
    val keyForRemoval1 = varyingVariable.registerDynamicallyListeningElement(variable1)
    varyingVariable.registerDynamicallyListeningElement(variable2)
    varyingVariable.registerDynamicallyListeningElement(variable3)
    varyingVariable.getTestDynamicallyListeningElements.nonEmpty should be(true)
    varyingVariable.getTestDynamicallyListeningElements.size should be(3)
    keyForRemoval1.performRemove
    varyingVariable.getTestDynamicallyListeningElements.nonEmpty should be(true)
    varyingVariable.getTestDynamicallyListeningElements.size should be(2)
    varyingVariable.getTestDynamicallyListeningElements.toList.contains(variable1) should be(false)
  }

  test("Domain definition works as expected"){
    val variable = TestVaryingVariable(store)
    variable.domain should be(None)
    variable.checkValueWithinDomain(Long.MinValue) should be(true)
    variable.checkValueWithinDomain(Long.MaxValue) should be(true)
    variable.setDomain(47,251)
    variable.checkValueWithinDomain(0) should be(false)
    variable.checkValueWithinDomain(487) should be(false)
    variable.checkValueWithinDomain(47) should be(true)
    variable.checkValueWithinDomain(251) should be(true)
    variable.checkValueWithinDomain(100) should be(true)
  }

  test("The Store distinguish the decision variable from the others"){
    val defined = TestVaryingVariable(store)
    val listened = TestVaryingVariable(store)
    val invariant = TestInvariant(store)
    invariant.addDefinedVariable(defined)
    invariant.addListenedVariable(listened)
    store.setupPropagationStructure()
    store.decisionVariable(listened.)
  }

}
