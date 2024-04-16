package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.{IncredibleBulk, Solution, Store, Variable}

class AbstractComputationTestSuite extends AnyFunSuite {

  test("Constant variable can not have dynamically listening elements") {
    val store = new Store()
    val constantVariable = TestVaryingVariable(store,constant = true)
    constantVariable.getTestDynamicallyListeningElements should be(null)

    val exception = intercept[IllegalArgumentException](
      TestInvariant(store,List(constantVariable), None)
    )
    assert(
      exception.getMessage.contains(
        "Constant variable does not propagate, no need to keep track of listening element."
      )
    )
  }

  test("Invariants keep track and can remove dynamically listening elements") {
    val store = new Store()
    val variable1     = TestVaryingVariable(store)
    val variable2     = TestVaryingVariable(store)
    val variable3     = TestVaryingVariable(store)
    val testInvariant = TestInvariant(store, List(variable1,variable2,variable3), None)

    variable1.getTestDynamicallyListeningElements.size should be(1)
    variable1.getTestDynamicallyListeningElements.head should be((testInvariant,-1))
    variable2.getTestDynamicallyListeningElements.size should be(1)
    variable2.getTestDynamicallyListeningElements.head should be((testInvariant,-1))
    variable3.getTestDynamicallyListeningElements.size should be(1)
    variable3.getTestDynamicallyListeningElements.head should be((testInvariant,-1))

    store.close()

    testInvariant.keysForRemoval(1).stopListeningToThisElement()
    variable2.getTestDynamicallyListeningElements.size should be(0)
    testInvariant.keysForRemoval(2).stopListeningToThisElement()
    variable3.getTestDynamicallyListeningElements.size should be(0)
    testInvariant.keysForRemoval(1) = variable2.registerDynamicallyListeningElement(testInvariant,1)
    variable2.getTestDynamicallyListeningElements.size should be(1)
  }

  test("Domain definition works as expected") {
    val store = new Store()
    val variable = TestVaryingVariable(store)
    variable.domain should be(None)
    variable.isValueWithinDomain(Long.MinValue) should be(true)
    variable.isValueWithinDomain(Long.MaxValue) should be(true)
    variable.setDomain(47, 251)
    variable.isValueWithinDomain(0) should be(false)
    variable.isValueWithinDomain(487) should be(false)
    variable.isValueWithinDomain(47) should be(true)
    variable.isValueWithinDomain(251) should be(true)
    variable.isValueWithinDomain(100) should be(true)
  }

  test("Variable bulking works as expected"){
    val store = new Store()
    val bulkedVariables: List[TestVaryingVariable] = List.fill(25)(TestVaryingVariable(store))
    val invariant1 = TestInvariant(store, List.empty, None)
    val invariant2 = TestInvariant(store, List.empty, None)

    val bulk1 = IncredibleBulk.bulkRegistering(bulkedVariables, "MyIncredibleBulk", store)
    val bulk2 = IncredibleBulk.bulkRegistering(bulkedVariables, "MyIncredibleBulk", store)

    bulk1 should be(bulk2)

    invariant1.addIncredibleBulk(bulk1)
    invariant2.addIncredibleBulk(bulk2)
    store.close()
  }

  test("Saving and loading a Solution works as expected"){
    val store = new Store()
    val variable1 = TestVaryingVariable(store, 1)
    val variable2 = TestVaryingVariable(store, 2)
    val variable3 = TestVaryingVariable(store, 3)
    val variable4 = TestVaryingVariable(store, 4)
    val variable5 = TestVaryingVariable(store, 5)
    val variable6 = TestVaryingVariable(store, 6)

    TestInvariant(store, List(variable1,variable2), Some(variable4))
    TestInvariant(store, List(variable2,variable3), Some(variable5))
    TestInvariant(store, List(variable4,variable5), Some(variable6))

    // variable4 = (variable1+variable2)
    // variable5 = (variable2+variable3)
    // variable6 = (variable4+variable5)

    store.close()
    store.performTotalPropagation()

    // start : variable6 = (1+2)+(2+3) = 8
    variable6.value should be(8)

    val solution: Solution = store.save

    variable1.setValue(10)
    store.performPartialPropagation(variable6)
    // start : variable6 = (10+2)+(2+3) = 17
    variable6.value should be(17)

    variable2.setValue(5)
    store.performPartialPropagation(variable6)
    // start : variable6 = (10+5)+(5+3) = 23
    variable6.value should be(23)

    solution.restoreSolution()
    // start : variable6 = (1+2)+(2+3) = 8
    variable6.value should be(8)
  }
}
