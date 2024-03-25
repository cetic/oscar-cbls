package oscar.cbls.test.core.propagation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.propagation.{
  TestInvariantElement,
  TestPropagationStructure,
  TestVariableElement
}

/** Tests about the consistency of the propagation structure
  */

class PropagationConsistencyTestSuite extends AnyFunSuite with Matchers {
  test("An error should be raised when to element listen from each other from two different structures") {
    val struct1 = new TestPropagationStructure()
    val elem1 = new TestVariableElement(struct1)

    val struct2 = new TestPropagationStructure()
    val elem2 = new TestInvariantElement(struct2)

    an[java.lang.IllegalArgumentException] should be thrownBy elem2.registerStaticAndDynamicDependency(elem1)

  }

}
