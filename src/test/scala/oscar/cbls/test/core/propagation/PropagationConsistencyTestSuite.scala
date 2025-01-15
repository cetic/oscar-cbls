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
  test(
    "An error should be raised when two elements listen from each other from two different structures"
  ) {
    val struct1 = new TestPropagationStructure()
    val elem1   = new TestVariableElement(struct1)

    val struct2 = new TestPropagationStructure()
    val elem2   = new TestInvariantElement(struct2)

    an[java.lang.IllegalArgumentException] should be thrownBy elem1.registerListeningElement(elem2)

  }

  test("Nothing should be done when propagating a structure that is not closed") {
    val struct1 = new TestPropagationStructure()
    val elem1   = new TestVariableElement(struct1)
    val elem2   = new TestInvariantElement(struct1)

    elem1.registerListeningElement(elem2)

    elem1.update()

    struct1.totalPropagation(false)

    assert(elem1.nbUpdate == 0)
    assert(elem2.nbUpdate == 0)

  }

  test("No static link can be added when the structure is closed") {
    val struct1 = new TestPropagationStructure()
    val var1    = new TestVariableElement(struct1)
    val inv1    = new TestInvariantElement(struct1)
    var1.registerListeningElement(inv1)

    val var2 = new TestVariableElement(struct1)

    struct1.close()

    an[java.lang.IllegalArgumentException] should be thrownBy var2.setDefiningInvariant(inv1)

  }

  test("The static graph shall be dropped after closing the structure") {
    val struct1 = new TestPropagationStructure()

    val var1 = new TestVariableElement(struct1)
    val inv1 = new TestInvariantElement(struct1)
    var1.registerListeningElement(inv1)

    struct1.close()

    assert(var1.staticGraphIsNull)
    assert(inv1.staticGraphIsNull)
  }

  test("A element cannot be registered for partial propagation when the structure is closed") {
    val struct1 = new TestPropagationStructure()
    val var1    = new TestVariableElement(struct1)
    val inv1    = new TestInvariantElement(struct1)
    val var2    = new TestVariableElement(struct1)

    var1.registerListeningElement(inv1)
    var2.setDefiningInvariant(inv1)

    struct1.close()

    an[java.lang.IllegalArgumentException] should be thrownBy struct1.registerForPartialPropagation(
      var2
    )
  }

  test("Cannot setup a structure that is already closed") {
    val struct1 = new TestPropagationStructure()

    struct1.close()

    an[java.lang.IllegalArgumentException] should be thrownBy struct1.close()
  }

}
