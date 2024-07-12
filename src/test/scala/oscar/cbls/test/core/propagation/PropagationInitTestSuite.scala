package oscar.cbls.test.core.propagation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.propagation.{
  TestInvariantElement,
  TestPropagationStructure,
  TestVariableElement
}

/** Tests about the initialisation of the propagation structure
  */

class PropagationInitTestSuite extends AnyFunSuite with Matchers {

  val seed: Option[Long] = Some(3000)

  val structureGenerator = new PropagationStructureGenerator(seed)

  test("An error shall be raised when there is a cycle in the propagation structure") {
    val struct = new TestPropagationStructure

    val var1: TestVariableElement  = new TestVariableElement(struct)
    val var2: TestVariableElement  = new TestVariableElement(struct)
    val var3: TestVariableElement  = new TestVariableElement(struct)
    val inv1: TestInvariantElement = new TestInvariantElement(struct)
    val inv2: TestInvariantElement = new TestInvariantElement(struct)

    var1.registerListeningElement(inv1)
    var2.setDefiningInvariant(inv1)
    var2.registerListeningElement(inv2)
    var3.setDefiningInvariant(inv2)
    var3.registerListeningElement(inv1)

    an[java.lang.IllegalArgumentException] should be thrownBy struct.close()
  }

  test("An error shall be raised when the debug level is higher that 3 or lower that 0") {
    an[java.lang.IllegalArgumentException] should be thrownBy new TestPropagationStructure(4)
    an[java.lang.IllegalArgumentException] should be thrownBy new TestPropagationStructure(-1)
  }

  test("The layer computation algorithm is coherent with the layer computed on build") {
    for (_ <- 0 to 10) {
      val struct = structureGenerator.generateStructure(15, 30, 5, 15, 5, 15, 5, 15, 10)

      struct.close()
      struct.validateLayerAssignation()

    }
  }

}
