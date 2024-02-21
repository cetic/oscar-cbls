package oscar.cbls.test.core.propagation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.propagation.{TestVariableElement,TestInvariantElement,TestPropagationStructure}

class PropagationSuite extends AnyFunSuite with Matchers {

  val structureGenerator = new PropagationStructureGenerator

  test("An error shall be raised when there is a cycle in the propagation structure") {
    val struct = new TestPropagationStructure

    val var1 : TestVariableElement = new TestVariableElement(struct)
    val var2 : TestVariableElement = new TestVariableElement(struct)
    val var3 : TestVariableElement = new TestVariableElement(struct)
    val inv1 : TestInvariantElement = new TestInvariantElement(struct)
    val inv2 : TestInvariantElement = new TestInvariantElement(struct)

    inv1.registerStaticAndDynamicDependency(var1)
    var2.setDefiningInvariant(inv1)
    inv2.registerStaticAndDynamicDependency(var2)
    var3.setDefiningInvariant(inv2)
    inv1.registerStaticAndDynamicDependency(var3)

    an[java.lang.IllegalArgumentException] should be thrownBy struct.close
  }

  // test("The layer computation algorithm is coherent with the layer computed on build") {
  //   for (_ <- 0 to 10) {
  //     val (struct,elements) = structureGenerator.generateStructure(5,15,5,15,5,15,10)

  //     struct.close

  //     elements.foreach(_.validateLayer)

  //   }
  // }


  test("test") {
    val (struct,elements) = structureGenerator.generateStructure(3,6,1,4,1,4,5)

    val name = "Variable 22"

    struct.registerForPartialPropagation(elements.filter(_.name == name)(0))

    struct.close
    struct.validateLayerAssignation




    new java.io.PrintWriter("Graph.gv") {
      write(struct.myToDot(Some(elements.filter(_.name == name)(0))))
      close()
    }

  }

}
