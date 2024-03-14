package oscar.cbls.test.core.propagation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.propagation.{TestVariableElement,TestInvariantElement,TestPropagationStructure}

class PropagationStruct1UnitTestSuite extends AnyFunSuite with Matchers {

  val struct = new TestPropagationStructure

  val in1_1 = new TestVariableElement(struct)
  val in1_2 = new TestVariableElement(struct)
  val out1_1 = new TestVariableElement(struct)
  val out1_2 = new TestVariableElement(struct)
  val inv1 = new TestInvariantElement(struct)
  inv1.registerStaticAndDynamicDependency(in1_1)
  inv1.registerStaticAndDynamicDependency(in1_2)
  out1_1.setDefiningInvariant(inv1)
  out1_2.setDefiningInvariant(inv1)

  val in2_1 = new TestVariableElement(struct)
  val in2_2 = new TestVariableElement(struct)
  val out2_1 = new TestVariableElement(struct)
  val out2_2 = new TestVariableElement(struct)
  val inv2 = new TestInvariantElement(struct)
  inv2.registerStaticAndDynamicDependency(in2_1)
  inv2.registerStaticAndDynamicDependency(in2_2)
  out2_1.setDefiningInvariant(inv2)
  out2_2.setDefiningInvariant(inv2)

  val outGraphAsDot = true
  if (outGraphAsDot) {
    new java.io.PrintWriter("Graph.gv") {
      write(struct.myToDot())
      close()
    }
  }

  struct.registerForPartialPropagation(out2_1)
  struct.registerForPartialPropagation(out1_1)

  struct.close

  private def checkUpdate(variable : TestVariableElement,value : Int) = {
    assert(variable.nbUpdate == value,s"Wrong nb of update for ${variable.name} (${variable.nbUpdate} instead of $value)")
  }

  test("Unit test: only the variable that are impacted by a change are updated during a total propagation") {

    in1_1.update
    struct.totalPropagation(false)

    checkUpdate(in1_1,1)
    checkUpdate(in1_2,0)
    checkUpdate(in2_1,0)
    checkUpdate(in2_2,0)
    checkUpdate(out1_1,1)
    checkUpdate(out1_2,1)
    checkUpdate(out2_1,0)
    checkUpdate(out2_2,0)

    struct.resetPropagationFlags
  }


  test("Unit test: in a partial propagation, only the variable that are required for a target are updated") {

    in1_1.update
    struct.partialPropagation(out2_1,false)

    checkUpdate(in1_1,0)
    checkUpdate(in1_2,0)
    checkUpdate(in2_1,0)
    checkUpdate(in2_2,0)
    checkUpdate(out1_1,0)
    checkUpdate(out1_2,0)
    checkUpdate(out2_1,0)
    checkUpdate(out2_2,0)

    struct.resetPropagationFlags
  }

  test("Unit test: after a partial propagation, if a total propagation is triggered, the postponed variables are updated") {

    struct.totalPropagation(false)

    checkUpdate(in1_1,1)
    checkUpdate(in1_2,0)
    checkUpdate(in2_1,0)
    checkUpdate(in2_2,0)
    checkUpdate(out1_1,1)
    checkUpdate(out1_2,1)
    checkUpdate(out2_1,0)
    checkUpdate(out2_2,0)

    struct.resetPropagationFlags
  }

  test("Unit test: A partial propagation where the target is not registered for partial propagation has the same effect as a total propagation") {

    in1_1.update
    struct.partialPropagation(out2_2,false)

    checkUpdate(in1_1,1)
    checkUpdate(in1_2,0)
    checkUpdate(in2_1,0)
    checkUpdate(in2_2,0)
    checkUpdate(out1_1,1)
    checkUpdate(out1_2,1)
    checkUpdate(out2_1,0)
    checkUpdate(out2_2,0)

    struct.resetPropagationFlags
  }

  test("Unit test: after a partial propagation, if a new partial propagation is triggered, the postponed variables that are concerned by this propagation are updated") {

    in1_1.update
    struct.partialPropagation(out2_1,false)
    struct.partialPropagation(out1_1,false)

    checkUpdate(in1_1,1)
    checkUpdate(in1_2,0)
    checkUpdate(in2_1,0)
    checkUpdate(in2_2,0)
    checkUpdate(out1_1,1)
    checkUpdate(out1_2,0)
    checkUpdate(out2_1,0)
    checkUpdate(out2_2,0)

    struct.resetPropagationFlags

  }

  test("Unit test: after the last partial propagation, the postponed variables are updated by a total propagation") {

    struct.totalPropagation(false)

    checkUpdate(in1_1,0)
    checkUpdate(in1_2,0)
    checkUpdate(in2_1,0)
    checkUpdate(in2_2,0)
    checkUpdate(out1_1,0)
    checkUpdate(out1_2,1)
    checkUpdate(out2_1,0)
    checkUpdate(out2_2,0)

  }



}
