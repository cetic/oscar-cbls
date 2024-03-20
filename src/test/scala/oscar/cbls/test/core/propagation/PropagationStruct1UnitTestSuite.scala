package oscar.cbls.test.core.propagation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.propagation.{
  TestInvariantElement,
  TestPropagationStructure,
  TestVariableElement
}
import org.scalatest.Suites

/** Unit tests about the propagation. In this unit tests, the propagation structure is statically
  * defined and the updates are statically checked. The tests are parametrized by the debug level.
  * The actual tests are done in [[PropagationStruct1UnitTestSuites]]
  *
  * @param debugLevel the debug level for the test
  */
class PropagationStruct1UnitTestSuite(debugLevel: Int) extends AnyFunSuite with Matchers {

  val struct = new TestPropagationStructure(debugLevel)

  val var0 = new TestVariableElement(struct)
  val var1 = new TestVariableElement(struct)
  val var2 = new TestVariableElement(struct)
  val var3 = new TestVariableElement(struct)
  val inv4 = new TestInvariantElement(struct)
  inv4.registerStaticAndDynamicDependency(var0)
  inv4.registerStaticAndDynamicDependency(var1)
  var2.setDefiningInvariant(inv4)
  var3.setDefiningInvariant(inv4)

  val var5 = new TestVariableElement(struct)
  val var6 = new TestVariableElement(struct)
  val var7 = new TestVariableElement(struct)
  val var8 = new TestVariableElement(struct)
  val inv9 = new TestInvariantElement(struct)
  inv9.registerStaticAndDynamicDependency(var5)
  inv9.registerStaticAndDynamicDependency(var6)
  var7.setDefiningInvariant(inv9)
  var8.setDefiningInvariant(inv9)

  // Put this flag at true to get a dot version of the propagation flag
  // The propagation flag is saved in Graph.gv
  // To get the graph in svg use
  // $> dot -Tsvg Graph.gv -o Graph.svg
  val outGraphAsDot = false
  if (outGraphAsDot) {
    new java.io.PrintWriter("Graph.gv") {
      write(struct.myToDot())
      close()
    }
  }

  struct.registerForPartialPropagation(var2)
  struct.registerForPartialPropagation(var7)

  struct.close

  private def checkUpdate(variable: TestVariableElement, value: Int) = {
    assert(
      variable.nbUpdate == value,
      s"Wrong nb of update for ${variable.name} (${variable.nbUpdate} instead of $value)"
    )
  }

  test(
    "Unit test: only the variable that are impacted by a change are updated during a total propagation"
  ) {

    var0.update
    struct.totalPropagation(false)

    checkUpdate(var0, 1)
    checkUpdate(var1, 0)
    checkUpdate(var5, 0)
    checkUpdate(var6, 0)
    checkUpdate(var2, 1)
    checkUpdate(var3, 1)
    checkUpdate(var7, 0)
    checkUpdate(var8, 0)

    struct.resetPropagationFlags
  }

  test(
    "Unit test: in a partial propagation, only the variable that are required for a target are updated"
  ) {

    var0.update
    struct.partialPropagation(var7, false)
    if (debugLevel < 3) {
      checkUpdate(var0, 0)
      checkUpdate(var1, 0)
      checkUpdate(var5, 0)
      checkUpdate(var6, 0)
      checkUpdate(var2, 0)
      checkUpdate(var3, 0)
      checkUpdate(var7, 0)
      checkUpdate(var8, 0)
    } else {
      checkUpdate(var0, 1)
      checkUpdate(var1, 0)
      checkUpdate(var5, 0)
      checkUpdate(var6, 0)
      checkUpdate(var2, 1)
      checkUpdate(var3, 1)
      checkUpdate(var7, 0)
      checkUpdate(var8, 0)
    }

    struct.resetPropagationFlags
  }

  test(
    "Unit test: after a partial propagation, if a total propagation is triggered, the postponed variables are updated"
  ) {

    struct.totalPropagation(false)

    if (debugLevel < 3) {
      checkUpdate(var0, 1)
      checkUpdate(var1, 0)
      checkUpdate(var5, 0)
      checkUpdate(var6, 0)
      checkUpdate(var2, 1)
      checkUpdate(var3, 1)
      checkUpdate(var7, 0)
      checkUpdate(var8, 0)
    } else {
      checkUpdate(var0, 0)
      checkUpdate(var1, 0)
      checkUpdate(var5, 0)
      checkUpdate(var6, 0)
      checkUpdate(var2, 0)
      checkUpdate(var3, 0)
      checkUpdate(var7, 0)
      checkUpdate(var8, 0)
    }

    struct.resetPropagationFlags
  }

  test(
    "Unit test: A partial propagation where the target is not registered for partial propagation has the same effect as a total propagation"
  ) {

    var0.update
    struct.partialPropagation(var8, false)

    checkUpdate(var0, 1)
    checkUpdate(var1, 0)
    checkUpdate(var5, 0)
    checkUpdate(var6, 0)
    checkUpdate(var2, 1)
    checkUpdate(var3, 1)
    checkUpdate(var7, 0)
    checkUpdate(var8, 0)

    struct.resetPropagationFlags
  }

  test(
    "Unit test: after a partial propagation, if a new partial propagation is triggered, the postponed variables that are concerned by this propagation are updated"
  ) {

    var0.update
    struct.partialPropagation(var7, false)
    if (debugLevel < 3) {
      checkUpdate(var0, 0)
      checkUpdate(var1, 0)
      checkUpdate(var5, 0)
      checkUpdate(var6, 0)
      checkUpdate(var2, 0)
      checkUpdate(var3, 0)
      checkUpdate(var7, 0)
      checkUpdate(var8, 0)
    } else {
      checkUpdate(var0, 1)
      checkUpdate(var1, 0)
      checkUpdate(var5, 0)
      checkUpdate(var6, 0)
      checkUpdate(var2, 1)
      checkUpdate(var3, 1)
      checkUpdate(var7, 0)
      checkUpdate(var8, 0)
    }

    struct.resetPropagationFlags
    struct.partialPropagation(var2, false)

    if (debugLevel < 3) {
      checkUpdate(var0, 1)
      checkUpdate(var1, 0)
      checkUpdate(var5, 0)
      checkUpdate(var6, 0)
      checkUpdate(var2, 1)
      checkUpdate(var3, 0)
      checkUpdate(var7, 0)
      checkUpdate(var8, 0)
    } else {
      checkUpdate(var0, 0)
      checkUpdate(var1, 0)
      checkUpdate(var5, 0)
      checkUpdate(var6, 0)
      checkUpdate(var2, 0)
      checkUpdate(var3, 0)
      checkUpdate(var7, 0)
      checkUpdate(var8, 0)
    }

    struct.resetPropagationFlags

  }

  test(
    "Unit test: after the last partial propagation, the postponed variables are updated by a total propagation"
  ) {

    struct.totalPropagation(false)

    if (debugLevel < 3) {
      checkUpdate(var0, 0)
      checkUpdate(var1, 0)
      checkUpdate(var5, 0)
      checkUpdate(var6, 0)
      checkUpdate(var2, 0)
      checkUpdate(var3, 1)
      checkUpdate(var7, 0)
      checkUpdate(var8, 0)
    } else {
      checkUpdate(var0, 0)
      checkUpdate(var1, 0)
      checkUpdate(var5, 0)
      checkUpdate(var6, 0)
      checkUpdate(var2, 0)
      checkUpdate(var3, 0)
      checkUpdate(var7, 0)
      checkUpdate(var8, 0)
    }

  }

}

class PropagationStruct1UnitTestSuites
    extends Suites(Seq.tabulate(4)(new PropagationStruct1UnitTestSuite(_)): _*)
