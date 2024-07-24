package oscar.cbls.test.testBench

import oscar.cbls.core.computation.{Invariant, Variable}
import org.scalacheck.Gen
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable

abstract class TestVariable(val variable: Variable) {
  val model = variable.model

  def generateMove(): Gen[VariableMove]

}

object TestVariable {
  def apply(v: Variable): TestVariable = {
    v match {
      case intVar: IntVariable => new IntTestVariable(intVar)
      case setVar: SetVariable => new SetTestVariable(setVar)
      case _ => throw new Error(s"This type of variable ($v) is not yet handled by the test bench")
    }
  }
}

abstract class VariableMove(testVar: TestVariable) {
  def mkMove(): Unit
}
