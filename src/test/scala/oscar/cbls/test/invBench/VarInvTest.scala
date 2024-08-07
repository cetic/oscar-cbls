package oscar.cbls.test.invBench

import oscar.cbls.core.computation.{Store, Variable}
import org.scalacheck.Gen
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable

abstract class TestVariable(val variable: Variable) {
  val model: Store = variable.model

  def generateMove(): Gen[VariableMove]

  def generateAssignMove(): Gen[VariableMove]
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
