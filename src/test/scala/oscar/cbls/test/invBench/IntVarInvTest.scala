package oscar.cbls.test.invBench

import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import oscar.cbls.core.computation.integer.IntVariable

class IntTestVariable(override val variable: IntVariable) extends TestVariable(variable) {
  override def generateMove(): Gen[VariableMove] = {
    variable.domain match {
      case None =>
        for { value <- arbitrary[Long] } yield IntegerAssignMove(this, value)
      case Some((min, max)) =>
        for {
          value <- Gen.choose(min, max)
        } yield IntegerAssignMove(this, value)
    }
  }

  override def generateAssignMove(): Gen[VariableMove] = generateMove()

  override def toString: String = {
    s"IntVariable(${variable})"
  }

}

case class IntegerAssignMove(testVar: IntTestVariable, value: Long) extends VariableMove(testVar) {
  override def mkMove(): Unit = {
    testVar.variable := value
    testVar.variable.model.propagate()
  }

}
