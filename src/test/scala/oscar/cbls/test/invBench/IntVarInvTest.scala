package oscar.cbls.test.invBench

import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import oscar.cbls.core.computation.Variable
import oscar.cbls.core.computation.integer.IntVariable

case class IntegerAssignMove(newValue: Long,id : Int) extends VariableMove(id) {

  override def mkMove(testVar : Variable): Unit = {
    testVar match {
      case intTestVar : IntVariable => intTestVar := newValue
        intTestVar.model.propagate()
      case _ => throw new Error("Int Movement can only update variable of type Int")
    }
  }

  def updateState(state : VariableState) =
    state match {
      case intState : IntegerState => IntegerState(newValue,intState.id,intState.domain)
      case _ => throw new Error("Int Movement can only update state of type IntegerState")
    }

}

case class IntegerState(value: Long,id : Int,domain : Option[(Long,Long)]) extends VariableState(id) {

  override def canMake(m : VariableMove) : Boolean = true

  override def generateMove(): Gen[VariableMove] = {
      domain match {
        case None =>
          for { value <- arbitrary[Long] } yield IntegerAssignMove(value,id)
        case Some((min, max)) =>
          for {
            value <- Gen.choose(min, max)
          } yield IntegerAssignMove(value,id)
      }
    }
}
