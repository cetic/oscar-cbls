package oscar.cbls.test.invBench

import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import oscar.cbls.core.computation.Variable
import oscar.cbls.core.computation.integer.IntVariable

/** Defines the move that assignes a value to a IntVariable
  *
  * @param newValue
  *   The new value that will be assigned
  * @param id
  *   The id of the variable
  */

case class IntegerAssignMove(newValue: Long, id: Int) extends VariableMove(id) {

  override def toString(): String = s"IntegerAssignMove($newValue)"

  override def mkMove(testVar: Variable): Unit = {
    testVar match {
      case intTestVar: IntVariable =>
        intTestVar := newValue
        intTestVar.model.propagate()
      case _ => throw new Error("Int Movement can only update variable of type Int")
    }
  }

  def updateState(state: VariableState) =
    state match {
      case intState: IntVarState => IntVarState(newValue, intState.id, intState.domain)
      case _ => throw new Error("Int Movement can only update state of type IntegerState")
    }

}

/** Defines the state of an Int variable
  *
  * @param value
  *   The current value of the variable
  * @param id
  *   The id of the variable
  * @param domain
  *   The domain of the variable if there is one
  */

case class IntVarState(value: Long, id: Int, domain: Option[(Long, Long)])
    extends VariableState(id) {

  override def toString(): String = {
    val domainString = domain.map(d => ", domain=[${d._1},${d._2}]").getOrElse("")
    s"IntegerState($value$domainString)"
  }

  override def canMake(m: VariableMove): Boolean = {
    m match {
      case IntegerAssignMove(newValue, moveId) =>
        moveId == id && domain.map(d => d._1 <= newValue && newValue <= d._2).getOrElse(true)
      case _ => throw new Error("IntVarState can only test moves of type IntegerAssignMove")
    }
  }

  override def generateMove(): Gen[VariableMove] = {
    domain match {
      case None =>
        for { value <- arbitrary[Long] } yield IntegerAssignMove(value, id)
      case Some((min, max)) =>
        for {
          value <- Gen.choose(min, max)
        } yield IntegerAssignMove(value, id)
    }
  }
}
