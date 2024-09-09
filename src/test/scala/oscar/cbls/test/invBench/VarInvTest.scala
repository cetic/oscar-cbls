package oscar.cbls.test.invBench

import oscar.cbls.core.computation.{Store, Variable}
import org.scalacheck.Gen
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import org.scalacheck.Arbitrary

/** The class that defines the moves on the variables. Each type of variable have at least one type
  * of move
  *
  * @param varId
  *   The id of the variable
  */

abstract class VariableMove(val varId: Int) {

  def mkMove(testVar: Variable): Unit

  def updateState(state: VariableState): VariableState
}

/** The class that contains the states of the variable.
  *
  * The state is able to:
  *   - Know if a move can be done (according to the current state and the domain for the variables
  *     that have a domain)
  *   - Generate a move (according to the current state and the domain for the variables that have a
  *     domain
  *
  * @param id
  *   The id of the variable
  */

abstract class VariableState(id: Int) {
  def generateMove(): Gen[VariableMove]

  def canMake(m: VariableMove): Boolean
}

object VariableState {
  def apply(v: Variable, id: Int) = {
    v match {
      case intVar: IntVariable =>
        val longGen: Gen[Long] =
          intVar.domain match {
            case None             => Arbitrary.arbitrary[Long]
            case Some((min, max)) => Gen.choose(min, max)
          }
        for (value <- longGen) yield IntVarState(value, id, intVar.domain)
      case setVar: SetVariable =>
        val intGen: Gen[Int] =
          setVar.domain match {
            case None             => Arbitrary.arbitrary[Int]
            case Some((min, max)) => Gen.choose(min.toInt, max.toInt)
          }
        for (l <- Gen.listOf(intGen))
          yield SetVarState(l.toSet, id, setVar.domain.map(d => (d._1.toInt, d._2.toInt)))
    }
  }

}
