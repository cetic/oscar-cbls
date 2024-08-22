package oscar.cbls.test.invBench

import oscar.cbls.core.computation.{Store, Variable}
import org.scalacheck.Gen
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import org.scalacheck.Arbitrary


abstract class VariableMove(val varId: Int) {

  def mkMove(testVar: Variable): Unit

  def updateState(state: VariableState): VariableState
}

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
        for (value <- longGen) yield IntegerState(value, id, intVar.domain)
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
