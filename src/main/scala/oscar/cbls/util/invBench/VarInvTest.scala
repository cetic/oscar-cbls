// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.util.invBench

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.seq.SeqVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.core.computation.Variable
import oscar.cbls.modeling.routing.VRS

import scala.collection.immutable.HashSet

/** Class defining the moves on the variables. Each type of variable has at least one type of move.
  *
  * @param varId
  *   The id of the variable.
  */
abstract class VariableMove(val varId: Int) {

  def mkMove(testVar: Variable): Unit

  def updateState(state: VariableState): VariableState
}

/** Class containing the states of the variable.
  *
  * The state is able to:
  *   - Know if a move can be done (according to the current state and the domain for the variables
  *     that have a domain).
  *   - Generate a move (according to the current state and the domain for the variables that have a
  *     domain.
  *
  * @param id
  *   The id of the variable.
  */
abstract class VariableState(id: Int) {
  def generateMove(): Gen[VariableMove]

  def canMake(m: VariableMove): Boolean
}

object VariableState {
  def apply(v: Variable, id: Int, routing: Option[VRS] = None): Gen[VariableState] = {
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
      case _: SeqVariable =>
        routing match {
          case None =>
            val domain = (0, 1000)
            Gen.const(SeqVariableState(id, SeqVariableStackableState(0, 0, None), domain))
          case Some(vrs) =>
            val routed = vrs.routes.pendingValue
            val unrouted: HashSet[Int] =
              HashSet.from(vrs.v until vrs.n).filter(!routed.contains(_))
            Gen.const(
              RoutingVariableState(
                id,
                SeqVariableStackableState(vrs.routes.value().size, 0, None),
                vrs,
                unrouted,
                routed
              )
            )
        }
    }
  }
}
