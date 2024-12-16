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

package oscar.cbls.lib.neighborhoods.combinator

import oscar.cbls.core.computation.integer.IntSavedValue
import oscar.cbls.core.computation.seq.SeqSavedValue
import oscar.cbls.core.computation.set.SetSavedValue
import oscar.cbls.core.computation.{SavedValue, Solution}
import oscar.cbls.core.search.Move

case class LoadSolutionMove(
  s: Solution,
  override val objValueAfter: Long,
  override val neighborhoodName: String
) extends Move(objValueAfter, neighborhoodName) {

  private val moveString: String = {
    var str: String = ""
    for (savedValue: SavedValue <- s.savedValues) {
      savedValue match {
        case intSavedValue: IntSavedValue =>
          if (!intSavedValue.variable.isConstant)
            str += s"\t- value ${intSavedValue.savedValue} to variable ${intSavedValue.variable}\n"
        case setSavedValue: SetSavedValue =>
          if (!setSavedValue.variable.isConstant)
            str += s"\t- value ${setSavedValue.savedValue} to variable ${setSavedValue.variable}\n"
        case seqSavedValue: SeqSavedValue =>
          if (!seqSavedValue.variable.isConstant)
            str += s"\t- value ${seqSavedValue.savedValue} to variable ${seqSavedValue.variable}\n"
      }
    }
    str
  }

  override def commit(): Unit = s.restoreSolution()

  override def toString: String = {
    s""" $neighborhoodName ${super.toString}
       |Loads:
       |$moveString""".stripMargin
  }
}
