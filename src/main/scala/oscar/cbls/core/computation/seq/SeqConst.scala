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

package oscar.cbls.core.computation.seq

import oscar.cbls.core.computation.Store

/** Used to create constant SeqVariable
  */
object SeqConst {
  private var nextNameCounter: Int = -1
  private def nextName(): String = {
    nextNameCounter += 1
    s"SeqConst_$nextNameCounter"
  }

  /** Creates a constant SeqVariable based on parameters.
    *
    * This variable value cannot be modified.
    *
    * @param model
    *   The propagation structure to which the element is attached.
    * @param initialValue
    *   The initial value of the SeqVariable.
    * @param name
    *   The (optional) name of the Variable.
    * @param maxPivotPerValuePercent
    *   The maximum number of [[oscar.cbls.algo.sequence.affineFunction.Pivot]] per 100 value in the
    *   IntSequence. When defining a new checkpoint, if this value is exceeded, a regularization is
    *   done.
    * @return
    *   A SeqVariable with the constant flag at true.
    */
  def apply(
    model: Store,
    initialValue: List[Int],
    name: String = nextName(),
    maxPivotPerValuePercent: Int = 4
  ): SeqVariable = {
    SeqVariable(model, initialValue, name, maxPivotPerValuePercent, isConstant = true)
  }
}
