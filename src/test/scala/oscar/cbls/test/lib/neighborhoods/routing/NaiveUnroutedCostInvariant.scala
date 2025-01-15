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

package oscar.cbls.test.lib.neighborhoods.routing

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.seq.{SeqNotificationTarget, SeqUpdate, SeqVariable}
import oscar.cbls.core.computation.{Invariant, Store}

class NaiveUnroutedCostInvariant(
  model: Store,
  input: SeqVariable,
  maxPossibleValue: Int,
  unroutedCost: Long,
  output: IntVariable
) extends Invariant(model, None)
    with SeqNotificationTarget {

  input.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  affectOutputFromScratch(input)

  override def notifySeqChanges(
    seqVariable: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit =
    affectOutputFromScratch(seqVariable)

  override def checkInternals(): Unit = {}

  private[this] def affectOutputFromScratch(seq: SeqVariable): Unit = {
    var tmp = 0L
    for (node <- 0 until maxPossibleValue)
      if (!seq.pendingValue.contains(node)) tmp += unroutedCost
    output := tmp
  }
}
