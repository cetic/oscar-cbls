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

/** Naive invariant which maintains the sum of distances for nodes in sequences. Used for tests on
  * little sequences
  */
private[neighborhoods] class NaiveSumDistancesInvariant(
  model: Store,
  input: SeqVariable,
  distances: Array[Array[Long]],
  output: IntVariable
) extends Invariant(model, None)
    with SeqNotificationTarget {

  input.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  affectOutputFromScratch()

  override def notifySeqChanges(
    seqVariable: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit = {
    affectOutputFromScratch()
  }

  override def checkInternals(): Unit = {}

  private[this] def affectOutputFromScratch(): Unit = {
    var tmp         = 0L
    val seqIterator = input.pendingValue.iterator
    var previous    = if (seqIterator.hasNext) seqIterator.next().value else 0
    while (seqIterator.hasNext) {
      val current = seqIterator.next().value
      tmp += distances(previous)(current)
      previous = current
    }
    output := tmp
  }
}
