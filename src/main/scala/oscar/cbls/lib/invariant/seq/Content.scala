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

package oscar.cbls.lib.invariant.seq

import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.computation.seq._
import oscar.cbls.core.computation.set.SetVariable

import scala.collection.mutable

/** Companion object of the [[Content]] class. */
object Content {

  /** Creates a Content Invariant that maintains the content of a SeqVariable as a SetVariable.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param input
    *   The SeqVariable whose content is maintained by this Invariant.
    * @param output
    *   The content of the input variable as a Set.
    */
  def apply(model: Store, input: SeqVariable, output: SetVariable): Content = {
    new Content(model, input, output)
  }
}

/** Invariant that maintains the content of a SeqVariable as a SetVariable.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   The SeqVariable whose content is maintained by this Invariant.
  * @param output
  *   The content of the input variable as a Set.
  */
class Content(model: Store, input: SeqVariable, output: SetVariable)
    extends Invariant(model, Some(s"Content maintainer of ${input.name()}"))
    with SeqNotificationTarget {

  input.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  updateFromScratch(input.value)

  /* HashMap maintaining the number of duplicated elements.
   * Only duplicated one. ex : three times 54 in the input sequence ==> (54 -> 2) in the HashMap
   */
  private var duplicatedElements: mutable.HashMap[Int, Int] = mutable.HashMap.empty

  /** Notifies the listening [[oscar.cbls.core.propagation.PropagationElement]] that the listened
    * [[oscar.cbls.core.computation.seq.SeqVariable]] has changed.
    *
    * Implemented by the listening [[oscar.cbls.core.propagation.PropagationElement]]. Called by the
    * listened [[oscar.cbls.core.computation.seq.SeqVariable]].
    *
    * @param v
    *   The listened SeqVariable.
    * @param contextualVarIndex
    *   The optional index of the SeqVariable in the context of the listening Invariant. Default -1
    * @param changes
    *   A stacked list of SeqUpdate, the first one represents the latest update. Use its prev value
    *   to get the previous SeqUpdate...
    */
  override def notifySeqChanges(
    v: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit = {
    if (!digestUpdates(changes)) {
      updateFromScratch(changes.newValue)
    }
  }

  /** Digests the batch of updates.
    *
    * @param changes
    *   The batch of updates since the last propagation.
    * @return
    *   True if all the updates could be treated incrementally false otherwise.
    */
  private def digestUpdates(changes: SeqUpdate): Boolean = {
    changes match {
      case SeqUpdateInsert(value, _, prev) =>
        if (!digestUpdates(prev)) return false
        if (!(output :+= value)) {
          if (duplicatedElements.contains(value)) duplicatedElements(value) += 1
          else duplicatedElements.addOne((value, 1))
        }
        true
      case SeqUpdateMove(_, _, _, _, prev) => digestUpdates(prev)
      case SeqUpdateRemove(removedPositionExplorer, prev) =>
        if (!digestUpdates(prev)) return false
        val removedValue: Int = removedPositionExplorer.value
        duplicatedElements.get(removedValue) match {
          case None => output :-= removedValue
          case Some(nbOccurrence) =>
            if (nbOccurrence == 1) duplicatedElements.remove(removedValue)
            else duplicatedElements(removedValue) -= 1
        }
        true
      case SeqUpdateDefineCheckpoint(prev, _)                       => digestUpdates(prev)
      case SeqUpdateRollBackToTopCheckpoint(_, howToRollBack, _, _) => digestUpdates(howToRollBack)
      case SeqUpdateReleaseTopCheckpoint(prev, _)                   => digestUpdates(prev)
      case SeqUpdateLastNotified(_)                                 => true
      case SeqUpdateAssign(_)                                       => false
    }
  }

  /** Uses an IntSequence to define the new content (overriding the current output value).
    *
    * This is NOT incremental. Generally used at start or when [[Content.digestUpdates]] returns
    * false.
    * @param value
    *   The considered value to recompute this Invariant output.
    */
  private def updateFromScratch(value: IntSequence): Unit = {
    output := Set.empty
    duplicatedElements = mutable.HashMap.empty
    val contentWithOccurrences: List[(Int, Int)] =
      value.unorderedContentNoDuplicateWithNBOccurrences
    contentWithOccurrences foreach (valueAndOccurrence => {
      val (value, occ) = valueAndOccurrence
      if (occ > 1)
        duplicatedElements.addOne((value, occ - 1)) // stocking only the duplicated value
      output :+= value
    })
  }

  /** Allows to check and debug propagation elements.
    *
    * This method can be called after the propagation according to the debug level of the
    * propagation structure (see [[oscar.cbls.core.propagation.PropagationStructure]]). It can be
    * used to check if the invariant worked properly by, for example, recomputing the value from
    * scratch.
    */
  override def checkInternals(): Unit = {
    val contentWithOccurrences: List[(Int, Int)] =
      input.value.unorderedContentNoDuplicateWithNBOccurrences
    contentWithOccurrences foreach (elementAndOccurrence => {
      val (element, occ) = elementAndOccurrence
      if (occ > 1)
        require(
          duplicatedElements(element) == occ - 1,
          s"The number of occurrence of element $element does not correspond to reality. " +
            s"Should be $occ got ${duplicatedElements(element) + 1}"
        )
      require(
        output.value().contains(element),
        s"Output does not contains $element.\n" +
          s"Input value no duplicates : ${input.value.unorderedContentNoDuplicate}\n" +
          s"Output : ${output.value()}"
      )
    })
  }
}
