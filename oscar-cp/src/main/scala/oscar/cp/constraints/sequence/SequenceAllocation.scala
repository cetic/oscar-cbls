/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.cp.constraints.sequence

import oscar.cp.core.delta._
import oscar.cp.core.variables._
import oscar.cp.core._

/**
  * Ensures that elements are present in a single sequence among the alternatives.
  */
class SequenceAllocation(
                           sequences: Array[CPSeqVar], //Sequence variables
                           elements: Array[Int], //Elements to dispatch between seqeunces
                           resourceVars: Array[CPIntVar] //Resource variables: Indicates which sequence contains each element
                         ) extends Constraint(sequences.head.store) {

  assert(elements.length == resourceVars.length)

  val resource: Map[Int, CPIntVar] = elements.zipWithIndex.map { case (elem, index) => (elem, resourceVars(index)) }.toMap

  private def isCovered(seq: Int) = seq >= 0 && seq < sequences.length

  override def associatedVars(): Iterable[CPVar] = sequences ++ resourceVars

  override def setup(l: CPPropagStrength): Unit = {
    //Setting up filtering:
    for (seq <- sequences.indices) sequences(seq).filterWhenDomainChangesWithDelta()(filterWithSeqDelta(seq))
    for (elem <- elements) resource(elem).filterWhenDomainChangesWithDelta()(filterWithResourceDelta(elem))

    propagate()
  }

  override def propagate(): Unit = {
    for (elem <- elements) {
      val possibleSeqs = resource(elem)
      for (seq <- sequences.indices) {
        val sequenceVar = sequences(seq)
        if (sequenceVar.isRequired(elem) && !possibleSeqs.isBoundTo(seq)) possibleSeqs.assign(seq) //Assigning resource if elem required
        if (!possibleSeqs.hasValue(seq) && sequenceVar.isPossibleOrMember(elem)) sequenceVar.excludes(elem) //Excluding sequence if resource not available
        if (!sequenceVar.isPossibleOrMember(elem) && possibleSeqs.hasValue(seq)) possibleSeqs.removeValue(seq) //Excluding resource if elem not possible
      }
      if (possibleSeqs.isBound && possibleSeqs.value >= 0 && possibleSeqs.value < sequences.length && !sequences(possibleSeqs.value).isRequired(elem))
        sequences(possibleSeqs.value).requires(elem) //Requiring element if resource set
    }
  }

  def filterWithSeqDelta(seq: Int)(delta: DeltaSeqVar): Unit = {
    for (elem <- delta.deltaMembers() ++ delta.deltaRequired()) resource(elem).assign(seq)
    for (elem <- delta.deltaPossible()) resource(elem).removeValue(seq)
  }

  def filterWithResourceDelta(elem: Int)(delta: DeltaIntVar): Boolean = {
    if (resource(elem).isBound && isCovered(resource(elem).value)) sequences(resource(elem).value).requires(elem)
    for (seq <- delta.values) if (isCovered(seq)) sequences(seq).excludes(elem)
    resource(elem).isBound
  }
}

object SequenceAllocation {
  def apply(sequences: Array[CPSeqVar], elements: Array[Int], resources: Array[CPIntVar]) = new SequenceAllocation(sequences, elements, resources)
}
