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

import oscar.algo.Inconsistency
import oscar.cp.core.variables._
import oscar.cp.core._

import scala.collection.mutable

/**
  * Ensures that elem x is the last in the sequence
  */
abstract class Last(sequence: CPSeqVar, elem: Int) extends Constraint(sequence.store){
  val store: CPStore = sequence.store

  override def associatedVars(): Iterable[CPVar] = Array(sequence)
}

class LastHeadSeq(sequence: CPHeadSeqVar, elem: Int) extends Last(sequence, elem) {
  assert(sequence.isPossibleOrRequired(elem) || sequence.lastAppended == elem)

  override def setup(l: CPPropagStrength): Unit = {
    sequence.requires(elem)
    propagate()
    if (isActive) {
      sequence.filterAppendableNextWhenReset(filterAppendableNext)(this)
      sequence.callPropagateOnChangesWithDelta(this)
    }
  }

  override def propagate(): Unit = {
    if (sequence.isMember(elem)) {
      if (sequence.lastAppended != elem || sequence.allRequiredNotMember.nonEmpty) throw Inconsistency
      sequence.excludeAllPossibles()
      deactivate()
    }
  }

  def filterAppendableNext(candidates: mutable.Set[Int]): Unit = {
    if (isActive && sequence.allRequiredNotMember.exists(_ != elem)) {
      candidates.remove(elem)
    }
  }
}

class LastInsertSeq(sequence: CPInsertSeqVar, elem: Int) extends Last(sequence, elem) {
  assert((sequence.isMember(elem) && sequence.last == elem) || sequence.isInsertableAfter(elem, sequence.last))

  override def setup(l: CPPropagStrength): Unit = {
    if(sequence.isPossibleOrRequired(elem)) sequence.insertAfter(elem, sequence.last)
    propagate()
    sequence.removeInsertionsAfter(elem) //Removing possibility to insert element at the end of the sequence
  }

  override def propagate(): Unit ={
    if(sequence.last != elem) throw Inconsistency
  }
}

object Last {
  def apply(sequence: CPHeadSeqVar, elem: Int): Last = new LastHeadSeq(sequence, elem)

  def apply(sequence: CPInsertSeqVar, elem: Int): Last = new LastInsertSeq(sequence, elem)
}
