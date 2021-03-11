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

/**
  * Ensures that elem is the first in the sequence
  */
abstract class First(sequence: CPSeqVar, elem: Int) extends Constraint(sequence.store) {
  val store: CPStore = sequence.store

  override def associatedVars(): Iterable[CPVar] = Array(sequence)
}

class FirstHeadSeq(sequence: CPHeadSeqVar, elem: Int) extends First(sequence, elem) {
  assert((sequence.isMember(elem) && sequence.getElemAt(0) == elem) || sequence.isEmpty && sequence.isAppendableNext(elem))

  override def setup(l: CPPropagStrength): Unit = {
    sequence.append(elem)
    this.deactivate()
  }
}

class FirstInsertSeq(sequence: CPInsertSeqVar, elem: Int) extends First(sequence, elem) {
  assert((sequence.isMember(elem) && sequence.head == elem) || sequence.isInsertableAfter(elem, -1))

  override def setup(l: CPPropagStrength): Unit = {
    if(sequence.isPossibleOrRequired(elem)) sequence.insertAfter(elem, -1)
    propagate()
    sequence.removeInsertionsAfter(-1) //Removing possibility to insert element in front of the sequence
  }

  override def propagate(): Unit ={
    if(sequence.head != elem) throw Inconsistency
  }
}

object First {
  def apply(sequence: CPHeadSeqVar, elem: Int): First = new FirstHeadSeq(sequence, elem)

  def apply(sequence: CPInsertSeqVar, elem: Int): First = new FirstInsertSeq(sequence, elem)
}
