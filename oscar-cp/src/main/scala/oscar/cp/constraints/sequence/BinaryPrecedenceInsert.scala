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

import oscar.algo._
import oscar.cp.core._
import oscar.cp.core.variables._

class BinaryPrecedenceInsert(sequence: CPInsertSeqVar, elem1: Int, elem2: Int, dependent: Boolean = false) extends Precedence(sequence, dependent) {

  override def setup(l: CPPropagStrength): Unit = {
    if (dependent) sequence.store.add(Dependency(sequence, Set(elem1, elem2))) //adding dependency constraint if needed
    sequence.removeInsertion(elem1, elem2)
    propagate()
    sequence.callPropagateWhenDomainChanges(this)
  }

  override def propagate(): Unit = {
    val seqPosition: Map[Int, Int] = (sequence.allMembers.zipWithIndex :+ (-1, -1)).toMap

    if(sequence.isExcluded(elem1) || sequence.isExcluded(elem2)) {
      deactivate()
    } else if(sequence.isMember(elem1)){
      if(sequence.isMember(elem2)) {
        if (seqPosition(elem1) >= seqPosition(elem2)) Inconsistency
        deactivate()
      } else {
        val pos1 = seqPosition(elem1)
        for(pred <- sequence.allCurrentInsertionsFor(elem2).filter(seqPosition(_) < pos1))
          sequence.removeInsertion(elem2, pred)
      }
    } else if(sequence.isMember(elem2)){
      val pos2 = seqPosition(elem2)
      for(pred <- sequence.allCurrentInsertionsFor(elem1).filter(seqPosition(_) >= pos2))
        sequence.removeInsertion(elem1, pred)
    } else {
      val min1 = sequence.allCurrentInsertionsFor(elem1).map(seqPosition).min
      val max2 = sequence.allCurrentInsertionsFor(elem2).map(seqPosition).max

      for(pred <- sequence.allCurrentInsertionsFor(elem1).filter(seqPosition(_) > max2))
        sequence.removeInsertion(elem1, pred)
      for(pred <- sequence.allCurrentInsertionsFor(elem2).filter(seqPosition(_) < min1))
        sequence.removeInsertion(elem2, pred)
    }
  }
}


