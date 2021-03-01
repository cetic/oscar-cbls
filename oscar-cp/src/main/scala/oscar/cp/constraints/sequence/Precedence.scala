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
import oscar.cp.core.delta._

import scala.collection.mutable

/**
 * @author Charles Thomas (cftmthomas@gmail.com)
 *
 * Ensures precedences between some elements of a sequence
 *
 * @param sequence A sequence variable
 * @param dependent True if the elements are also dependent from each other.
 */
abstract class Precedence(sequence: CPSeqVar, dependent: Boolean) extends Constraint(sequence.store) {

  override def associatedVars(): Iterable[CPVar] = Array(sequence)

}

class PrecedenceHead(sequence: CPHeadSeqVar, order: Seq[Int], dependent: Boolean = false) extends Precedence(sequence, dependent) {
  protected val position: Map[Int, Int] = order.zipWithIndex.toMap
  protected val allBefore: Map[Int, Set[Int]] = position.map { case (elem, pos) => (elem, order.take(pos).toSet) }
  protected val allAfter: Map[Int, Set[Int]] = position.map { case (elem, pos) => (elem, order.drop(pos + 1).toSet) }

  override def setup(l: CPPropagStrength): Unit = {
    if (dependent) sequence.store.add(Dependency(sequence, order.toSet)) //adding dependency constraint if needed
    propagate()
    if (isActive) {
      sequence.filterAppendableNextWhenReset(filterAppendableNext)(this)
      sequence.filterWhenDomainChangesWithDelta()(filterWithDelta)(this)
    }
  }

  override def propagate(): Unit = {
    for (appended <- sequence.allMembers) {
      if (position.contains(appended)) for (elem <- allBefore(appended).filterNot(sequence.isMember)) {
        if (dependent || sequence.isRequiredNotMember(elem)) throw Inconsistency
        else if (sequence.isPossibleOrRequired(elem)) sequence.excludes(elem)
      }
    }
  }

  def filterWithDelta(delta: DeltaSeqVar): Unit = {
    for (appended <- delta.deltaMembers()) {
      if (position.contains(appended)) for (elem <- allBefore(appended).filterNot(sequence.isMember)) {
        if (dependent || sequence.isRequiredNotMember(elem)) throw Inconsistency
        else if (sequence.isPossibleOrRequired(elem)) sequence.excludes(elem)
      }
    }
  }

  def filterAppendableNext(candidates: mutable.Set[Int]): Unit = {
    if (isActive) {
      if (dependent) {
        val firstPossible = order.find(sequence.isPossibleOrRequired)
        if (firstPossible.nonEmpty) for (elem <- allAfter(firstPossible.get)) candidates.remove(elem)
        else deactivate()
      }
      else {
        val firstRequired = order.find(sequence.isRequiredNotMember)
        if (firstRequired.nonEmpty) for (elem <- allAfter(firstRequired.get)) candidates.remove(elem)
      }
    }
  }
}

class PrecedenceInsert(sequence: CPInsertSeqVar, order: Seq[Int], dependent: Boolean = false) extends Precedence(sequence, dependent) {

  override def setup(l: CPPropagStrength): Unit = {
    if (dependent) sequence.store.add(Dependency(sequence, order.toSet)) //adding dependency constraint if needed
    propagate()
    sequence.callPropagateWhenDomainChanges(this)
  }

  override def propagate(): Unit = {
    val seqIter = sequence.allMembersIterator
    var next = -1

    //checking that elements of order present in sequence respect the precedences:
    for(e <- order.filter(sequence.isMember)){
      while(seqIter.hasNext && next != e){
        next = seqIter.next()
      }
      if(next != e) throw Inconsistency //Elem not found -> Failure
    }

    val seqPosition: Map[Int, Int] = (sequence.allMembers.zipWithIndex :+ (-1, -1)).toMap

    //Filtering possible insertions:
    var precPos = -1
    var possibleExists = false
    for(elem <- order){
      if(sequence.isMember(elem)) precPos = seqPosition(elem)
      else if(sequence.isPossibleOrRequired(elem)){
        possibleExists = true
        for(prev <- sequence.allCurrentInsertionsFor(elem).filter(seqPosition(_) < precPos))
          sequence.removeInsertion(elem, prev)
      }
    }

    if(possibleExists){
      var nextPos = sequence.length
      for(elem <- order.reverse){
        if(sequence.isMember(elem)) nextPos = seqPosition(elem)
        else if(sequence.isPossibleOrRequired(elem)){
          for(prev <- sequence.allCurrentInsertionsFor(elem).filter(seqPosition(_) >= nextPos))
            sequence.removeInsertion(elem, prev)
        }
      }
    }
    else deactivate() //Deactivating constraint if all elems from order are either members or excluded
  }
}

object Precedence {
  def apply(sequence: CPHeadSeqVar, order: Seq[Int], dependent: Boolean): Precedence =
    new PrecedenceHead(sequence, order, dependent)

  def apply(sequence: CPHeadSeqVar, order: Seq[Int]): Precedence =
    new PrecedenceHead(sequence, order)

  def apply(sequence: CPInsertSeqVar, order: Seq[Int], dependent: Boolean): Precedence = {
    if(order.length == 2) new BinaryPrecedenceInsert(sequence, order.head, order.last, dependent)
    else new PrecedenceInsert(sequence, order, dependent)
  }

  def apply(sequence: CPInsertSeqVar, order: Seq[Int]): Precedence = {
    if(order.length == 2) new BinaryPrecedenceInsert(sequence, order.head, order.last)
    else new PrecedenceInsert(sequence, order)
  }

  def apply(sequence: CPInsertSeqVar, elem1: Int, elem2: Int, dependent: Boolean): Precedence =
    new BinaryPrecedenceInsert(sequence, elem1, elem2, dependent)

  def apply(sequence: CPInsertSeqVar, elem1: Int, elem2: Int): Precedence =
    new BinaryPrecedenceInsert(sequence, elem1, elem2)
}


