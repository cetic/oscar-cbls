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
 *******************************************************************************/

package oscar.cp.core.variables

import oscar.cp.core._
import oscar.cp.core.delta._

import scala.collection.mutable

abstract class CPSeqVar extends CPVar with mutable.Iterable[Int] {

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  def isBound: Boolean

  /**
   * @return true if the element is in the domain of the variable (member U possible U excluded).
   */
  def inDomain(elem: Int): Boolean

  /**
   * @return the cardinality variable for the sequence
   */
  def cardinality: CPIntVar


  //Set domain:

  /**
   * @return true if the element is possible (can eventually be member)
   */
  def isPossibleOrRequired(elem: Int): Boolean

  /**
   * @return true if the element is possible (is or can eventually be member
   */
  def isPossibleOrMember(elem: Int): Boolean

  /**
   * @return all the elements that can eventually be member of the sequence
   */
  def allPossibleOrRequired: Iterable[Int]

  /**
   * @return all the elements that are or can eventually be member of the sequence
   */
  def allPossibleOrMember: Iterable[Int]

  /**
   * @return true if the element is required
   */
  def isRequiredNotMember(elem: Int): Boolean

  /**
   * @return true if the element is required or member
   */
  def isRequired(elem: Int): Boolean

  /**
   * @return all the required or members elements in the sequence
   */
  def allRequired: Iterable[Int]

  /**
   * @return all the required elements in the sequence
   */
  def allRequiredNotMember: Iterable[Int]

  /**
   * Sets element elem to required
   */
  def requires(elem: Int): Unit

  /**
   * @return true if the element has been excluded from the sequence
   */
  def isExcluded(elem: Int): Boolean

  /**
   * @return all the elements excluded from the sequence
   */
  def allExcluded: Iterable[Int]

  /**
   * excludes elem from the sequence
   */
  def excludes(elem: Int): Unit

  /**
   * excludes all remaining possible elements from domain
   */
  def excludeAllPossibles(): Unit


  //Sequence domain:

  /**
   * @return the current length of the sequence
   */
  def length: Int

  /**
   * @return true if the element is part of the sequence
   */
  def isMember(elem: Int): Boolean

  /**
   * @return the current sequence of member elements
   */
  def allMembers: Seq[Int]


  //Delta and propagation methods:

  /**
    * Level 2 registration: ask that the propagate() method of the constraint c is called whenever the domain of the variable changes
    * @see oscar.cp.core.Constraint#propagate()
    */
  def callPropagateWhenDomainChanges(c: Constraint): Unit

  def callPropagateOnChangesWithDelta(c: Constraint): DeltaSeqVar

  def delta(constraint: Constraint, id: Int = 0): DeltaSeqVar

  def changed(seqVar: DeltaSeqVar): Boolean

  def membersChanged(seqVar: DeltaSeqVar): Boolean = seqVar.oldSizeMember != length

  def deltaMembersSize(seqVar: DeltaSeqVar): Int = length - seqVar.oldSizeMember

  def deltaMembers(seqVar: DeltaSeqVar): Iterator[Int]

  def filterWhenDomainChangesWithDelta(idempotent: Boolean = false, priority: Int = CPStore.MaxPriorityL2 - 2)(filter: DeltaSeqVar => Unit)(implicit constraint: Constraint): DeltaSeqVar


  override def foreach[U](f: Int => U): Unit = allMembers.foreach(f)
  override def iterator(): Iterator[Int] = allMembers.iterator

  override def toString(): String = "sequence(mem:" + allMembers.mkString(" -> ") + "; req:[" + allRequiredNotMember.mkString(", ") + "]; poss:[" + allPossibleOrRequired.mkString(", ") + "])"
}
