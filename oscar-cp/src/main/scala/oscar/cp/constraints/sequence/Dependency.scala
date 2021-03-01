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

import oscar.cp.core.variables._
import oscar.cp.core._
import oscar.cp.core.delta._

/**
 * @author Charles Thomas (cftmthomas@gmail.com)
 *
 * Enforces dependencies between elements of a sequence (a <=> b)
 */
class Dependency(sequence: CPSeqVar, dependents: Set[Int]) extends Constraint(sequence.store) {
  for (elem <- dependents) {
    assert(sequence.inDomain(elem))
  }

  override def associatedVars(): Iterable[CPVar] = Array(sequence)

  override def setup(l: CPPropagStrength): Unit = {
    propagate()
    if (isActive) sequence.filterWhenDomainChangesWithDelta()(filterWithDelta)(this) //Setting up filtering
  }

  override def propagate(): Unit = {
    var require = false
    var exclude = false

    val iter = dependents.iterator
    while (iter.hasNext && !require && !exclude) {
      val elem = iter.next()
      if (sequence.isRequired(elem)) require = true
      else if (sequence.isExcluded(elem)) exclude = true
    }

    if (require) setElemsRequired()
    if (exclude) setElemsExcluded()
  }

  def filterWithDelta(delta: DeltaSeqVar): Unit = {
    var require = false
    var exclude = false

    val newRequiredIter = delta.deltaRequired()
    while (newRequiredIter.hasNext && !require) {
      val elem = newRequiredIter.next()
      if (dependents.contains(elem)) require = true
    }

    val newExcludedIter = delta.deltaPossible()
    while (newExcludedIter.hasNext && !require && !exclude) {
      val elem = newExcludedIter.next()
      if (dependents.contains(elem)) exclude = true
    }

    if (require) setElemsRequired()
    if (exclude) setElemsExcluded()
  }

  def setElemsRequired(): Unit = {
    dependents.foreach(sequence.requires)
    deactivate()
  }

  def setElemsExcluded(): Unit = {
    dependents.foreach(sequence.excludes)
    deactivate()
  }
}

object Dependency {
  def apply(sequence: CPSeqVar, dependents: Set[Int]): Dependency = new Dependency(sequence, dependents)
}
