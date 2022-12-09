/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cbls.core.search

trait AcceptanceCriterion {
  def apply(oldValue: Long, newValue: Long): Boolean
}

object AcceptAll extends AcceptanceCriterion {
  override def apply(oldValue: Long, newValue: Long): Boolean = true
}

object StrictImprovement extends AcceptanceCriterion {
  override def apply(oldValue: Long, newValue: Long): Boolean = newValue < oldValue
}

case class StrictlyBetterThan(value: Long) extends AcceptanceCriterion {
  override def apply(oldValue: Long, newValue: Long): Boolean = newValue < value
}

case class DifferentOf(value: Long) extends AcceptanceCriterion {
  override def apply(oldValue: Long, newValue: Long): Boolean = newValue != value
}

case class OverrideCriterion(overridingCriterion: AcceptanceCriterion) extends AcceptanceCriterion {
  override def apply(oldValue: Long, newValue: Long): Boolean = {
    (oldValue == Long.MaxValue || newValue != Long.MaxValue) && overridingCriterion(oldValue, newValue)
  }
}
