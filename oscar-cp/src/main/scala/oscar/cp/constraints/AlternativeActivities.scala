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

package oscar.cp.constraints

import oscar.cp.core._
import oscar.cp.core.variables._

/**
 * @author Charles Thomas (cftmthomas@gmail.com)
 *
 * Alternative between activities:
 * start/duration/end == optional{Starts/Durations/Ends}(resource)
 */
class AlternativeActivities(
                   start: CPIntVar,
                   duration: CPIntVar,
                   end: CPIntVar,
                   optionalStarts: Array[CPIntVar],
                   optionalDurations: Array[CPIntVar],
                   optionalEnds: Array[CPIntVar],
                   resource: CPIntVar
                 ) extends Constraint(start.store){
  assert(optionalStarts.length == optionalEnds.length)
  assert(optionalStarts.length == optionalDurations.length)

  val store: CPStore = start.store

  override def associatedVars(): Iterable[CPVar] = Array(start, duration, end, resource) ++ optionalStarts ++ optionalDurations ++ optionalEnds

  override def setup(l: CPPropagStrength): Unit = {
    store.post(new ElementVar(optionalStarts, resource, start))
    store.post(new ElementVar(optionalDurations, resource, duration))
    store.post(new ElementVar(optionalEnds, resource, end))
    this.deactivate()
  }
}

object AlternativeActivities{
  def apply(
             start: CPIntVar,
             duration: CPIntVar,
             end: CPIntVar,
             optionalStarts: Array[CPIntVar],
             optionalDurations: Array[CPIntVar],
             optionalEnds: Array[CPIntVar],
             resource: CPIntVar
           ): AlternativeActivities = new AlternativeActivities(start, duration, end, optionalStarts, optionalDurations, optionalEnds, resource)
}


