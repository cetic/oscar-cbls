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

package oscar.cp.examples.scheduling

import oscar.cp._
import oscar.algo.search._
import oscar.cp.scheduling._
import oscar.cp.constraints._

/**
 *  @authors: Pierre Schaus  pschaus@gmail.com
 *  @authors: Renaud Hartert ren.hartert@gmail.com
 */
object RCPSP extends CPModel with App {

  // (duration, consumption)
  val instance = Array((5, 1), (3, 1), (9, 3), (1, 2), (2, 2), (8, 1), (3, 2), (2, 2), (2, 1), (1, 1), (1, 2))
  val durationsData = instance.map(_._1)
  val demandsData = instance.map(_._2)
  val capa = 4
  val nTasks = instance.size
  val Tasks = 0 until nTasks

  val horizon = durationsData.sum
  implicit val cp = CPSolver()

  val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durations(t))
  val demands = Array.tabulate(nTasks)(t => CPIntVar(demandsData))
  val resources = Array.tabulate(nTasks)(t => CPIntVar(0))

  val makespan = maximum(ends)

  // Cumulative
  add(maxCumulativeResource(starts, durations, ends, demands, resources, CPIntVar(capa), 0))

  minimize(makespan) search {
    binaryFirstFail(starts)
  }

  println(cp.start())
}
