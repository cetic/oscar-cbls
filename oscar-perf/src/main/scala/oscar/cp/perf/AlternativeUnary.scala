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

package oscar.cp.perf

import oscar.cp._

import scala.util.Random

/**
 * @author Steven Gay steven.gay@uclouvain.be
 * @author Cyrille Dejemeppe cyrille.dejemeppe@uclouvain.be
 */

object AlternativeUnary extends CPModel with App {

  val rand = new Random(0)
  val nTasks = 25
  val durationsData = Array.fill(nTasks)(rand.nextInt(10))
  val horizon = durationsData.sum

  val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durations(t))
  val resources = Array.fill(nTasks)(CPIntVar(0, 1))
  val one = CPIntVar(1)
  val demands = Array.fill(nTasks)(one)

  val makespan = maximum(ends)
  val nResources = 2
  
  // Resources
  for (r <- 0 until nResources) {
    add(unaryResource(starts, durations, ends, resources, r), Medium)
  }
  
  add(maxCumulativeResource(starts, durations, ends, demands, CPIntVar(2)), Medium)

  minimize(makespan) search {
    val schVars = starts ++ resources
    splitLastConflict(schVars, schVars(_).size)
  }

  start()
}