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

object AlternativeTask extends CPModel with App {

  val durationsData = Array(2, 2, 3, 3, 3, 4, 4, 5, 6, 7)
  val demandsData1 = Array(1, 2, 1, 3, 2, 1, 1, 2, 3, 2)
  val demandsData2 = Array(2, 3, 2, 1, 2, 3, 2, 3, 1, 3)
  val horizon = durationsData.sum
  val capaMax = 3

  val nTasks = durationsData.size
  val Tasks = 0 until nTasks

  val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => CPIntVar(durations(t).min to horizon))
  val demands = Array.tabulate(nTasks)(t => CPIntVar(Set(demandsData1(t), demandsData2(t))))
  val resources = Array.fill(nTasks)(CPIntVar(1 to 2))

  val makespan = maximum(ends)

  // Consistency 
  for (t <- Tasks) {
    add(ends(t) === starts(t) + durations(t))
  }
  
  // Alternative demands
  for (t <- Tasks) {
    add((resources(t) ?=== 1) ==> (demands(t) ?=== demandsData1(t)))
    add((resources(t) ?=== 2) ==> (demands(t) ?=== demandsData2(t)))
  }
  
  // Cumulative
  add(maxCumulativeResource(starts, durations, ends, demands, resources, CPIntVar(capaMax), 1))
  add(maxCumulativeResource(starts, durations, ends, demands, resources, CPIntVar(capaMax), 2))

  minimize(makespan) search {
    binaryFirstFail(resources) ++ binaryFirstFail(starts)
  }

  onSolution {
    println("Makespan of " + makespan.value)
    for (t <- Tasks) {
      print("Task " + t)
      print(" of duration " + durations(t).value)
      print(" and demand " + demands(t).value)
      print(" is executed on resource " + resources(t).value)
      println(" at time " + starts(t).value)
    }
    println()
  }

  println(start())
}
