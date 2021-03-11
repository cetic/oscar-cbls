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
import oscar.cp.scheduling.visual.{VisualGanttChart, VisualProfile}
import oscar.visual._

import scala.io.Source

object CumulativeJobShop extends CPModel with App {

  // Parsing		
  // -------

  val lines = Source.fromFile("data/cJobShop.txt").getLines.filter(!_.isEmpty).toList
  val firstLine = lines.head.trim.split(" ")
  val nJobs = firstLine(0).toInt
  val nTasksPerJob = firstLine(1).toInt
  val nResources = firstLine(2).toInt
  val capacity = firstLine(3).toInt

  val data = for (line <- lines.tail) yield line.trim.split("[ ,\t]+").toArray
  val jobs = data.map(_(0).toInt)
  val resources = data.map(_(1).toInt)
  val durations = data.map(_(2).toInt)

  val nActivities = nJobs * nTasksPerJob
  val Activities = 0 until nActivities
  val Resources = 0 until nResources

  // Modeling	
  // --------

  val horizon = durations.sum

  // Activities & Resources
  val durationsVar = Array.tabulate(nActivities)(t => CPIntVar(durations(t)))
  val startsVar = Array.tabulate(nActivities)(t => CPIntVar(0 to horizon - durationsVar(t).min))
  val endsVar = Array.tabulate(nActivities)(t => CPIntVar(durationsVar(t).min to horizon))
  val demandsVar = Array.fill(nActivities)(CPIntVar(1))
  val resourcesVar = Array.tabulate(nActivities)(t => CPIntVar(resources(t)))
  val makespan = maximum(endsVar)

  // Consistency 
  for (t <- Activities) {
    add(endsVar(t) === startsVar(t) + durationsVar(t))
  }
  // Precedences
  for (t <- 1 to Activities.max if jobs(t - 1) == jobs(t)) {
    add(endsVar(t - 1) <= startsVar(t))
  }
  // Cumulative
  for (r <- Resources) {
    add(maxCumulativeResource(startsVar, durationsVar,endsVar, demandsVar, resourcesVar, CPIntVar(2), r))
  }

  // Visualization  
  // -------------

  val frame = new VisualFrame("Cumulative JobShop Problem", nResources + 1, 1)
  val colors = VisualUtil.getRandomColors(nResources, true)
  val gantt = new VisualGanttChart(startsVar, durationsVar, endsVar, i => jobs(i), colors = i => colors(resources(i)))
  

  val profiles = Array.tabulate(nResources)(r => new VisualProfile(startsVar, durationsVar,endsVar, demandsVar, resourcesVar,2,r,color = colors(r)))
  onSolution { 
    gantt.update(1, 20) 
    profiles.foreach(_.update(1, 20))
  }
  frame.createFrame("Gantt chart").add(gantt)
  for (r <- Resources) frame.createFrame("profile resource "+r).add(profiles(r))
  frame.pack

  // Search
  // ------

  minimize(makespan) search {
    setTimes(startsVar, durationsVar, endsVar)
  }

  val stat = start()
  println(stat)
}
