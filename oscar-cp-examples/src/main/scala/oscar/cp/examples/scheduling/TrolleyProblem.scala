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

import oscar.cp.constraints.UnaryResourceVilim
import oscar.cp._
import oscar.cp.scheduling.visual.{VisualStateResource, VisualGanttChart}
import oscar.visual.{VisualUtil, VisualFrame}

/**
 * Created on 08/12/14.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 *
 * This object models and solves the trolley model available at
 * P. van Hentenryck, L. Michel, P. Laborie, W. Nuijten, and J. Rogerie. Combinatorial optimization
 * in OPL Studio. In Proc. EPIA 1999, pages 1–15, 1999.
 */

object TrolleyProblem extends CPModel with App {

  // ----------------    DATA     ----------------------
  
  val tasks = Array("loadA", "unload1", "process1", "load1", "unload2", "process2", "load2", "unloadS")
  val process1Index = tasks.indexOf("process1")
  val process2Index = tasks.indexOf("process2")
  val stateActivityIndexes = (0 until tasks.length).filter(i => i != process1Index && i != process2Index)
  val locations = Array("m1", "m2", "m3", "areaA", "areaS")
  val jobs = Array("j1", "j2", "j3", "j4", "j5", "j6")
  val job = Array(
    ("m1", 80, "m2", 60),
    ("m2", 120, "m3", 80),
    ("m2", 80, "m1", 60),
    ("m1", 160, "m3", 100),
    ("m3", 180, "m2", 80),
    ("m2", 140, "m3", 60))

  val loadDuration = 20
  val horizon = 2000
  val tt = Array(
    Array(0, 50, 60, 50, 90),
    Array(50, 0, 60, 90, 50),
    Array(60, 60, 0, 80, 80),
    Array(50, 90, 80, 0, 120),
    Array(90, 50, 80, 120, 0))

  val location = Array.tabulate(jobs.length)(j =>
    Array("areaA",
      job(j)._1,
      job(j)._1,
      job(j)._1,
      job(j)._3,
      job(j)._3,
      job(j)._3,
      "areaS"
    ))

  val locationFlat = location.flatten

  val stateNeeded = Array.tabulate(jobs.length)(j => Array.tabulate(tasks.length)(t => {
    if (t != process1Index && t != process2Index) {
      locations.indexOf(location(j)(t))
    }
    else {
      -1
    }
  }))

  val duration = Array.tabulate(jobs.length)(j =>
    Array(loadDuration,
      loadDuration,
      job(j)._2,
      loadDuration,
      loadDuration,
      job(j)._4,
      loadDuration,
      loadDuration
    ))

  val ttMap = Array.tabulate(locations.length)(l1 =>
    Array.tabulate(locations.length)(l2 => ((locations(l1), locations(l2)), tt(l1)(l2)))).flatten.toMap[(String, String), Int]

  val ttMatrix = Array.tabulate(jobs.length * tasks.length)(t1 =>
    Array.tabulate(jobs.length * tasks.length)(t2 =>
      ttMap((locationFlat(t1), locationFlat(t2)))
    ))


  // ----------------    MODEL     ----------------------
    
  val durationVars = Array.tabulate(jobs.length)(j => Array.tabulate(tasks.length)(t => CPIntVar(duration(j)(t))))
  val durationVarsFlat = durationVars.flatten
  val stateDurVars = Array.tabulate(jobs.length)(j => stateActivityIndexes.map(t => durationVars(j)(t))).flatten
  val startVars = Array.tabulate(jobs.length)(j => Array.tabulate(tasks.length)(t => CPIntVar(0 until horizon - durationVars(j)(t).min)))
  val startVarsFlat = startVars.flatten
  val stateStartVars = Array.tabulate(jobs.length)(j => stateActivityIndexes.map(t => startVars(j)(t))).flatten
  val endVars = Array.tabulate(jobs.length)(j => Array.tabulate(tasks.length)(t => startVars(j)(t)+duration(j)(t)))
  val endVarsFlat = endVars.flatten
  val stateEndVars = Array.tabulate(jobs.length)(j => stateActivityIndexes.map(t => endVars(j)(t))).flatten
  val makespan = maximum(endVarsFlat)

  //Precedence constraints
  for (j <- 0 until jobs.length) {
    for (t <- 0 until tasks.length - 1) {
      add(endVars(j)(t) <= startVars(j)(t + 1))
    }
  }


  //UnaryResources
  for (machine <- Seq("m1", "m2", "m3")) {
    val activities =
      (for {
        j <- 0 until jobs.size;
        t <- 0 until tasks.length;
        if (job(j)._1 == machine && t == process1Index) ||
          (job(j)._3 == machine && t == process2Index)
      } yield {
        (startVars(j)(t), durationVars(j)(t), endVars(j)(t))
      }).toArray
     add(unaryResource(activities.map(_._1), activities.map(_._2), activities.map(_._3)))
  }

  //State resource constraints
  stateResource(stateStartVars, stateDurVars, stateEndVars, stateNeeded.flatten.filter(s => s >= 0), tt)

  // ----------------    VISUALISATION     ----------------------

  val frame = new VisualFrame("Trolley Problem", 2, 1)
  val stateColors = VisualUtil.getRandomColors(locations.length + 1, pastel=true)
  val gantt = new VisualGanttChart(startVarsFlat, durationVarsFlat, endVarsFlat, i => i / tasks.length, colors = i => {
    if (i % tasks.length == process1Index || i % tasks.length == process2Index) {
      stateColors(locations.length)
    }
    else {
      stateColors(locations.indexOf(locationFlat(i)))
    }
  })

  val states = new VisualStateResource(startVarsFlat, durationVarsFlat, endVarsFlat, stateNeeded.flatten, stateNeeded.flatten.map(e => e >= 0), colors = i => stateColors(i))

  onSolution {
    gantt.update(1, 20)
    states.update(1, 20)
  }

  frame.createFrame("Gantt Chart").add(gantt)
  frame.createFrame("Trolley Location").add(states)
  frame.pack()

  // ----------------    SEARCH     ----------------------


  minimize(makespan) search {
    setTimes(startVarsFlat, durationVarsFlat, endVarsFlat,i => -endVarsFlat(i).min)
  }

  val stat = start()
  println(stat)

}
