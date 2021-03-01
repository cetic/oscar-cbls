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
import oscar.cp.preprocessing.ShavingUtils
import oscar.util.RandomGenerator

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


/**
 * Job-Shop Problem
 *
 *  A Job is a a sequence of n Activities that must be executed one after the
 *  others. There are n machines and each activity of the jobs require one of the
 *  n machines. The objective is to assign the starting time of each activity
 *  minimizing the total makespan and such that no two activities from two different
 *  jobs requiring the same machine overlap.
 *
 *  @author Pierre Schaus  pschaus@gmail.com
 *  @author Renaud Hartert ren.hartert@gmail.com
 *  @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
object JobShopWithLNSAndShaving extends CPModel with App {

  // Parsing    
  // -----------------------------------------------------------------------

  var lines = Source.fromFile("data/ft10.txt").getLines().toList

  val nJobs = lines.head.trim().split(" ")(0).toInt
  val nTasksPerJob = lines.head.trim().split(" ")(1).toInt
  val nResources = lines.head.trim().split(" ")(2).toInt

  val nActivities = nJobs * nTasksPerJob

  val Activities = 0 until nActivities
  val Jobs = 0 until nJobs
  val Resources = 0 until nResources

  lines = lines.drop(1)

  val jobs = Array.fill(nActivities)(0)
  val resources = Array.fill(nActivities)(0)
  val durations = Array.fill(nActivities)(0)

  for (i <- Activities) {

    val l = lines.head.trim().split("[ ,\t]+").map(_.toInt)

    jobs(i) = l(0)
    resources(i) = l(1)
    durations(i) = l(2)

    lines = lines.drop(1)
  }

  // Modeling 
  // -----------------------------------------------------------------------

  val horizon = durations.sum

  // Activities & Resources
  val durationVars = Array.tabulate(nActivities)(t => CPIntVar(durations(t), s"d$t"))
  val startVars = Array.tabulate(nActivities)(t => CPIntVar(0 to horizon - durationVars(t).min, s"s$t"))
  val endVars = Array.tabulate(nActivities)(t => CPIntVar(durationVars(t).min to horizon, s"e$t"))
  val demandVars = Array.fill(nActivities)(CPIntVar(1))

  val makespan = maximum(endVars)

  val bestSolutionStarts = Array.ofDim[Int](nActivities)
  var bestSolutionMk = Int.MaxValue

  onSolution {
    for (a <- 0 until nActivities) {
      bestSolutionStarts(a) = startVars(a).min
    }
    bestSolutionMk = makespan.min
  }

  // Constraints & Search
  // -----------------------------------------------------------------------

  // Consistency 
  for (t <- Activities) {
    add(endVars(t) === startVars(t) + durationVars(t))
  }
  // Precedences
  for (t <- 1 to Activities.max if jobs(t - 1) == jobs(t)) {
    add(endVars(t - 1) <= startVars(t))
  }
  // Cumulative
  val rankBranchings = for (r <- Resources) yield {
    def filter(x: Array[CPIntVar]) = Activities.filter(resources(_) == r).map(x(_))
    val (s,d,e) = (filter(startVars), filter(durationVars), filter(endVars))
    add(unaryResource(s,d,e))
    rank(s,d,e)
  }
  
  minimize(makespan) 

  val rankBranching = rankBranchings.reduce{_++_}
    
  solver.search {
    conflictOrderingSearch(startVars, startVars(_).min, startVars(_).min)
  }
  solver.silent = true

  start(nSols = 1)

  val constraintBuffer = ArrayBuffer[Constraint]()

  val maxFails = 2000
  val relaxProba = 90
  val nRelaxations = 100000
  for (r <- 1 to nRelaxations) {
    constraintBuffer.clear()
    val stats = startSubjectTo(failureLimit = maxFails) {
      for (a <- 0 until nActivities) {
        if (RandomGenerator.nextInt(100) > relaxProba) {
          constraintBuffer += startVars(a) === bestSolutionStarts(a)
        }
      }
      add(constraintBuffer)
    }
    // Shaving
    if (stats.nSols > 0) {
      solver.propagate()
      val startDomSizeBefore = startVars.map(sv => sv.size)
      ShavingUtils.boundsShaving(solver, startVars)
      val startDomSizeAfter = startVars.map(sv => sv.size)
      println(s"Makespan: $bestSolutionMk")
      println(s"Shaving on min and max has removed ${startVars.indices.foldLeft(0)((acc, i) => acc + (startDomSizeBefore(i) - startDomSizeAfter(i)))} values from the domains of ${startVars.length} variables.")
    }
  }
}

