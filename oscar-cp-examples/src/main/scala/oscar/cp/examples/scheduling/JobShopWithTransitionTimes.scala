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

import oscar.algo.branchings.ConflictOrderingSearch
import oscar.cp._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPIntVar

import scala.io.Source

/**
 * Created on 25/07/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 *
 * This object models and solves a jobshop in which there are
 * transition times between activities. A transition time between
 * two activities i and j is the minimum amount of time needed
 * between the end of i and the start of j.
 */

object JobShopWithTransitionTimes extends CPModel with App {

  // ----------------    DATA     ----------------------
  val instanceURI = "data/jobshoptransitiontimes/taillard-jobshop-20_15-1_300_50_150-1"
  val lines = Source.fromFile(instanceURI).getLines.toArray
  val nbProblems = lines(0).replaceAll(" ","").toInt
  val nJobs = lines(1).split(" ")(0) toInt
  val nJobsWithSourceJob = nJobs + 1 //there is one fake activity in a fake job
  val nMachines = lines(2).replaceAll(" ","") toInt //same number of machines for each job
  val nActivities = nJobs * nMachines
  val nActivityWithSourceActivity = nActivities + 1
  val activities = for(j <- 2 + nJobsWithSourceJob until (2+nJobsWithSourceJob+nJobs*nMachines) ; activityLine = lines(j).split(" ").filterNot(_ == ""))
    yield (activityLine(0).toInt, activityLine(1).toInt, activityLine(2).toInt, activityLine(3).toInt)

  val transitionTimesWithSourceActivity=Array.fill(nActivityWithSourceActivity)(Array.fill(nActivityWithSourceActivity)(-1))
  for(i <- 0 until nActivityWithSourceActivity; j <- 0 until nActivityWithSourceActivity)
    transitionTimesWithSourceActivity(i)(j)=lines(2+nJobsWithSourceJob+nJobs*nMachines+i*nActivityWithSourceActivity+j) toInt

  //release dates are modelled as a transition from the source activity to the activity
  val startMins = transitionTimesWithSourceActivity(0).drop(1)

  val transitionTimes = transitionTimesWithSourceActivity map (_.drop(1)) drop(1)

  val machines = activities map (_._1 - 1) //remove 1 to get the usual indexing
  val durations = activities map (_._2)
  val minSeparationTimes = activities map (_._3)
  val maxSeparationTimes = activities map (_._4)

  // ----------------    MODEL     ----------------------
  val maxTransitions : Array[Int] = transitionTimes map (_ max)
  val ttMatrices = Array.tabulate(nMachines)(m => {
    val activities = (0 until nActivities).filter(machines(_) == m)
    Array.tabulate(activities.length, activities.length)((i, j) => transitionTimes(activities(i))(activities(j)))
  })
  val horizon : Int = startMins.max + durations.sum + maxTransitions.sum
  val jobs : Array[Int] = Array.tabulate(nJobs)(j => Array.fill(nMachines)(j)).flatten
  val startsVar : Array[CPIntVar] = (0 until nActivities).map(t => CPIntVar(startMins(t),horizon, "start" + t))
  val durationsVar = durations.map(CPIntVar(_))
  val endsVar : Array[CPIntVar] = Array.tabulate(nActivities)(a => startsVar(a) + durations(a))
  val makespan = maximum(endsVar)

  solver.silent = true

  // Consistency constraints
  for (t <- 0 until nActivities) {
    add(endsVar(t) === startsVar(t) + durationsVar(t))
  }

  // Precedences
  for (t <- 1 until nActivities if jobs(t - 1) == jobs(t)) {
    add(endsVar(t - 1) <= startsVar(t))
  }

  //unary with transition times
  for(m <- 0 until nMachines) {
    def filter(x: Array[CPIntVar]) = (0 until nActivities).filter(machines(_) == m).map(x(_))
    val (s,d,e) = (filter(startsVar), filter(durationsVar), filter(endsVar))
    post(unaryResource(s, d, e, ttMatrices(m)),Medium)
  }

  minimize(makespan)

  search(conflictOrderingSearch(startsVar,startsVar(_).min,startsVar(_).min))

  val startTime = System.currentTimeMillis()

  onSolution {
    println((System.currentTimeMillis() - startTime) / 1000 + "s " + makespan.value)
  }

  println(start(timeLimit=60))
}
