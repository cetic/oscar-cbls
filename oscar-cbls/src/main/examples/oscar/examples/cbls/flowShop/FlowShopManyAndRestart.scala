package oscar.examples.cbls.flowShop

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

import oscar.cbls._
import oscar.cbls.lib.search.combinators.BestSlopeFirst
import oscar.cbls.lib.search.neighborhoods.{WideningFlipNeighborhood, SwapsNeighborhood}
import oscar.cbls.modeling.CBLSModel

object FlowShopManyAndRestart extends CBLSModel with App {

  val machineToJobToDuration:Array[Array[Long]] =
    Array(
      Array(1,2,1,7,2,5,5,6,7),
      Array(4,5,3,1,8,3,7,8,4),
      Array(6,8,2,5,3,1,2,2,8),
      Array(4,1,7,2,5,5,6,4,5))

  val nbMachines = machineToJobToDuration.length
  val nbJobs = machineToJobToDuration(0).length
  val jobs = 0 until nbJobs
  val machines = 0 until nbMachines

  println("flowShop(jobs:" + nbJobs + ",machines:" + nbMachines + ")")

  val jobSequence:Array[CBLSIntVar] = Array.tabulate(nbJobs)(p => CBLSIntVar(p,jobs,"jobStartingAtPosition" + p))

  val machineToRoundToStartingTimes:Array[Array[IntValue]] = Array.fill(nbMachines,nbJobs)(null)
  val machineToRoundToEndingTimes:Array[Array[IntValue]] = Array.fill(nbMachines,nbJobs)(null)

  for { m <- machines
        round <- jobs } {
    val start:IntValue = (m,round) match{
      case (0,0) => CBLSIntConst(0)
      case (0,_) => machineToRoundToEndingTimes(0)(round-1)
      case (_,0) => machineToRoundToEndingTimes(m-1)(0)
      case (_,_) => max2(machineToRoundToEndingTimes(m)(round-1), machineToRoundToEndingTimes(m-1)(round))
    }
    machineToRoundToStartingTimes(m)(round) = start
    machineToRoundToEndingTimes(m)(round) = start + machineToJobToDuration(m).element(jobSequence(round))
  }

  val obj:Objective = machineToRoundToEndingTimes(nbMachines-1)(nbJobs-1)

  println("closing model")
  s.close()

  val search = (BestSlopeFirst(
    List(
      shiftNeighborhood(jobSequence),
      rollNeighborhood(jobSequence),
      SwapsNeighborhood(jobSequence),
      WideningFlipNeighborhood(jobSequence)))
    onExhaustRestartAfter (shuffleNeighborhood(jobSequence, numberOfShuffledPositions=() => nbJobs/2),3,obj)
    onExhaustRestartAfter (shuffleNeighborhood(jobSequence),2,obj))

  search.verbose = 2

  search.doAllMoves(_ => false,obj)

  println("job sequence:" + jobSequence.map(_.value).mkString(","))
  println(obj)

  def printValMatrix(a:Array[Array[IntValue]]) = a.map(_.map(_.value).mkString("\t")).mkString("\n")

  println("startingTimes:\n" + printValMatrix(machineToRoundToStartingTimes) + "\n")
  println("endingTimes:\n" + printValMatrix(machineToRoundToEndingTimes) + "\n")
}
