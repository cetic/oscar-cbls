package examples.oscar.cbls.flowShop

import oscar.cbls._
import oscar.cbls.core.computation.{CBLSIntConst, CBLSIntVar, IntValue}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.BestSlopeFirst
import oscar.cbls.lib.search.neighborhoods.{SwapsNeighborhood, WideningFlipNeighborhood}
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

  println(s"flowShop(jobs:$nbJobs,machines:$nbMachines)")

  val jobSequence:Array[CBLSIntVar] = Array.tabulate(nbJobs)(p => CBLSIntVar(p,jobs,s"jobStartingAtPosition$p"))

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

  val search = BestSlopeFirst(
    List(
      shiftNeighborhood(jobSequence),
      rollNeighborhood(jobSequence),
      SwapsNeighborhood(jobSequence),
      WideningFlipNeighborhood(jobSequence)))
    .onExhaustRestartAfter (shuffleNeighborhood(jobSequence, numberOfShuffledPositions=() => nbJobs/2),3,obj)
    .onExhaustRestartAfter (shuffleNeighborhood(jobSequence),2,obj
    )

  search.verbose = 2

  search.doAllMoves(_ => false,obj)

  println("job sequence:" + jobSequence.map(_.value).mkString(","))
  println(obj)

  def printValMatrix(a:Array[Array[IntValue]]) = a.map(_.map(_.value).mkString("\t")).mkString("\n")

  println("startingTimes:\n" + printValMatrix(machineToRoundToStartingTimes) + "\n")
  println("endingTimes:\n" + printValMatrix(machineToRoundToEndingTimes) + "\n")
}
