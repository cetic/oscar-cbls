package oscar.xcsp3.competition.solvers

import oscar.algo.search.{DFSearch, SearchStatistics}
import oscar.cp._
import oscar.xcsp3.competition.solvers.resources.COSSolver

object ConfOrderSolverWithRestarts extends COSSolver {

  override def startSearch(solver: CPSolver, stopCondition: DFSearch => Boolean, decsionVars: Array[CPIntVar], auxiliaryVars: Array[CPIntVar], maximizeObjective: Option[Boolean]): SearchStatistics = {

    val initialTimeLimit : Long = 1000000000L
    var restartTimeLimit : Long = initialTimeLimit
    var globalStop = false
    var lastSearchStats : SearchStatistics = null
    var initialTime = System.nanoTime()
    val r = 1.3
    var completed = false

    val stopRestartCondition : DFSearch => Boolean = search => {
      val initialStop = stopCondition(search)
      if(initialStop)
       globalStop = true

      val stopRestart = restartTimeLimit < System.nanoTime() - initialTime
      initialStop || stopRestart || completed
    }

    val rand = new scala.util.Random(42)

    solver.search(
      if(auxiliaryVars.isEmpty) conflictOrderingSearch(
        decsionVars,
        i => decsionVars(i).size,
//        learnValueHeuristic(decsionVars, if(maximizeObjective.isDefined) if(maximizeObjective.get) decsionVars(_).max else decsionVars(_).min else decsionVars(_).max)
        learnValueHeuristic(decsionVars, if(maximizeObjective.isDefined) if(maximizeObjective.get) decsionVars(_).randomValue(rand) else decsionVars(_).randomValue(rand) else decsionVars(_).randomValue(rand))
      )
      else conflictOrderingSearch(
        decsionVars,
        i => decsionVars(i).size,
//        learnValueHeuristic(decsionVars, if(maximizeObjective.isDefined) if(maximizeObjective.get) decsionVars(_).max else decsionVars(_).min else decsionVars(_).max)
        learnValueHeuristic(decsionVars, if(maximizeObjective.isDefined) if(maximizeObjective.get) decsionVars(_).randomValue(rand) else decsionVars(_).randomValue(rand) else decsionVars(_).randomValue(rand))
      ) ++ conflictOrderingSearch(
        auxiliaryVars,
        i => auxiliaryVars(i).size,
//        learnValueHeuristic(auxiliaryVars, if(maximizeObjective.isDefined) if(maximizeObjective.get) auxiliaryVars(_).max else auxiliaryVars(_).min else auxiliaryVars(_).max)
        learnValueHeuristic(auxiliaryVars, if(maximizeObjective.isDefined) if(maximizeObjective.get) auxiliaryVars(_).randomValue(rand) else auxiliaryVars(_).randomValue(rand) else auxiliaryVars(_).randomValue(rand))
      )
    )

    //find first solution
    lastSearchStats = solver.start(timeLimit = 10, nSols = 1)
    if(lastSearchStats.completed) {
      completed = true
    }

    //restarts
    while(!globalStop) {
//      println(s"restartTimeLimit : $restartTimeLimit")
      initialTime = System.nanoTime()
      lastSearchStats = solver.startSubjectTo(stopRestartCondition, Int.MaxValue, null){}
      if(lastSearchStats.completed) {
        completed = true
      }
      if(lastSearchStats.nSols > 0) {
        restartTimeLimit = initialTimeLimit
      }
      restartTimeLimit = math.ceil(restartTimeLimit * r).toLong
    }

    lastSearchStats
  }
}
