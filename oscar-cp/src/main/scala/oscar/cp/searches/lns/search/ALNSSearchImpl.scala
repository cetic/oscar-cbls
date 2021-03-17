package oscar.cp.searches.lns.search

import oscar.algo.Inconsistency
import oscar.algo.search.DFSearch
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators._
import oscar.cp.searches.lns.selection.{AdaptiveStore, Metrics}

import scala.collection.mutable
import scala.util.Random

/**
  * Adaptive lage neighbourhood search implementation.
  */
class ALNSSearchImpl(solver: CPSolver, decisionVars: Array[CPIntVar], auxiliaryVars: Array[CPIntVar], config: ALNSConfig) extends ALNSSearch(solver, decisionVars, config){
  var startTime: Long = System.nanoTime()
  var iter: Long = 1L
  val history: mutable.ArrayBuffer[(Long, String, Double)] = mutable.ArrayBuffer()

  def this(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig){
    this(solver, vars, Array[CPIntVar](), config)
  }

  val vars: Array[CPIntVar] = decisionVars ++ auxiliaryVars

  //Stop conditions:
  val endTime: Long = if(config.timeout > 0) System.nanoTime() + config.timeout else Long.MaxValue //Maximal allocated time
  var iterTimeout: Long = if(config.timeout > 0) config.timeout / 120 else Long.MaxValue //The iteration allocated time
  var endIter: Long = Long.MaxValue //Maximal allocated time for the current iteration
  var optimumFound = false //True if the whole search space has been explored (csp and cop) or the optimum has been found (cop)
  var nSols = 0
  var nFailures = 0
  var stopSearch = false
  val opDeactivation: Boolean = config.metaParameters.getOrElse(Symbol("opDeactivation"), false).asInstanceOf[Boolean]

  val stopCondition: (DFSearch) => Boolean = (s: DFSearch) => {
    var stop = optimumFound
    stop |= nSols != 0 && s.nSolutions >= nSols
    stop |= nFailures != 0 && s.nBacktracks >= nFailures
    stop |= System.nanoTime() >= endTime
    stop |= System.nanoTime() >= endIter
    stop
  }

  //Solutions management:
  val solsFound = new mutable.ListBuffer[CPIntSol]()
  var currentSol: Option[CPIntSol] = None  //Current solution
  var bestSol: Option[CPIntSol] = currentSol //Best solution so far
  var previousBest: Option[CPIntSol] = None //Previous best solution

  val maximizeObjective: Option[Boolean] = if(solver.objective.objs.nonEmpty) Some(solver.objective.objs.head.isMax) else None
  if(!solver.silent) println("Objective type: " + (if(maximizeObjective.isDefined) if(maximizeObjective.get) "max" else "min" else "none"))

  solver.onSolution(onSolution())

  //Learning phase:
  var learning = false //Currently in learning phase
  var learnRatio = 0.3 //The ratio of remaining time that a learning phase will have

  //Stagnation:
  var stagnation = 0
  val stagnationThreshold: Int = config.metaParameters.getOrElse(Symbol("stagnationThreshold"), 0).asInstanceOf[Int]

  lazy val relaxOps: Array[ALNSOperator] = config.relaxStore.getElements.toArray
  lazy val searchOps: Array[ALNSOperator] = config.searchStore.getElements.toArray

  lazy val relaxWeights: Array[Double] = Array.fill[Double](relaxOps.length)(currentSol.get.objective.toDouble)
  lazy val searchWeights: Array[Double] = Array.fill[Double](searchOps.length)(currentSol.get.objective.toDouble)

  lazy val relaxStore: AdaptiveStore[ALNSOperator] = config.relaxStore
  lazy val searchStore: AdaptiveStore[ALNSOperator] = config.searchStore

  lazy val nOpCombinations: Int = relaxOps.filter(_.isActive).map(_.nParamVals).sum * searchOps.filter(_.isActive).map(_.nParamVals).sum

  def timeInSearch: Long = System.nanoTime() - startTime

  protected def onSolution(): Unit = {
    val time = timeInSearch

    if(maximizeObjective.isDefined) {
      currentSol = Some(new CPIntSol(vars.map(_.value), solver.objective.objs.head.best, time))
      optimumFound = solver.objective.isOptimum() || (config.objective.isDefined && ((maximizeObjective.get && solver.objective.objs.head.best >= config.objective.get) || (!maximizeObjective.get && solver.objective.objs.head.best <= config.objective.get)))
      if (bestSol.isEmpty || (maximizeObjective.get && currentSol.get.objective > bestSol.get.objective) || (!maximizeObjective.get && currentSol.get.objective < bestSol.get.objective)) {
        previousBest = bestSol
        bestSol = currentSol
        solsFound += currentSol.get
      }
    }
    else{
      currentSol = Some(new CPIntSol(vars.map(_.value), 0, time))
      optimumFound = true //In case of CSP, no point of searching another solution
      bestSol = currentSol
      solsFound += currentSol.get
    }
  }

  protected def searchFirstSol(): Unit = {
    if(!solver.silent) println("Starting first solution search...")

    val defaultNSols = nSols
    nSols = 1

    val stats = solver.startSubjectTo(stopCondition, Int.MaxValue, null){
      if(auxiliaryVars.nonEmpty) solver.search(SearchFunctions.conflictOrdering(decisionVars, if(maximizeObjective.isDefined) if(maximizeObjective.get) "Min" else "Max"  else "Max", valLearn = false) ++
        SearchFunctions.conflictOrdering(auxiliaryVars, if(maximizeObjective.isDefined) if(maximizeObjective.get) "Min" else "Max"  else "Max", valLearn = false))
      else solver.search(SearchFunctions.conflictOrdering(decisionVars, if(maximizeObjective.isDefined) if(maximizeObjective.get) "Min" else "Max"  else "Max", valLearn = false))
    }

    nSols = defaultNSols

    if(!solver.silent) println("Time elapsed: " + (System.nanoTime() - startTime)/1000000000.0 + "s")
    iterTimeout = stats.time * 1000000 * 2
    optimumFound |= stats.completed

    if(solsFound.nonEmpty) solsFound(0)  = new CPIntSol(solsFound.head.values, solsFound.head.objective, 0L)
  }

  override def searchFrom(sol: CPIntSol): ALNSSearchResults = {
    if(maximizeObjective.isDefined) {
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = sol.objective
    }
    currentSol = Some(sol)
    bestSol = Some(sol)

    solsFound += new CPIntSol(sol.values, sol.objective, 0L)

    search()
  }

  override def search(): ALNSSearchResults = {
    startTime = System.nanoTime()

    //Searching first solution if needed
    if(currentSol.isEmpty){
      searchFirstSol()
    }

    while(System.nanoTime() < endTime && !optimumFound && !stopSearch){

      //learning phase:
      if (config.metaParameters.getOrElse(Symbol("learning"), false).asInstanceOf[Boolean]) {
        if (!solver.silent) println("\nStarting learning phase...")
        alnsLearning()
        if (!solver.silent) {
          println("Learning phase done.")
          println("Time elapsed: " + (System.nanoTime() - startTime) / 1000000000.0 + "s")
        }
      }

      alnsLoop()

//      if(!solver.silent) println("ALNS Search done, resetting operators...")
//      resetStore()
    }

    if(optimumFound && !solver.silent) println("Optimal solution Found!")
    Metrics.worstTTI = 1.0
    if(!solver.silent) println("Search done, retrieving results")
    val results = new ALNSSearchResults(
      solsFound.toArray,
      relaxOps.filter(_.name != "dummy"),
      searchOps,
      maximizeObjective.isDefined & optimumFound,
      solsFound.isEmpty & optimumFound,
      history.toArray
    )
    if(!solver.silent) println(results)
    results
  }

  protected def alnsLoop(): Unit = {
    if (!solver.silent) println("\nStarting adaptive LNS...")

    val t = timeInSearch

    relaxWeights.zipWithIndex.foreach{case(score, index) =>
      history += ((t, relaxOps(index).name, score))
    }

    searchWeights.zipWithIndex.foreach{case(score, index) =>
      history += ((t, searchOps(index).name, score))
    }

    stagnation = 0
    while(
      System.nanoTime() < endTime &&
      relaxStore.nonActiveEmpty &&
      searchStore.nonActiveEmpty &&
      !optimumFound &&
      !stopSearch
    ){
      lnsIter(relaxStore.select(), searchStore.select())
      if(stagnationThreshold > 0 && stagnation >= stagnationThreshold) stopSearch = true
    }
  }

  protected def lnsIter(relax: ALNSOperator, search: ALNSOperator, sol: CPIntSol = currentSol.get): Unit = {
    if(!learning) endIter = Math.min(System.nanoTime() + iterTimeout, endTime)

    if(!solver.silent){
      println("\nStarting new search with: " + relax.name + " and " + search.name)
      println("Operator timeout: " + (endIter - System.nanoTime())/1000000000.0 + "s")
    }

    //New search using selected strategies:
    val (relaxFunction, _, _) = relax.getFunction
    val (searchFunction, searchFailures, searchDiscrepancy) = search.getFunction
    if(searchFailures.isDefined) nFailures = searchFailures.get

    var relaxDone = true
    val searchObjective = currentSol.get.objective
    val startObjective = sol.objective
    if(startObjective != searchObjective){
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = startObjective
      currentSol = Some(sol)
    }
    val iterStart = timeInSearch

    val stats = solver.startSubjectTo(stopCondition, searchDiscrepancy.getOrElse(Int.MaxValue), null) {
      try {
        relaxFunction(sol)
        searchFunction(sol)
      }
      catch {
        case _: Inconsistency => relaxDone = false
      }
    }

    val iterEnd = timeInSearch
    val newObjective = currentSol.get.objective

    if(searchFailures.isDefined) nFailures = 0 //Resetting failures number to 0

    val improvement = math.abs(newObjective - startObjective)

    if(math.abs(newObjective - searchObjective) > 0) stagnation = 0
    else stagnation += 1

    if(stats.completed && relax.name == ALNSBuilder.FullRelax) optimumFound = true

    if (!solver.silent){
      if(!relaxDone) println("Search space empty, search not applied, improvement: " + improvement)
      else if(stats.completed) println("Search space completely explored, improvement: " + improvement)
      else println("Search done, Improvement: " + improvement)
    }

    //Updating probability distributions:
    relax.update(iterStart, iterEnd, startObjective, newObjective, stats, fail = !relaxDone && !learning, iter)
    if(!relax.isInstanceOf[ALNSReifiedOperator]){
      val relaxScore = if (relax.isActive) relaxStore.adapt(relax)
      else {
        if (!solver.silent) println("Operator " + relax.name + " deactivated")
        if (!learning) relaxStore.deactivate(relax)
        -1.0
      }
      val index = relaxOps.indexOf(relax)
      if(relaxWeights(index) !=  relaxScore) history += ((iterEnd, relax.name, relaxScore))
    }

    if(relaxDone || relax.name == "dummy"){
      search.update(iterStart, iterEnd, startObjective, newObjective, stats, fail = false, iter)
      if(!search.isInstanceOf[ALNSReifiedOperator]){
        val searchScore = if (search.isActive) searchStore.adapt(search)
        else {
          if (!solver.silent) println("Operator " + search.name + " deactivated")
          if (!learning) searchStore.deactivate(search)
          -1.0
        }
        val index = searchOps.indexOf(search)
        if(searchWeights(index) != searchScore) history += ((iterEnd, search.name, searchScore))
      }
    }

    if(currentSol.get.objective != bestSol.get.objective){
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = bestSol.get.objective
      currentSol = bestSol
    }
    iter += 1
  }

  /**
    * Performs a learning phase where each operator is tried at least once from the same solution.
    */
  protected def alnsLearning(): Unit = {
    learning = true
    val initSol = currentSol.get

    val learningStart = System.nanoTime()
    val tAvail = (((endTime - learningStart) * learnRatio) / nOpCombinations).toLong
    val relaxPerf = mutable.Map[String, (ALNSOperator, mutable.ArrayBuffer[(Long, Int)])]()
    val searchPerf = mutable.Map[String, (ALNSOperator, mutable.ArrayBuffer[(Long, Int)])]()

    for {
      relax <- Random.shuffle(getReifiedOperators(relaxOps))
      search <- Random.shuffle(getReifiedOperators(searchOps))
    } {
      endIter = Math.min(System.nanoTime() + tAvail, endTime)

      while (System.nanoTime() < endIter){
        lnsIter(relax, search)
      }

      val execStats = relax.lastExecStats

      val time = if(execStats.isDefined) execStats.get.time else 0L
      val improvement = if(execStats.isDefined) execStats.get.improvement else 0

      if(!relaxPerf.contains(relax.name)) relaxPerf += relax.name -> (relax, mutable.ArrayBuffer[(Long, Int)]())
      relaxPerf(relax.name)._2 += ((time, improvement))
      if(!searchPerf.contains(search.name)) searchPerf += search.name -> (relax, mutable.ArrayBuffer[(Long, Int)]())
      searchPerf(search.name)._2 += ((time, improvement))

      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = initSol.objective
      currentSol = Some(initSol)
    }

    if(config.metaParameters.getOrElse(Symbol("opDeactivation"), false).asInstanceOf[Boolean]) {
      relaxPerf.values.filter { case (op, perfs) =>
        op.isActive && perfs.map(_._2).max == 0
      }.foreach { case (op, _) =>
        op.setActive(false)
        if (!solver.silent) println("Operator " + op.name + " deactivated")
      }

      searchPerf.values.filter { case (op, perfs) =>
        op.isActive && perfs.map(_._2).max == 0
      }.foreach { case (op, _) =>
        op.setActive(false)
        if (!solver.silent) println("Operator " + op.name + " deactivated")
      }

      relaxOps.filter(!_.isActive).foreach(relaxStore.deactivate)
      searchOps.filter(!_.isActive).foreach(searchStore.deactivate)
    }

    if(!solver.silent) {
      println(relaxStore.nActive + " relax operators remaining.")
      println(searchStore.nActive + " search operators remaining.")
    }

    currentSol = bestSol
    solver.objective.objs.head.best = bestSol.get.objective
    endIter = endTime
    learning = false
  }

  protected def getReifiedOperators(operators: Seq[ALNSOperator]) = operators.flatMap{
    case op: ALNSSingleParamOperator[_] => op.getReifiedParameters
    case op: ALNSTwoParamsOperator[_,_] => op.getReifiedParameters
    case op: ALNSOperator => Seq(op)
  }

  /**
    * resets the store(s)
    */
  protected def resetStore(): Unit = {
    relaxOps.foreach(operator => {
      operator.resetFails()
      operator.setActive(true)
    })
    searchOps.foreach(operator => {
      operator.resetFails()
      operator.setActive(true)
    })
    relaxStore.reset()
    searchStore.reset()
  }

  protected def onStagnation(): Unit = {
    ???
//    if(!solver.silent) println("Stagnation")
//
//    learning = true
//    val initSol = previousBest.get
//    val bestObjective = bestSol.get.objective
//    val tAvail = iterTimeout
//    currentSol = Some(initSol)
//
//    val sortedOps = operators.toSeq.sortBy(op => iter - op.lastSuccessIter)
//    var i = 0
//    var newSolFound = false
//    while(i < sortedOps.length && !newSolFound){
//      endIter = System.nanoTime() + tAvail
//      val operator = sortedOps(i)
//      var bestReached = false
//      while(System.nanoTime() < endIter && !bestReached){
//        lnsIter(operator)
//        bestReached = (maximizeObjective.get && currentSol.get.objective >= bestObjective) || (!maximizeObjective.get && currentSol.get.objective <= bestObjective)
//        newSolFound = (maximizeObjective.get && currentSol.get.objective > bestObjective) || (!maximizeObjective.get && currentSol.get.objective < bestObjective)
//      }
//
//      if(bestReached){
//        endIter = System.nanoTime() + tAvail
//        while(System.nanoTime() < endIter && !newSolFound) {
//          lnsIter(operator)
//          newSolFound = (maximizeObjective.get && currentSol.get.objective > bestObjective) || (!maximizeObjective.get && currentSol.get.objective < bestObjective)
//        }
//      }
//
//      //Restoring initial objective:
//      solver.objective.objs.head.relax()
//      solver.objective.objs.head.best = initSol.objective
//      currentSol = Some(initSol)
//      i += 1
//    }
//
//    currentSol = bestSol
//    solver.objective.objs.head.best = bestSol.get.objective
//    endIter = endTime
//    learning = false
//
//    if(newSolFound){
//      stagnation = 0
//      if(!solver.silent) println("Stagnation over")
//    }
  }

  protected def paramTuning(): Unit = ???
}
