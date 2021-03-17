package oscar.cp.searches.lns.search

import oscar.algo.Inconsistency
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.operators.{ALNSElement, ALNSOperator, ALNSReifiedOperator}
import oscar.cp.searches.lns.selection.{AdaptiveStore, Metrics, PriorityStore, RouletteWheel}

import scala.util.Random

/**
  * TODO
  */
class ALNSDiveAndExplore(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig) extends ALNSSearchImpl(solver, vars, config){
//  lazy val efficiencyEvalTime: Long = iterTimeout * 2 //(config.timeout/nOpCombinations.toDouble).ceil.toLong
//  lazy val minEfficiencyThreshold: Double = 0.5 //Math.abs(bestSol.get.objective - previousBest.get.objective) / (efficiencyEvalTime.toDouble * 10)
  lazy val tolerance = 0.5
  lazy val efficiencyEvalIters = 5

  var currentRelaxStore: AdaptiveStore[ALNSOperator] = relaxStore
  var currentSearchStore: AdaptiveStore[ALNSOperator] = searchStore

  protected def computeOpEfficiency(element: ALNSElement): Double = Metrics.efficiencyFor(element, efficiencyEvalIters)

  override def alnsLoop(): Unit = {
    if (!solver.silent){
      println("\nStarting adaptive LNS...")
      println("n operators: " + nOpCombinations)
//      println("Efficiency Threshold: " + minEfficiencyThreshold)
      println("tolerance: " + tolerance)
    }
    if(!solver.silent) println("\nStarting dive...")
    dive()
    if(!solver.silent) println("\nStagnation, starting exploration")
    explore()
  }

  def timeLearning(): Unit = {
    learning = true
    iterTimeout = config.timeout
    for {
      relax <- Random.shuffle(getReifiedOperators(relaxOps))
      search <- Random.shuffle(getReifiedOperators(searchOps))
    } {
      lnsIter(relax, search)
    }
    learning = false

    manageIterTimeout()
    println("learning done, iterTimeout: " + iterTimeout)
  }

  protected def dive(): Unit = {
    //Running each op combination to evaluate TTI:
    timeLearning()

    relaxWeights.indices.foreach(i => relaxWeights(i) = relaxOps(i).efficiency)
    searchWeights.indices.foreach(i => searchWeights(i) = searchOps(i).efficiency)

    val relaxPriority = new PriorityStore[ALNSOperator](relaxOps, relaxWeights.clone(), 1.0, true, computeOpEfficiency)
    val searchPriority = new PriorityStore[ALNSOperator](searchOps, searchWeights.clone(), 1.0, true, computeOpEfficiency)
    currentRelaxStore = relaxPriority
    currentSearchStore = searchPriority

    val t = timeInSearch

    relaxWeights.zipWithIndex.foreach{case(score, index) =>
      history += ((t, relaxOps(index).name, score))
    }
    searchWeights.zipWithIndex.foreach{case(score, index) =>
      history += ((t, searchOps(index).name, score))
    }

    //Diving:

    while(System.nanoTime() < endTime && relaxPriority.nonActiveEmpty && searchPriority.nonActiveEmpty && !optimumFound){
      val relax = relaxPriority.select()
      val search = searchPriority.select()
      lnsIter(relax, search)
      checkEfficiency(relax)
      checkEfficiency(search)
      val execStats = relax.lastExecStats
      if(execStats.isEmpty || execStats.get.improvement == 0){
        if(relax.name != "dummy") relaxPriority.deactivate(relax)
        searchPriority.deactivate(search)
      }
      else{
        relaxPriority.reset()
        searchPriority.reset()
      }
    }
  }

  protected def explore(): Unit = {

    val relaxRWheel = new RouletteWheel[ALNSOperator](relaxOps.filter(_.isActive), relaxOps.filter(_.isActive).map(multiArmedBandit), 1.0, false, multiArmedBandit)
    val searchRWheel = new RouletteWheel[ALNSOperator](searchOps.filter(_.isActive), searchOps.filter(_.isActive).map(multiArmedBandit), 1.0, false, multiArmedBandit)
    currentRelaxStore = relaxRWheel
    currentSearchStore = searchRWheel

    relaxWeights.indices.foreach(i => if(relaxOps(i).isActive) relaxWeights(i) = multiArmedBandit(relaxOps(i)))
    searchWeights.indices.foreach(i => if(searchOps(i).isActive) searchWeights(i) = multiArmedBandit(searchOps(i)))

    val t = timeInSearch

    relaxWeights.zipWithIndex.foreach{case(score, index) =>
      history += ((t, relaxOps(index).name, score))
    }
    searchWeights.zipWithIndex.foreach{case(score, index) =>
      history += ((t, searchOps(index).name, score))
    }

    while(System.nanoTime() < endTime && relaxRWheel.nonActiveEmpty && searchRWheel.nonActiveEmpty && !optimumFound){
      val relax = relaxRWheel.select()
      val search = searchRWheel.select()
      lnsIter(relax, search)
      checkEfficiency(relax)
      if(!relax.isActive){
        relaxRWheel.deactivate(relax)
        manageIterTimeout()
      }
      checkEfficiency(search)
      if(!search.isActive){
        searchRWheel.deactivate(search)
        manageIterTimeout()
      }
    }
  }

  protected def multiArmedBandit(elem: ALNSElement): Double = {
    computeOpEfficiency(elem) + Math.sqrt((2 * Math.log(iter))/elem.execs)
  }

  protected def checkEfficiency(op: ALNSOperator): Unit = {
    if(op.name != "dummy" && op.time >= iterTimeout) {
      val efficiency = Metrics.efficiencyFor(op, iterTimeout * efficiencyEvalIters)
      if(!solver.silent) println("Operator " + op.name + " efficiency is " + efficiency)
      if (efficiency < Metrics.searchEfficiencyFor(solsFound.toSeq, iterTimeout * efficiencyEvalIters, System.nanoTime() - startTime) * tolerance) {
        op.setActive(false)
        if (!solver.silent) println("Operator " + op.name + " deactivated due to low efficiency!")
        manageIterTimeout()
      }
    }
  }

  protected def manageIterTimeout(): Unit = {
    var maxTime = 0L
    searchOps.filter(op =>{ op.isActive && op.execs > 0}).foreach(op =>{
      val avgTime = Metrics.avgTime(op)
      if(avgTime > maxTime) maxTime = avgTime.ceil.toLong
    })
    iterTimeout = maxTime
  }

  override protected def lnsIter(relax: ALNSOperator, search: ALNSOperator, sol: CPIntSol = currentSol.get): Unit = {
    if(!learning) endIter = Math.min(System.nanoTime() + iterTimeout, endTime)

    if(!solver.silent){
      println("\nStarting new search with: " + relax.name + " and " + search.name)
      println("Operator timeout: " + (endIter - System.nanoTime())/1000000000.0 + "s")
    }

    //New search using selected strategies:
    val (relaxFunction, _, _) = relax.getFunction
    val (searchFunction, searchFailures, searchDiscrepancy) = search.getFunction
    if(searchFailures.isDefined) nFailures = searchFailures.get

    val searchObjective = currentSol.get.objective
    if(sol.objective != searchObjective){
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = sol.objective
      currentSol = Some(sol)
    }

    do {
      var relaxDone = true
      val startObjective = currentSol.get.objective

      val iterStart = System.nanoTime()

      val stats = solver.startSubjectTo(stopCondition, searchDiscrepancy.getOrElse(Int.MaxValue), null) {
        try {
          relaxFunction(currentSol.get)
          searchFunction(currentSol.get)
        }
        catch {
          case _: Inconsistency => relaxDone = false
        }
      }

      val iterEnd = System.nanoTime()
      val newObjective = currentSol.get.objective

      if(searchFailures.isDefined) nFailures = 0 //Resetting failures number to 0

      val improvement = math.abs(newObjective - startObjective)
      val time = iterEnd - iterStart

      if(math.abs(newObjective - searchObjective) > 0) stagnation = 0
      else stagnation += 1

      if (!solver.silent){
        if(!relaxDone) println("Search space empty, search not applied, improvement: " + improvement)
        else if(stats.completed) println("Search space completely explored, improvement: " + improvement)
        else println("Search done, Improvement: " + improvement)
      }

      //Updating probability distributions:
      relax.update(iterStart, iterEnd, startObjective, newObjective, stats, fail = !relaxDone && !learning, iter)
      if(!relax.isInstanceOf[ALNSReifiedOperator]){
        val relaxScore = if (relax.isActive) currentRelaxStore.adapt(relax)
        else {
          if (!solver.silent) println("Operator " + relax.name + " deactivated")
          if (!learning) currentRelaxStore.deactivate(relax)
          -1.0
        }
        val index = relaxOps.indexOf(relax)
        if(relaxWeights(index) !=  relaxScore) {
          history += ((timeInSearch, relax.name, relaxScore))
          relaxWeights(index) = relaxScore
        }
      }

      if(relaxDone || relax.name == "dummy"){
        search.update(iterStart, iterEnd, startObjective, newObjective, stats, fail = false, iter)
        if(!search.isInstanceOf[ALNSReifiedOperator]){
          val searchScore = if (search.isActive) currentSearchStore.adapt(search)
          else {
            if (!solver.silent) println("Operator " + search.name + " deactivated")
            if (!learning) currentSearchStore.deactivate(search)
            -1.0
          }
          val index = searchOps.indexOf(search)
          if(searchWeights(index) != searchScore) {
            history += ((timeInSearch, search.name, searchScore))
            searchWeights(searchOps.indexOf(search)) = searchScore
          }
        }
      }
    }while(System.nanoTime() < endIter && !learning)

    if(currentSol.get.objective != bestSol.get.objective){
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = bestSol.get.objective
      currentSol = bestSol
    }
    iter += 1
  }
}
