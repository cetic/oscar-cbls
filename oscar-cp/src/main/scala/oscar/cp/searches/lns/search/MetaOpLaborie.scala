package oscar.cp.searches.lns.search

import oscar.algo.Inconsistency
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.{ALNSOperator, ALNSReifiedOperator}
import oscar.cp.searches.lns.selection.{AdaptiveStore, Metrics, RouletteWheel}
import oscar.cp.{CPIntVar, CPSolver}

import scala.util.Random

/**
  * TODO
  */
class MetaOpLaborie(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig) extends ALNSSearchImpl(solver, vars, config) {

  val tolerance: Double = config.metaParameters.getOrElse(Symbol("tolerance"), 0.5).asInstanceOf[Double]
  override val stagnationThreshold = 10

  override lazy val relaxStore: AdaptiveStore[ALNSOperator] = new RouletteWheel[ALNSOperator](
    relaxOps,
    relaxWeights.clone(),
    0.2,
    false,
    checkEfficiency
  )

  override lazy val searchStore: AdaptiveStore[ALNSOperator] = new RouletteWheel[ALNSOperator](
    searchOps,
    searchWeights.clone(),
    0.2,
    false,
    checkEfficiency
  )

  override def alnsLoop(): Unit = {
    if (!solver.silent) println("\nStarting adaptive LNS...")
    stagnation = 0

    val t = timeInSearch

//    relaxWeights.zipWithIndex.foreach{case(score, index) =>
//      history += ((t, relaxOps(index).name, score))
//    }
//
//    searchWeights.zipWithIndex.foreach{case(score, index) =>
//      history += ((t, searchOps(index).name, score))
//    }

    timeLearning()

    while (
      System.nanoTime() < endTime &&
        relaxStore.nonActiveEmpty &&
        searchStore.nonActiveEmpty &&
        !optimumFound
    ) {
      val relax = relaxStore.select()
      val search = searchStore.select()
      /*if(stagnation >= stagnationThreshold && previousBest.isDefined) {
        lnsIter(relax, search, previousBest.get)
      }
      else*/ lnsIter(relax, search)
    }
  }

  protected def timeLearning(): Unit = {
    learning = true
    iterTimeout = config.timeout
    val orderedBaseline = config.metaParameters.getOrElse(Symbol("opOrder"), None).asInstanceOf[Option[Seq[String]]]
    val orderedRelax = if(orderedBaseline.isDefined) {
      val mapping = orderedBaseline.get.map(_.split("_")(0)).zipWithIndex.reverse.toMap
      relaxOps.toSeq.sortBy(op => mapping.getOrElse(op.name, mapping.size))
    }
    else Random.shuffle(relaxOps.toSeq)
    val orderedSearch = if(orderedBaseline.isDefined) {
      val mapping = orderedBaseline.get.map(_.split("_")(1)).zipWithIndex.reverse.toMap
      searchOps.toSeq.sortBy(op => mapping.getOrElse(op.name, mapping.size))
    }
    else Random.shuffle(searchOps.toSeq)
    orderedRelax.foreach(relax => {
      val search = searchOps(Random.nextInt(searchOps.length))
      lnsIter(relax, search)
    })
    orderedSearch.foreach(search => {
      while(search.execs < 1) {
        val relax = relaxStore.select()
        lnsIter(relax, search)
      }
    })
    learning = false

    maxOpIterTimeout()
    if(!solver.silent) println("learning done, iterTimeout: " + iterTimeout)
  }

  protected def checkEfficiency(op: ALNSOperator): Double = {
    if(op.name != "dummy") Metrics.efficiencyFor(op, iterTimeout)
    else 1.0
  }

  protected def maxOpIterTimeout(): Unit = {
    var maxTime = 0L
    searchOps.filter(op => {
      op.isActive && op.execs > 0 && op.sols > 0
    }).foreach(op => {
      val avgTime = Metrics.avgTime(op)
      if (avgTime > maxTime) maxTime = avgTime.ceil.toLong
    })
    iterTimeout = if(maxTime == 0L || learning) config.timeout else maxTime
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


      if(relaxDone || relax.name == "dummy"){
        search.update(iterStart, iterEnd, startObjective, newObjective, stats, fail = false, iter)
      }
    }while(System.nanoTime() < endIter && !learning)

    if(!relax.isInstanceOf[ALNSReifiedOperator]){
      var relaxScore = relaxStore.adapt(relax)
      val searchEfficiency = computeCurrentAverageScore(relaxWeights)
      if (opDeactivation && relax.time >= iterTimeout * 2 && (relaxScore < searchEfficiency * tolerance || relax.sols == 0)){
        relax.setActive(false)
        relaxScore = -1.0
        if (!learning) relaxStore.deactivate(relax)
        if (!solver.silent)println("Operator " + relax.name + " deactivated due to low efficiency!")
//        manageIterTimeout()
      }
      if (!solver.silent) {
        println("Search efficiency is " + searchEfficiency)
        println("Operator " + relax.name + " efficiency is " + relaxScore)
      }
      val index = relaxOps.indexOf(relax)
      if(relaxWeights(index) !=  relaxScore) {
        history += ((timeInSearch, relax.name, relaxScore))
        relaxWeights(index) = relaxScore
      }
    }

    if(!search.isInstanceOf[ALNSReifiedOperator]){
      var searchScore = searchStore.adapt(search)
      val searchEfficiency = computeCurrentAverageScore(searchWeights)
      if (opDeactivation && search.time >= iterTimeout * 2 && (searchScore < searchEfficiency * tolerance || search.sols == 0)){
        search.setActive(false)
        searchScore = -1.0
        if (!learning) searchStore.deactivate(search)
        if (!solver.silent)println("Operator " + search.name + " deactivated due to low efficiency!")
//        manageIterTimeout()
      }
      if (!solver.silent) {
        println("Search efficiency is " + searchEfficiency)
        println("Operator " + search.name + " efficiency is " + searchScore)
      }
      val index = searchOps.indexOf(search)
      if(searchWeights(index) != searchScore) {
        history += ((timeInSearch, search.name, searchScore))
        searchWeights(searchOps.indexOf(search)) = searchScore
      }
    }

    if(currentSol.get.objective != bestSol.get.objective){
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = bestSol.get.objective
      currentSol = bestSol
    }
    iter += 1
  }

  protected def computeCurrentAverageScore(scores: Array[Double]): Double ={
    val activeScores = scores.filter(_ > 0)
    val nActives = activeScores.length.toDouble
    (activeScores.map(_ / nActives).sum / nActives) * nActives
  }
}
