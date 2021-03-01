package oscar.cp.searches.lns.search

import oscar.cp.searches.lns.operators.ALNSOperator
import oscar.cp.searches.lns.selection.{AdaptiveStore, Metrics, RouletteWheel, RouletteWheelAlt}
import oscar.cp.{CPIntVar, CPSolver}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * TODO
  */
class EvalWindowLaborie(solver: CPSolver, decisionVars: Array[CPIntVar], auxiliaryVars: Array[CPIntVar], config: ALNSConfig) extends ALNSSearchImpl(solver, decisionVars, config) {
  val tolerance: Double = config.metaParameters.getOrElse(Symbol("tolerance"), 0.5).asInstanceOf[Double]
  val balance: Double = config.metaParameters.getOrElse(Symbol("balance"), 0.05).asInstanceOf[Double]
  def evalWindow: Long = 10 * iterTimeout
  override val stagnationThreshold = 10
  val altScore: Boolean = config.metaParameters.getOrElse(Symbol("altScore"), false).asInstanceOf[Boolean]
  val quickStart: Boolean = config.metaParameters.getOrElse(Symbol("quickStart"), false).asInstanceOf[Boolean]

  val iterStartState: ArrayBuffer[(Long, Int)] = ArrayBuffer[(Long, Int)]()

  var startObjective = 0

  def this(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig){
    this(solver, vars, Array[CPIntVar](), config)
  }

  def totalEfficiency: Double = Math.abs(startObjective - bestSol.get.objective) / (timeInSearch / 1000000000.0)

  override lazy val relaxStore: AdaptiveStore[ALNSOperator] =
    if(quickStart) new RouletteWheelAlt[ALNSOperator](
      relaxOps,
      relaxWeights.clone(),
      1.0,
      false,
      if(altScore) computeScoreAlt else computeScore
    )
    else new RouletteWheel[ALNSOperator](
      relaxOps,
      relaxWeights.clone(),
      1.0,
      false,
      if(altScore) computeScoreAlt else computeScore
    )

  override lazy val searchStore: AdaptiveStore[ALNSOperator] =
    if(quickStart) new RouletteWheelAlt[ALNSOperator](
      searchOps,
      searchWeights.clone(),
      1.0,
      false,
      if(altScore) computeScoreAlt else computeScore
    )
    else new RouletteWheel[ALNSOperator](
      searchOps,
      searchWeights.clone(),
      1.0,
      false,
      if(altScore) computeScoreAlt else computeScore
    )

  override def alnsLoop(): Unit = {
    startObjective = currentSol.get.objective

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

    if(!quickStart) timeLearning()

    while (
      System.nanoTime() < endTime &&
        relaxStore.nonActiveEmpty &&
        searchStore.nonActiveEmpty &&
        !optimumFound
    ) {
      iterStartState += ((timeInSearch, currentSol.get.objective))
      val relax = relaxStore.select()
      val search = searchStore.select()
      /*if(stagnation >= stagnationThreshold && previousBest.isDefined) {
        lnsIter(relax, search, previousBest.get)
        if(search.lastExecStats.get.improvement > 0) lnsIter(relax, search)
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

    manageIterTimeout()
    if(!solver.silent) println("learning done, iterTimeout: " + iterTimeout)
  }

  protected def computeScore(op: ALNSOperator): Double = {
    if(op.name != "dummy"){
      val now = timeInSearch
      val tWindowStart = if (solsFound.nonEmpty) solsFound.last.time - evalWindow else 0L
      val opLocalEfficiency = Metrics.efficiencySince(op, tWindowStart)
      val searchEfficiency = Metrics.searchEfficiencySince(iterStartState.toSeq, tWindowStart, now, currentSol.get.objective)

      //Computing score:
      val totEfficiency = totalEfficiency
      val opScore = if(totEfficiency <= 0) 0.0 else (1 - balance) * opLocalEfficiency + balance * (searchEfficiency / totEfficiency) * op.efficiency

      if (!solver.silent) {
        println("Search efficiency is " + searchEfficiency)
        println("Operator " + op.name + " efficiency is " + opLocalEfficiency)
        println("Operator " + op.name + " score is " + opScore)
      }

      if (opDeactivation && op.time >= iterTimeout * 2 && (opLocalEfficiency < searchEfficiency * tolerance || op.sols == 0)){
        op.setActive(false)
        if (!solver.silent) println("Operator " + op.name + " deactivated due to low efficiency!")
//        manageIterTimeout()
      }

      opScore
    }
    else 1.0
  }

  protected def computeScoreAlt(op: ALNSOperator): Double = {
    if(op.name != "dummy"){
      val now = timeInSearch
      val tWindowStart = if (solsFound.nonEmpty) solsFound.last.time - evalWindow else 0L
      val opEfficiency = Metrics.efficiencySince(op, tWindowStart)
      val searchEfficiency = Metrics.searchEfficiencySince(iterStartState.toSeq, tWindowStart, now, currentSol.get.objective)

      if (!solver.silent) {
        println("Search efficiency is " + searchEfficiency)
        println("Operator " + op.name + " efficiency is " + opEfficiency)
      }

      if (opDeactivation && op.time >= iterTimeout * 2 && (opEfficiency < searchEfficiency * tolerance || op.sols == 0)){
        op.setActive(false)
        if (!solver.silent) println("Operator " + op.name + " deactivated due to low efficiency!")
        //        manageIterTimeout()
      }

      opEfficiency
    }
    else 1.0
  }

  protected def manageIterTimeout(): Unit = {
    var maxTime = 0L
    searchOps.filter(op => {
      op.isActive && op.execs > 0 && op.sols > 0
    }).foreach(op => {
      val avgTime = Metrics.avgTime(op)
      if (avgTime > maxTime) maxTime = avgTime.ceil.toLong
    })
    iterTimeout = if(maxTime == 0L || learning) config.timeout else maxTime
  }
}
