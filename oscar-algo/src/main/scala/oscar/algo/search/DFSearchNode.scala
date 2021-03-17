package oscar.algo.search


import oscar.algo.Inconsistency

import scala.util.Random
import oscar.algo.reversible.ReversibleContextImpl
import oscar.algo.reversible.ReversibleBoolean

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class DFSearchNode extends ReversibleContextImpl with ConstrainableContext {

  var silent = false

  val random: Random = new Random(0)

  /**
    * @return The Random generator of this node potentially used in other algorithms
    */
  def getRandom: Random = random


  protected val failed = new ReversibleBoolean(this, false)

  protected val searchStrategy = new DFSearch(this)

  protected var heuristic: Branching = null

  final def searchEngine: DFSearch = searchStrategy

  final def onSolution(action: => Unit): DFSearchNode = {
    searchStrategy.onSolution(action); this
    //statusBehaviourDelegate.onSolution(action); this
  }

  final def onFailure(action: => Unit): DFSearchNode = {
    searchStrategy.onFailure(action); this
  }

  /** @return  true if this node can surely not lead to any solution */
  def isFailed: Boolean = failed.value

  /** Set the node in a failed state */
  def fail(): Unit = failed.setTrue()
  
  /** This function is executed when the node becomes a solution */
  def solFound(): Unit = ()

  def start: Unit = {
    start(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, null)
  }

  def start(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue, listener: DFSearchListener = null): SearchStatistics = {
    startSubjectTo(nSols, failureLimit, timeLimit, maxDiscrepancy, listener)()
  }

  def start(stopCondition: => Boolean): SearchStatistics = {
    startSubjectTo(stopCondition, Int.MaxValue, null)({})
  }

  def start(stopCondition: => Boolean, listener: DFSearchListener): SearchStatistics = {
    startSubjectTo(stopCondition, Int.MaxValue)({})
  }

  def start(stopCondition: => Boolean, maxDiscrepancy: Int): SearchStatistics = {
    startSubjectTo(stopCondition, maxDiscrepancy, null)({})
  }

  def start(stopCondition: => Boolean, maxDiscrepancy: Int, listener: DFSearchListener): SearchStatistics = {
    startSubjectTo(stopCondition, maxDiscrepancy, listener)({})
  }

  def startSubjectTo(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue, listener: DFSearchListener = null)(block: => Unit = () => ()): SearchStatistics = {
    val stopCondition = buildStopCondition(nSols, failureLimit, timeLimit)
    startSubjectTo(stopCondition, maxDiscrepancy, listener)(block)
  }

  def startSubjectTo(stopCondition: => Boolean)(block: => Unit): SearchStatistics = {
    startSubjectTo(stopCondition, Int.MaxValue, null)(block)
  }

  def startSubjectTo(stopCondition: => Boolean, listener: DFSearchListener)(block: => Unit): SearchStatistics = {
    startSubjectTo(stopCondition, Int.MaxValue, listener)(block)
  }

  def startSubjectTo(stopCondition: => Boolean, maxDiscrepancy: Int)(block: => Unit): SearchStatistics = {
    startSubjectTo((s: DFSearch) => stopCondition, maxDiscrepancy, null)(block)
  }

  def startSubjectTo(stopCondition: => Boolean, maxDiscrepancy: Int, listener: DFSearchListener)(block: => Unit): SearchStatistics = {
    startSubjectTo((s: DFSearch) => stopCondition, maxDiscrepancy, listener)(block)
  }

  def startSubjectTo(stopCondition: DFSearch => Boolean, maxDiscrepancy: Int, listener: DFSearchListener)(block: => Unit): SearchStatistics = {
    val t0 = System.currentTimeMillis()
    pushState() // Store the current state
    try {
      block // Apply the before search action
    }
    catch {
      case _: Inconsistency => fail()
    }
    searchStrategy.searchListener = listener // Set the listener
    searchStrategy.start(heuristic.maxDiscrepancy(maxDiscrepancy), stopCondition)
    pop() // Restore the current state
    searchStrategy.searchListener = null  // Remove the listener
    // Build the statistic object
    new SearchStatistics(
      searchStrategy.nNodes,
      searchStrategy.nBacktracks,
      System.currentTimeMillis() - t0,
      searchStrategy.isCompleted,
      this.time,
      this.maxSize,
      searchStrategy.nSolutions
    )
  }

  @inline private def buildStopCondition(nSols: Int, failureLimit: Int, timeLimit: Int): Function1[DFSearch, Boolean] = {
    // Build the stop condition
    val checkSol = nSols < Int.MaxValue
    val checkFailures = failureLimit < Int.MaxValue
    val checkTime = timeLimit < Int.MaxValue
    val maxTime = (timeLimit * 1000) + System.currentTimeMillis()
    (s: DFSearch) => {
      var stop = false
      stop |= (checkSol && s.nSolutions >= nSols)
      stop |= (checkFailures && s.nBacktracks >= failureLimit)
      stop |= (checkTime && System.currentTimeMillis() >= maxTime)
      stop
    }
  }

  def search(block: => Seq[Alternative]): DFSearchNode = {
    heuristic = Branching(block); this
  }

  def search(branching: Branching): DFSearchNode = {
    heuristic = branching; this
  }
}