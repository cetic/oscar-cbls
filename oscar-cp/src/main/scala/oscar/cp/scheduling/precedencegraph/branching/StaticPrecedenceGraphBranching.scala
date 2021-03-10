package oscar.cp.scheduling.precedencegraph.branching

import oscar.algo.reversible.ReversibleInt
import oscar.algo.search.{Branching, Decision}
import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.scheduling.precedencegraph.PrecedenceGraph
import oscar.cp.scheduling.precedencegraph.branching.decisions.PrecedenceDecision

/**
  * Created by saschavancauwelaert on 13/01/2017.
  */

class StaticPrecedenceGraphBranching(starts:Array[CPIntVar], ends: Array[CPIntVar], val precedenceGraph : Option[PrecedenceGraph], machineName: String = "") extends Branching {

  private[this] val nTasks = starts.length
  implicit private[this] val cp = starts(0).store

  private[this] val precGraph = precedenceGraph.getOrElse(null)

  //TODO: for now, the precedences are duplicated, we should remove that
  private[this] val precs : Array[Array[CPBoolVar]] = if(precGraph == null) {
    val internalPrec = Array.ofDim[CPBoolVar](nTasks, nTasks)
    //add the constraints to link scheduling vars with the precs
    for (i <- 0 until nTasks; j <- 0 until nTasks) {
      if (i != j)
        internalPrec(i)(j) = ends(i) ?<= starts(j)
      else {
        internalPrec(i)(j) = CPBoolVar()
        cp.post(internalPrec(i)(j) === 0)
      }
    }
    //a task i is either before or after a task j
    for (i <- 0 until nTasks; j <- 0 until i) {
      cp.post(internalPrec(i)(j) + internalPrec(j)(i) === 1)
    }
    internalPrec
  }
  else
    null

  private[this] val decisionPrecsIndices : Array[(Int,Int)] = Array.tabulate(nTasks,nTasks)((i,j) => (i,j)).flatten.filter(t => t._1 != t._2) //TODO: inefficient
  private[this] val numberMaxOfPrecs = decisionPrecsIndices.length

  private[this] val nFixedPrecRev = new ReversibleInt(cp, 0)
  private[this] var nFixedPrec = 0

  private[this] val nAllocatedActivitiesRev = new ReversibleInt(cp, 0)
  private[this] var nAllocatedActivities = 0

  final override def alternatives(): Seq[Alternative] = {
    //update the number of fixed precedences
    nFixedPrec = nFixedPrecRev.value
    var unboundPrecFound = false
    while (nFixedPrec < numberMaxOfPrecs && !unboundPrecFound) {
      val task1 = decisionPrecsIndices(nFixedPrec)._1
      val task2 = decisionPrecsIndices(nFixedPrec)._2
      if(precGraph != null && precGraph.hasNonDetectablePrecForPair(task1,task2)) {
        nFixedPrec += 1
      }
      else if(precs != null && precs(task1)(task2).isBound) {
        nFixedPrec += 1
      }
      else {
        unboundPrecFound = true
      }
    }
    if(nFixedPrec == numberMaxOfPrecs) {
      nAllocatedActivities = nAllocatedActivitiesRev.value
      // Update nAllocatedActivities (activities not running on machine are considered to be allocated)
      while (nAllocatedActivities < nTasks && starts(nAllocatedActivities).isBound) nAllocatedActivities += 1

      if (nAllocatedActivities == nTasks)
        noAlternative
      else {
        // Trail new nAllocatedActivities
        nAllocatedActivitiesRev.value = nAllocatedActivities
        // Alternatives
        val variable = starts(nAllocatedActivities)
        val value = starts(nAllocatedActivities).min
        val toRet = List(Decision.assign(variable, value), Decision.remove(variable, value))
        toRet
      }
    }
    else {
      nFixedPrecRev.value = nFixedPrec //trail number of fixed precedences
      val task1 = decisionPrecsIndices(nFixedPrec)._1
      val task2 = decisionPrecsIndices(nFixedPrec)._2

      if(precGraph != null && precGraph.isPrecDetectable(task1,task2)) {
        val oneBranch = if (precs != null) new PrecedenceDecision(precs(task1)(task2), 1, task1, task2, machineName) else new PrecGraphDecision(precGraph, task1, task2, machineName)
        List(oneBranch)
      }
      else if(precGraph != null && precGraph.isPrecDetectable(task2,task1)) {
        val oneBranch = if (precs != null) new PrecedenceDecision(precs(task1)(task2), 0, task2, task1, machineName) else new PrecGraphDecision(precGraph, task2, task1, machineName)
        List(oneBranch)
      }
      else {
        val left = if (precs != null) new PrecedenceDecision(precs(task1)(task2), 0, task2, task1, machineName) else new PrecGraphDecision(precGraph, task2, task1, machineName)
        val right = if (precs != null) new PrecedenceDecision(precs(task1)(task2), 1, task1, task2, machineName) else new PrecGraphDecision(precGraph, task1, task2, machineName)
        List(left,right)
      }
    }
  }
}
