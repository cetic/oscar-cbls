package oscar.cp.scheduling.precedencegraph

import oscar.algo.Inconsistency
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.{CPPropagStrength, Constraint}

class PrecedenceGraphPropagator(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], val pg: PrecedenceGraph, ttMatrix: Array[Array[Int]]) extends Constraint(starts(0).store) {

  //no transition times
  def this(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], pg: PrecedenceGraph) {
    this(starts, durations, ends, pg, Array.fill(starts.length, starts.length)(0))
  }

  private[this] val nTasks = starts.length

  //caches
  private[this] val currentMinDurations: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val currentMinStarts: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val currentMaxStarts: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val currentMinEnds: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val currentMaxEnds: Array[Int] = Array.ofDim[Int](nTasks)

  //updates
  private[this] val newMinStarts: Array[Int] = Array.ofDim[Int](nTasks)
  private[this] val newMaxEnds: Array[Int] = Array.ofDim[Int](nTasks)

  pg.subscribe(this)

  override def associatedVars() = starts ++ durations ++ ends

  override def setup(l: CPPropagStrength) = {
    for (i <- 0 until nTasks) {
      starts(i).callPropagateWhenBoundsChange(this)
      ends(i).callPropagateWhenBoundsChange(this)
    }
    propagate()
  }

  override def propagate(): Unit = {
    updateInputs()
    val topologicalOrder = pg.topologicalOrder() //TODO: would be nice to compute this incrementally

    //left to right
    var i = 0
    while (i < nTasks - 1) { //last task has no successors
      val task = topologicalOrder(i)
      val nbSuccessors = pg.nonDetectableSuccessorsOf(task)
      var j = 0
      while (j < nbSuccessors) {
        val successor = pg.nonDetectableSuccessorsContainers(task)(j)
        newMinStarts(successor) = math.max(newMinStarts(task) + currentMinDurations(task) + ttMatrix(task)(successor), newMinStarts(successor))
        if (newMinStarts(successor) > currentMaxStarts(successor))
          throw Inconsistency
        newMaxEnds(task) = math.min(newMaxEnds(successor) - currentMinDurations(successor) - ttMatrix(task)(successor), newMaxEnds(task))
        if (newMaxEnds(task) < currentMinEnds(task))
          throw Inconsistency
        j += 1
      }
      i += 1
    }

    //right to left
    i -= 1
    while (i > 0) { //first task has no predecessors
      val task = topologicalOrder(i)
      val nbPredecessors = pg.nonDetectablePredecessorsOf(task)
      var j = 0
      while (j < nbPredecessors) {
        val predecessor = pg.nonDetectablePredecessorsContainers(task)(j)
        newMaxEnds(predecessor) = math.min(newMaxEnds(task) - currentMinDurations(task) - ttMatrix(predecessor)(task), newMaxEnds(predecessor))
        if (newMaxEnds(predecessor) < currentMinEnds(predecessor))
          throw Inconsistency

        newMinStarts(task) = math.max(newMinStarts(predecessor) + currentMinDurations(predecessor) + ttMatrix(predecessor)(task), newMinStarts(task))
        if (newMinStarts(task) > currentMaxStarts(task))
          throw Inconsistency
        j += 1
      }
      i -= 1
    }

    updateBounds()
  }

  @inline
  protected def updateInputs(): Unit = {
    var i = 0
    while (i < nTasks) {
      currentMinDurations(i) = durations(i).min
      currentMinStarts(i) = starts(i).min
      currentMaxStarts(i) = starts(i).max
      currentMinEnds(i) = ends(i).min
      currentMaxEnds(i) = ends(i).max

      newMinStarts(i) = currentMinStarts(i)
      newMaxEnds(i) = currentMaxEnds(i)

      i += 1
    }
  }

  @inline
  protected def updateBounds(): Unit = {
    var i = 0
    while (i < nTasks) {
      if (newMinStarts(i) > currentMinStarts(i)) {
        if (newMinStarts(i) > currentMaxStarts(i) || newMinStarts(i) + currentMinDurations(i) > currentMaxEnds(i)) { //TODO: this check is already done during propagation, remove it here
          throw Inconsistency
        }
        starts(i).updateMin(newMinStarts(i))
        ends(i).updateMin(newMinStarts(i) + currentMinDurations(i))
      }
      if (newMaxEnds(i) < currentMaxEnds(i)) {
        if (newMaxEnds(i) < ends(i).min || newMaxEnds(i) - currentMinDurations(i) < starts(i).min) {
          throw Inconsistency
        }
        ends(i).updateMax(newMaxEnds(i))
        starts(i).updateMax(newMaxEnds(i) - currentMinDurations(i))
      }
      i += 1
    }
  }
}
