package oscar.cp.constraints.nooverlap

import oscar.cp.constraints.nooverlap.thetalambdatree.{ThetaLambdaTree, ThetaLambdaTreeTransitionTimes}
import oscar.cp.core.variables.CPIntVar

class NoOverlapLeftToRightTransitionTimes(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], runOnResource: Array[CPIntVar], resourceId : Int, transitionMatrix: Array[Array[Int]], transitionSetLowerBounds: Array[Int]) extends NoOverlapLeftToRight(starts, durations, ends, runOnResource, resourceId) {

  override protected[this] val tree : ThetaLambdaTree = new ThetaLambdaTreeTransitionTimes(starts.length, transitionSetLowerBounds)

  override protected def strengthenMinStart(currentPredecessorIndex: Int, ectIndex: Int) = {
    if(orderedMaxStartIds(currentPredecessorIndex) != ectIndex) { //pairwise ect of the predecessor can be used to strengthen the minimumum start of current activity (since when transition times are involved, detectable precedence rule does not detect all precedences)
      newMinStarts(ectIndex) = math.max(newMinStarts(ectIndex), currentMinEnds(orderedMaxStartIds(currentPredecessorIndex)) + transitionMatrix(orderedMaxStartIds(currentPredecessorIndex))(ectIndex)) //TODO: use the same idea in not-last and edge-finding !
    }
  }
}
