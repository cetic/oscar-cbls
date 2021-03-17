package oscar.cp.constraints.nooverlap

import oscar.cp._
import oscar.cp.constraints.InSetReif
import oscar.cp.constraints.nooverlap.util.TransitionTimesUtils
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar

class AlternativeResources(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], runOnResource: Array[CPIntVar], realResourceIds: Set[Int]) extends Constraint(starts(0).store) {
  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ runOnResource

  override def setup(l: CPPropagStrength): Unit = {
    for (r <- realResourceIds){
      s.post(new NoOverlap(starts, durations, ends, runOnResource, r))
    }
  }
}

class AlternativeResourcesTransitionTimes(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], runOnResource: Array[CPIntVar], realResourceIds: Set[Int], transitionMatrix: Array[Array[Int]]) extends AlternativeResources(starts, durations, ends, runOnResource, realResourceIds) {
  private[this] val nTasks = starts.length
  protected def postBinaryPrecedences(): Unit = {
    for(t1 <- 0 until nTasks){
      val runOnRealResource = CPBoolVar()(runOnResource(t1).store)
      s.post(new InSetReif(runOnResource(t1), realResourceIds, runOnRealResource))
      for(t2 <- t1 until nTasks) if(transitionMatrix(t1)(t2) > 0 || transitionMatrix(t2)(t1) > 0){
        val runOnSameResource = runOnResource(t1) ?=== runOnResource(t2)
        val binaryNoOverlap = ((ends(t1) + transitionMatrix(t1)(t2)) ?<= starts(t2)) || ((ends(t2) + transitionMatrix(t2)(t1)) ?<= starts(t1))
        s.post((runOnSameResource && runOnRealResource) ==> binaryNoOverlap)
      }
    }
  }
  override def setup(l: CPPropagStrength): Unit = {
    for (r <- realResourceIds){
      s.post(new NoOverlapTransitionTimes(starts, durations, ends, transitionMatrix, runOnResource, r, false))
    }
    postBinaryPrecedences()
  }
}

class AlternativeResourcesTransitionTimesFamilies(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], familyMatrix: Array[Array[Int]], family: Array[Int], runOnResource: Array[CPIntVar], realResourceIds: Set[Int], exactLB: Boolean=false) extends AlternativeResourcesTransitionTimes(starts, durations, ends, runOnResource, realResourceIds, TransitionTimesUtils.transitionMatrixFromFamilyMatrix(family, familyMatrix)) {
  override def setup(l: CPPropagStrength): Unit = {
    for (r <- realResourceIds){
      s.post(new NoOverlapTransitionTimesFamilies(starts, durations, ends, familyMatrix, family, runOnResource, r, exactLB, false))
    }
    postBinaryPrecedences()
  }
}
