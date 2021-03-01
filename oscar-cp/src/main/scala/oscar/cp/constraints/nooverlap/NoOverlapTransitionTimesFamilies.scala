package oscar.cp.constraints.nooverlap

import oscar.cp.constraints.nooverlap.util.{DynamicProgrammingTSP, TransitionTimesUtils}
import oscar.cp.core.variables.CPIntVar
import oscar.cp.scheduling.util.TransitionLowerBounds

class NoOverlapTransitionTimesFamilies(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], familyMatrix: Array[Array[Int]], family: Array[Int], runOnResource: Array[CPIntVar], resourceId : Int, exactLB: Boolean=false, postBinaryPrecedences: Boolean = true) extends NoOverlapTransitionTimes(starts, durations, ends, TransitionTimesUtils.transitionMatrixFromFamilyMatrix(family, familyMatrix), runOnResource, resourceId, postBinaryPrecedences) {
  private val familyTransitionLowerBounds = {
    if (exactLB) {
      DynamicProgrammingTSP.minTransitionTimesPerCardinality(familyMatrix)
    }
    else {
      new TransitionLowerBounds(familyMatrix).getLowerBounds()
    }
  }
  override val leftToRight : NoOverlapLeftToRight = new NoOverlapLeftToRightTransitionTimesFamilies(starts, durations, ends, runOnResource, resourceId, family, familyMatrix, familyTransitionLowerBounds)
  override val rightToLeft : NoOverlapLeftToRight = new NoOverlapLeftToRightTransitionTimesFamilies(ends map (-_), durations, starts map (-_), runOnResource, resourceId, family, transpose(familyMatrix), familyTransitionLowerBounds)
}