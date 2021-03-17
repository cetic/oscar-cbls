package oscar.cp.constraints.nooverlap

import oscar.cp.constraints.nooverlap.thetalambdatree.ThetaLambdaTreeTransitionTimesFamilies
import oscar.cp.constraints.nooverlap.util.TransitionTimesUtils
import oscar.cp.core.variables.CPIntVar

class NoOverlapLeftToRightTransitionTimesFamilies(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], runOnResource: Array[CPIntVar], resourceId : Int, families: Array[Int], familyMatrix: Array[Array[Int]], familyTransitionSetLowerBounds: Array[Int]) extends NoOverlapLeftToRightTransitionTimes(starts, durations, ends, runOnResource, resourceId, TransitionTimesUtils.transitionMatrixFromFamilyMatrix(families, familyMatrix), familyTransitionSetLowerBounds) {
  private[this] val nFamilies = familyMatrix.length
  protected[this] val familyMatrixTransposed = Array.tabulate(nFamilies)(i => Array.tabulate(nFamilies)(j => familyMatrix(j)(i)))
  protected[this] val minTransitionTimeToFamily = Array.tabulate(nFamilies)(i => if (familyMatrixTransposed(i).length > 1) familyMatrixTransposed(i).sorted.apply(1) else 0)
  protected[this] val minTransitionTimeFromFamily = Array.tabulate(nFamilies)(i => if (familyMatrix(i).length > 1) familyMatrix(i).sorted.apply(1) else 0)


  override protected[this] val tree : ThetaLambdaTreeTransitionTimesFamilies = new ThetaLambdaTreeTransitionTimesFamilies(starts.length, families, minTransitionTimeToFamily, familyTransitionSetLowerBounds)

  override protected def additionalTime(task : Int, isFrom: Boolean): Int = {
    val familiesInTheta = tree.familiesInTheta
    if (familiesInTheta != 0 && ((1 << families(task)) & familiesInTheta) == 0) {
      if(isFrom)
        minTransitionTimeFromFamily(families(task))
      else
        minTransitionTimeToFamily(families(task))
    }
    else {
      0
    }
  }

  override protected def strengthenMaxEndWithout(notLastTask: Int): Int = {
    val familiesInTheta = tree.lastFamiliesInThetaWithoutAnActivity
    val curFamily = families(notLastTask)
    if (familiesInTheta != 0 && ((1 << families(notLastTask)) & familiesInTheta) == 0) {
      minTransitionTimeFromFamily(curFamily)
    }
    else {
      0
    }
  }
}