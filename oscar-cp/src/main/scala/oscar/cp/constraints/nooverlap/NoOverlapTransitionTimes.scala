package oscar.cp.constraints.nooverlap

import oscar.cp._
import oscar.cp.core.CPPropagStrength
import oscar.cp.scheduling.util.TransitionLowerBounds

class NoOverlapTransitionTimes(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], transitionMatrix: Array[Array[Int]], runOnResource: Array[CPIntVar], resourceId : Int, postBinaryPrecedences: Boolean = true) extends NoOverlap(starts, durations, ends, runOnResource, resourceId) {
  private val transitionLowerBounds = new TransitionLowerBounds(transitionMatrix).getLowerBounds()
  override val leftToRight : NoOverlapLeftToRight = new NoOverlapLeftToRightTransitionTimes(starts, durations, ends, runOnResource, resourceId, transitionMatrix, transitionLowerBounds)
  override val rightToLeft : NoOverlapLeftToRight = new NoOverlapLeftToRightTransitionTimes(ends map (-_), durations, starts map (-_), runOnResource, resourceId, transpose(transitionMatrix), transitionLowerBounds)

  override def setup(l: CPPropagStrength): Unit = {
    super.setup(l)
    if(postBinaryPrecedences) {
      for(t1 <- 0 until nTasks; t2 <- 0 until t1 if(transitionMatrix(t1)(t2) > 0 || transitionMatrix(t2)(t1) > 0))
        s.post((runOnResource(t1) ?!== resourceId) || (runOnResource(t2) ?!== resourceId) || (ends(t1) + transitionMatrix(t1)(t2) ?<= starts(t2)) || (ends(t2) + transitionMatrix(t2)(t1) ?<= starts(t1)))
    }
  }

}