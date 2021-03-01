package oscar.cp.constraints.nooverlap

import oscar.cp.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}

class NoOverlap(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], runOnResource: Array[CPIntVar], resourceId : Int) extends Constraint(starts(0).store) {
  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ runOnResource

  protected[this] val nTasks = starts.length
  val leftToRight : NoOverlapLeftToRight = new NoOverlapLeftToRight(starts, durations, ends, runOnResource, resourceId)
  val rightToLeft : NoOverlapLeftToRight = new NoOverlapLeftToRight(ends map (-_), durations, starts map (-_), runOnResource, resourceId)

  override def setup(l: CPPropagStrength): Unit = {
    s.post(leftToRight)
    s.post(rightToLeft)
  }

  protected def transpose(matrix: Array[Array[Int]]) = {
    val nElements = matrix.length
    Array.tabulate(nElements, nElements)((i,j) => matrix(j)(i))
  }
}