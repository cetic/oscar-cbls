package oscar.cp.examples.userguide

import oscar.cp._
import oscar.cp.core._
import oscar.cp.core.variables.CPVar

object SimpleUserConstraint extends CPModel with App {

  class MyLessOrEqual(val X: CPIntVar, val Y: CPIntVar ) extends Constraint(X.store, "MyLessOrEqual") {

    override def associatedVars(): Iterable[CPVar] = Array(X, Y)

    override def setup(l: CPPropagStrength): Unit =  {
      X.callPropagateWhenBoundsChange(this)
      Y.callPropagateWhenBoundsChange(this)
      propagate()
    }

    override def propagate(): Unit = {
      if (Y.min >= X.max)
        deactivate()
      else {
        Y.updateMin(X.min)
        X.updateMax(Y.max)
      }
    }
  }

  val x = CPIntVar(0 to 10)
  val y = CPIntVar(0 to 5)

  add (new MyLessOrEqual(x,y))
}
