package oscar.cbls.modeling

import oscar.cbls.modeling.invariant.{Logic, MinMax, Numeric, Routing, Seq}

object Invariants {

  object logic   extends Logic
  object minMax  extends MinMax
  object numeric extends Numeric
  object routing extends Routing
  object seq     extends Seq
}
