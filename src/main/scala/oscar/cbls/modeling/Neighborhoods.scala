package oscar.cbls.modeling

import oscar.cbls.modeling.search.{Combinator, Neighborhoods, Routing}

object Neighborhoods extends Neighborhoods {

  object combinator extends Combinator
  object routing    extends Routing
}
