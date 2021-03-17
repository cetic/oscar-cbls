package oscar.cp.examples.userguide

import oscar.cp._

object LimitedDiscrepancy extends CPModel with App {

  val X = Array.fill(3)(CPIntVar(0 to 1))

  onSolution {
    println(X.mkString(","))
  }

  search {
    X.find(!_.isBound) match {
      case None => noAlternative // no more children
      case Some(x) =>
        branch{add(x === 0)} // left alternative
        {add(x === 1)} // right alternative
    }
  }

  // start a search with decrepancy limit <= 1
  start(maxDiscrepancy = 1)

  // iterative discrepancy search
  var d = 0
  for (d <- 0 until 5) {
    start(maxDiscrepancy = d)
  }

}
