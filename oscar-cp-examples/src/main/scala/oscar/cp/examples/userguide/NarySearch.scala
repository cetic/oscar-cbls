package oscar.cp.examples.userguide

import oscar.cp._

object NarySearch extends CPModel with App {

  val X = Array.fill(3)(CPIntVar(0 to 3))

  add(sum(X) === 3) // sum must be 3
  add(allDifferent(X)) // must all be different

  onSolution {
    println(X.mkString(","))
  }

  search {
    X.find(!_.isBound) match {
      case None => noAlternative // no more children
      case Some(x) => branchAll(0 to 3)(v => add(x === v))
    }
  }
  val stats = start() // start the search, and get search statistics
  println("#solutions: " + stats.nSols)

}
