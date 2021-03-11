package oscar.cp.examples.userguide

import oscar.cp._
import oscar.util._

object PredefinedSearches extends CPModel with App {

  val X = Array.fill(10)(CPIntVar(0 to 10))

  add(X(8)+X(9) === 3)
  add(sum(X) === 9) // sum must be 3
  add(allDifferent(X)) // must all be different

  // start the search and print
  // the number of fails for 3 different searches

  search {
    // variable heuristic: first unbound variable in X
    // value heuristic: min value first (default)
    binaryStatic(X)
  }
  var stats = start()
  println("with binaryStatic:"+stats.nFails)

  search {
    // variable heuristic: min regret (largest domain span)
    // value heuristic: median value of the domain
    binaryIdx(X,i => X(i).max-X(i).min,i => X(i).min)
  }
  stats = start()
  println("with custom binary:"+stats.nFails)

  search {
    // variable heuristic: min size domain
    // value heuristic: maximum value first
    binaryFirstFailIdx(X,i => X(i).max)
  }
  stats = start()
  println("with binaryFirstFail:"+stats.nFails)

  search {
    // variable heuristic: conflict ordering search
    // value heuristic: min value first
    conflictOrderingSearch(X,i => X(i).size, i => X(i).min)
  }
  stats = start()
  println("with conflict ordering search:"+stats.nFails)

}
