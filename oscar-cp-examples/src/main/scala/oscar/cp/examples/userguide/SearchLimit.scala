package oscar.cp.examples.userguide

import oscar.cp._

object SearchLimit extends CPModel with App {

  val X = Array.fill(30)(CPIntVar(0 to 1))

  search {
    binaryStatic(X)
  }

  // start the search for maximum 3 seconds
  var stats = start(timeLimit = 3)
  println("#time: " + stats.time)
  println(stats)

  var nSolutions = 0

  onSolution {
    nSolutions += 1
  }

  // start the search but stop it after 5 solutions met
  stats = start(nSolutions == 5)

  println("nSols:"+stats.nSols)

}
