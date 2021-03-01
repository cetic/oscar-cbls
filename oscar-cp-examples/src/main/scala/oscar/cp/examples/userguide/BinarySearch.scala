package oscar.cp.examples.userguide

import oscar.cp._

object BinarySearch extends CPModel with App {

  val X = Array.fill(3)(CPBoolVar())

  add(X(0) || X(1)) // of the two must be true

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
  val stats = start() // start the search, and get search statistics
  println("#solutions: "+ stats.nSols)
}