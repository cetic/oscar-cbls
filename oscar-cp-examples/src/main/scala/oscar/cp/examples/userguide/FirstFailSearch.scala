package oscar.cp.examples.userguide

import oscar.cp._
import oscar.util._

object FirstFailSearch extends CPModel with App {

  val X = Array.fill(10)(CPIntVar(0 to 10))

  add(X(8)+X(9) === 3)
  add(sum(X) === 9) // sum must be 3
  add(allDifferent(X)) // must all be different

  onSolution {
    println(X.mkString(","))
  }

  search {
    // select first the unbound variable with smallest domain
    selectMin(X)(!_.isBound)(_.size) match {
      case None => noAlternative // no more children
      case Some(x) => {
        val v = x.min
        branch(add(x === v))(add(x !== v))
      }
    }
  }


  val stats = start() // start the search, and get search statistics
  println("#fails:"+stats.nFails)

}
