package oscar.cp.examples.userguide

import oscar.cp._

object BasicModel extends CPModel with App {
  val x1 = CPIntVar(Set(1,5,9)) // Dom(x1) = {1,5,9}
  val x2 = CPIntVar(1 to 5)     // Dom(x2) = [1..5]
  val x3 = CPIntVar(1 until 5)  // Dom(x3) = [1..4]


  add(x1 !== x2)
  add(x1 + x2 === x3)

  search {
    binaryFirstFail(Seq(x1,x2))
  } onSolution {
    println("Solution found, value of x1 in this solution:" + x1.value)
  }

  start(nSols = 1)
}