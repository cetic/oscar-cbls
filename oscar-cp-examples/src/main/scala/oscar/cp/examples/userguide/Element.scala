package oscar.cp.examples.userguide

import oscar.cp._

object Element extends CPModel with App {

  val x = Array(1,3,2,7)

  val matrix = Array(Array(1,3,2,7),
                     Array(4,1,2,7),
                     Array(5,3,1,6),
                     Array(3,2,1,3))
  val w = Array.fill(4)(CPIntVar(0 to 10))
  val y1 = CPIntVar(0 to 3)
  val y2 = CPIntVar(0 to 3)

  val y = CPIntVar(0 to 3)
  val z = CPIntVar(0 to 5)


  add(x(y) === z) // index on array of integers
  add(w(y) === z) // index on array of variables
  add(matrix(y1)(y2) === 3)
}
