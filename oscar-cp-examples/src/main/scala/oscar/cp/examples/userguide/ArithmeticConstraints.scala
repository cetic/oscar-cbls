package oscar.cp.examples.userguide

import oscar.cp._

object ArithmeticConstraints extends CPModel with App {

  val x = Array.fill(10)(CPIntVar(0 to 3))
  add(sum(x) === 10)
  add(sum(0 until x.size)(i => x(i)) === 10) // equivalent to the above line

  val y = Array.fill(10,10)(CPIntVar(0 to 3))
  add(sum(0 until 5, 0 until 5){case(i,j) => y(i)(j)} === 10)
  add(sum(4, 4){case(i,j) => y(i)(j)} === 10) // equivalent to previous line

  val z = CPIntVar(0 to 3)
  add(-z === -2)
  add(z.abs === 2) // (absolute value of x) equal 2

}
