package oscar.cp.examples


import oscar.cp._

/**
 * Martin Gardner Problem:
 *
 * In this addition problem, each letter stands for a different digit.
 *
 *   T W O
 * + T W O
 * --------
 * F O U R
 *
 * If T = 7 and the letter O represents an even number, what is the only possible value for W
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object GardnerTwoPlusTwo extends CPModel with App {

  val T = CPIntVar(0 to 9)
  val W = CPIntVar(0 to 9)
  val O = CPIntVar(Set(0, 2, 4, 6, 8)) // even number
  val F = CPIntVar(0 to 9)
  val U = CPIntVar(0 to 9)
  val R = CPIntVar(0 to 9)

  val variables = Seq(T, W, O, F, U, R)

  add((T * 100 + W * 10 + O) * 2 === F * 1000 + O * 100 + U * 10 + R)
  add(allDifferent(variables), Strong)

  search {
    binaryFirstFail(variables)
  }

  onSolution {
    println("T:" + T + " W:" + W + " O:" + O + " F:" + F + " U:" + U + " R:" + R)
  }

  val stats = start()

  println(stats)
}
