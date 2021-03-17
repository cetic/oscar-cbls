package oscar.cp.examples

import oscar.cp._

/**
 * A Dudeney Numbers is a positive integer that is a perfect cube such that the sum of
 * its decimal digits is equal to the cube root of the number.
 * There are only six Dudeney Numbers and those are very easy to find with CP.
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object Dudeney extends CPModel with App {

  val n = 5

  val x = (0 until n).map(v => CPIntVar(0 to 9))
  val nb = CPIntVar(1 to math.pow(10, n).toInt - 1)
  val s = CPIntVar(1 to 9 * n)

  add(nb === (s * s * s))
  add(sum(0 until n)(i => x(i) * (math.pow(10, (n - i - 1)).toInt)) === nb)
  add(sum(x) === s)

  search { binaryFirstFail(x) }

  onSolution { println(nb.value) }

  val stats = start()

  println(stats)
}
