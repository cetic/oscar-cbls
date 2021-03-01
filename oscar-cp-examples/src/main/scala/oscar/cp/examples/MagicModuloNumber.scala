package oscar.cp.examples

import oscar.cp.{CPIntVar, CPModel, add, binaryFirstFail, modulo, onSolution, search, start}

/**
 * A number with an interesting property:
 *
 * When I divide it by  2, the remainder is 1.
 * When I divide it by  3, the remainder is 2.
 * When I divide it by  4, the remainder is 3.
 * When I divide it by  5, the remainder is 4.
 * When I divide it by  6, the remainder is 5.
 * When I divide it by  7, the remainder is 6.
 * When I divide it by  8, the remainder is 7.
 * When I divide it by  9, the remainder is 8.
 * When I divide it by 10, the remainder is 9.
 *
 * It's not a small number, but it's not really big, either.
 * When I looked for a smaller number with this property I couldn't find one.
 * Can you find it?
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object MagicModuloNumber extends CPModel with App {

  val x = CPIntVar(0 to 10000)

  for (i <- 2 to 10) {
    add(modulo(x, i, i - 1))
  }

  search { binaryFirstFail(Seq(x)) }

  onSolution { println(x) }

  val stats = start(nSols = 1)
  println(stats)
}
