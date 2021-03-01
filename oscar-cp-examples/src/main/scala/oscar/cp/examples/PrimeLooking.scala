package oscar.cp.examples

import oscar.cp.{CPIntVar, CPModel, add, binaryFirstFail, search, start}

/**
 * Martin Gardner Problem:
 * Call a number "prime-looking" if it is composite but not divisible by 2,3 or 5.
 * The three smallest prime-looking numbers are 49, 77 and 91.
 * There are 168 prime numbers less than 1000.
 * How many prime-looking numbers are there less than 1000?
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object PrimeLooking extends CPModel with App {

  val n = 1000

  val x  = CPIntVar(1 to n)
  val d1 = CPIntVar(2 to n)
  val d2 = CPIntVar(2 to n)

  add(d1 * d2 === x)
  add(d1 <= d2) // avoid symmetric solutions

  // prevent divisibility by 2,3 and 5
  for (v <- List(2, 3, 5)) {
    for (i <- 1 to 1000 if (v * i <= n)) {
      add(d1 !== (v * i))
      add(d2 !== (v * i))
    }
  }

  search { binaryFirstFail(Seq(x, d1, d2)) }

  val stats = start()
  println(stats)
}
