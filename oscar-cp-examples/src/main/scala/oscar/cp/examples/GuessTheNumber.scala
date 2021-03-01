package oscar.cp.examples

import oscar.cp._

/**
 * I know a 5 digit number having a property that With a 1 after it,
 * it is three times as large as it would be with a one before it.
 * Guess the number ?
 *
 * @author Eric Loots
 */
object GuessTheNumber extends CPModel with App {
  //  model imagined by JF Puget
  val x = CPIntVar(0 to 100000)
  add(x * 10 + 1 === (x + 100000) * 3)
  search { binaryStatic(Seq(x)) }
  onSolution {
    println(s"x => $x")
  }
  val stats = start()
  println(stats)
}
