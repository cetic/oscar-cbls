package oscar.cp.examples

import oscar.cp.{CPIntVar, CPModel, Medium, add, binaryStaticIdx, gcc, onSolution, search, start}

/**
 * Compute a sequence a_i (0 <= i < 10) such that the sequence contains exactly a_i occurrences of i.
 */
object MagicSequence extends CPModel with App {
  //
  // data
  //
  val n = if (args.length > 0) args(0).toInt else 10
  //
  // variables
  //
  val x = Array.fill(n)(CPIntVar(0 to n - 1))
  val allValues = Array.tabulate(n)(i => (i, x(i)))
  //
  // constraints
  //
  add(gcc(x, allValues), Medium)

  search {
    binaryStaticIdx(x,i => x(i).min)
  }

  onSolution {
    println("\nSolution:")
    println("x: " + x.mkString(" "))
  }
  println(start())
}
