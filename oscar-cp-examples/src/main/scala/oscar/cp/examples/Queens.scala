package oscar.cp.examples

import oscar.cp._
/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * this program search for all the solutions
 * Using Non Deterministic Search
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object Queens extends CPModel with App {

  val nQueens = 60 // Number of queens
  val Queens = 0 until nQueens

  // Variables
  val queens = Array.fill(nQueens)(CPIntVar.sparse(0, nQueens - 1))

  // Constraints
/*
  add(new AllDiffFWC(queens))
  add(new AllDiffFWC(Queens.map(i => queens(i) + i)))
  add(new AllDiffFWC(Queens.map(i => queens(i) - i)))
*/
/*
  add(allDifferent(queens),Weak)
  add(allDifferent(Queens.map(i => queens(i) + i)),Weak)
  add(allDifferent(Queens.map(i => queens(i) - i)),Weak)
*/

  // Variables
  val rQueens = Array.tabulate(nQueens)(i => queens(i) + i)
  val fQueens = Array.tabulate(nQueens)(i => queens(i) - i)


  // Constraints

  for (i <- Queens; j <- Queens; if j < i) {
    add(queens(i) !== queens(j))
    add(rQueens(i) !== rQueens(j))
    add(fQueens(i) !== fQueens(j))
  }

  // Search heuristic
  search(binaryFirstFail(queens))

  search(binaryIdx(queens,minDom(queens),minVal(queens)))


  // Execution
  val stats = start(100000)//
  println(stats)
}
