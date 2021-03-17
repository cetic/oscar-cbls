package oscar.cp.examples

import oscar.algo.search.DFSLinearizer
import oscar.cp._

/**
  * @author Sascha Van Cauwelart
  * @author Pierre Schaus pschaus@gmail.com
  */
object QueensReplay extends  App {

  implicit val cp = CPSolver()

  val nQueens = 30
  // Number of queens
  val Queens = 0 until nQueens

  // Variables
  val queens = Array.fill(nQueens)(CPIntVar.sparse(0, nQueens - 1))

  // Search heuristic
  search(binaryFirstFail(queens))

  val allDiffs = Seq(allDifferent(queens),
    allDifferent(Queens.map(i => queens(i) + i)),
    allDifferent(Queens.map(i => queens(i) - i)))

  val linearizer = new DFSLinearizer()

//  onSolution(println(queens.mkString(",")))

  // Execution with FC allDifferent
  val statsInit = startSubjectTo(searchListener = linearizer, timeLimit = 5) {
    add(allDiffs,Weak)
  }

  println(statsInit)

  // Replay with AC allDifferent
  val statsReplayAC = cp.replaySubjectTo(linearizer, queens, timeLimit = 20) {
    add(allDiffs,Strong)
  }

  println(statsReplayAC)



}
