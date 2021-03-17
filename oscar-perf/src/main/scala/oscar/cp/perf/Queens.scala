/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

package oscar.cp.perf

import oscar.cp._

/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * this program search for all the solutions
 * Using Non Deterministic Search
 * @author Pierre Schaus pschaus@gmail.com
 */
object Queens {
  def main(args: Array[String]): Unit = {

    val cp = CPSolver()
    cp.silent = true
    val n = 12 //number of queens
    val Queens = 0 until n
    //variables
    val queens = for (i <- Queens) yield CPIntVar(cp, 1 to n)

    var nbsol = 0
      cp.add(allDifferent(queens), Strong)
      cp.add(allDifferent(for (i <- Queens) yield queens(i) + i), Strong)
      cp.add(allDifferent(for (i <- Queens) yield queens(i) - i), Strong)
    cp.search {
      queens.find(!_.isBound) match {
        case None => noAlternative
        case Some(x) => branchAll(1 to n)(v => cp.add(x === v))
      }
    }
    println(cp.start())

  }
}


