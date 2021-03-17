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
 * @author Pierre Schaus pschaus@gmail.com
 */
object QueensJCBC {
  def main(args: Array[String]): Unit = {

    val cp = CPSolver()
    cp.silent = true
    val n = 88 //number of queens
    val Queens = 0 until n
    //variables
    val queens = for (i <- Queens) yield CPIntVar.sparse(0, n-1)(cp)

    var nbsol = 0

    val cl = Medium
    cp.add(allDifferent(queens), cl)
    cp.add(allDifferent(for (i <- Queens) yield queens(i) + i), cl)
    cp.add(allDifferent(for (i <- Queens) yield queens(i) - i), cl)
    
    cp.search {
      binaryFirstFail(queens)
    }
    
    println(cp.start(nSols=1))

  }
}


