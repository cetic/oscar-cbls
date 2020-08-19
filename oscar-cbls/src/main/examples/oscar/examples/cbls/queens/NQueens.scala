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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/

package oscar.examples.cbls.queens

import oscar.cbls._
import oscar.cbls.modeling.CBLSModel
import oscar.cbls.util.StopWatch

import scala.util.Random

/** Local Search for NQueens
 *  Moves are operated by swapping variables, using a standard neighborhood
 */
object NQueens extends CBLSModel with App with StopWatch{

  startWatch()
  val N = 1000

  println("NQueensEasy(" + N + ")")
  val range:Range = Range(0,N)

  val init = Random.shuffle(range.toList).iterator
  val queens = Array.tabulate(N)((q:Int) => CBLSIntVar(init.next(), 0 until N, "queen" + q))

  //c.post(AllDiff(Queens)) //enforced because we swap queens and they are always allDiff
  post(allDiff( for (q <- range) yield queens(q) + q) )
  post(allDiff( for (q <- range) yield q - queens(q)) )

  val maxViolQueens = argMax(violations(queens)).setName("most violated queens")

  close()

  println("close: " + this.getWatch + " [ms]")
  val neighborhood =
    swapsNeighborhood(queens, "SwapQueens",
      searchZone2 = () => {val v = maxViolQueens.value; (_,_) => v}, //much faster than using searchZone1, since hotRestart is on Zone1, and not on zone2, and would be log(n)
      symmetryCanBeBrokenOnIndices = false) //since one search zone has been specified, and the other is the full range

  val it = neighborhood.doAllMoves(_ >= N || c.violation.value == 0, c)

  println("total: " + this.getWatch + " [ms]")

  println("it: " + it)
  println(c.violation)
  println(queens.mkString(","))

  println(s.stats)
}
