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

package oscar.cbls.test.contraints

import oscar.cbls._
import oscar.cbls.modeling._

/**
 * Local Search for NQueens
 * Moves are operated by swapping variables
 */
object NQueensEasy1 extends CBLSModel with App{
  startWatch()

  val N = 20
  val range = 0 until N
  println("NQueens(" + N + ")")

  val rand = new scala.util.Random()

  // initial solution
  val init = rand.shuffle(range.toList).toArray

  val queens = Array.tabulate(N)(q => CBLSIntVar(init(q), range, "queen" + q))

  //alldiff on rows in enforced because we swap queens initially different
  c.add(allDiff(Array.tabulate(N)(q => queens(q) + q)))
  c.add(allDiff(Array.tabulate(N)(q => q - queens(q))))

  close()

  //this tabu search is a bit simplistic: does not use the invariants for maintaining Tabu...
  //and max violated queens might be all tabu

  var it = 0
  val tabu = Array.fill(N)(0)
  val tenure = 3

  while(violation.value > 0){
    selectMin(range,range)(
      (p,q) => swapVal(queens(p),queens(q)),
      (p,q) => tabu(p) < it && tabu(q) < it && p < q)
    match{
      case (q1,q2) =>
        queens(q1) :=: queens(q2)
        tabu(q1)= it + tenure
        tabu(q2) = it + tenure
      case _ => println("Warning: Tabu it too big compared to queens count")
    }
    it += 1
  }

  println(getWatchString)
  println(it)
  println(queens.mkString(","))
}
