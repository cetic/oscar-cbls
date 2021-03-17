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
 * Relaxed version of stable mariage
 * @author Pierre Schaus pschaus@gmail.com
 */
object StableMariage {

  def main(args: Array[String]): Unit = {

    val n = 9

    val Women = 0 until n
    val Men = 0 until n

    // for each man, what is his ranking for the women (higher is better)
    val rankWomen = Array(Array(1, 7, 6, 2, 4, 8, 3, 5, 0),
      Array(1, 3, 6, 2, 7, 5, 0, 4, 8),
      Array(5, 0, 4, 1, 8, 6, 3, 7, 2),
      Array(4, 2, 7, 1, 6, 5, 3, 0),
      Array(0, 4, 5, 7, 6, 8, 3, 2, 1),
      Array(5, 1, 6, 3, 7, 0, 4, 2),
      Array(3, 2, 1, 0, 7, 5, 4, 8, 6),
      Array(8, 5, 3, 0, 4, 6, 7, 2, 1),
      Array(5, 8, 0, 4, 1, 6, 3, 7, 2))

    // for each woman, what is her ranking for the men (higher is better)			
    val rankMen = Array(Array(0, 1, 3, 5, 6, 2, 7, 8, 4),
      Array(2, 3, 6, 0, 4, 8, 5, 7, 1),
      Array(3, 2, 5, 8, 7, 4, 1, 6, 0),
      Array(5, 0, 7, 4, 1, 6, 3, 2),
      Array(4, 1, 2, 5, 0, 7, 8, 3, 6),
      Array(3, 2, 0, 8, 7, 4, 1, 6, 5),
      Array(3, 7, 4, 2, 8, 0, 1, 6, 5),
      Array(3, 2, 0, 4, 1, 8, 7, 5, 6),
      Array(4, 2, 8, 1, 7, 6, 5, 3, 0))

    val cp = CPSolver()

    val wife = Array.fill(n)(CPIntVar(cp, Women)) // wife(i) is the woman chosen for man i
    val husband = Array.fill(n)(CPIntVar(cp, Men)) // husband(j) is the man chosen for woman j
    var nbSol = 0

    for (m <- Men) {
      cp.add(elementVar(husband, wife(m), m), Strong)
    }
    for (w <- Women) {
      cp.add(elementVar(wife, husband(w), w), Strong)
    }

    for (m <- Men; w <- Women) {
      val pref_m = element(rankMen(m), wife(m), Weak) // preference of m for his wife
      val pref_w = element(rankWomen(w), husband(w), Weak) // preference of w for her husband     
    }
    cp.search {
      binaryFirstFail(wife)
    }
    println(cp.start())

  }

}
