/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.examples.hakank

import oscar.cp._
/*
  Bowls and Oranges problem in Oscar.
  From BitTorrent Developer Challenge
  http://www.bittorrent.com/company/about/developer_challenge
  """
  You have 40 bowls, all placed in a line at exact intervals of 
  1 meter. You also have 9 oranges. You wish to place all the oranges 
  in the bowls, no more than one orange in each bowl, so that there are 
  no three oranges A, B, and C such that the distance between A and B is 
  equal to the distance between B and C. How many ways can you arrange 
  the oranges in the bowls?.
  """
  Via http://surana.wordpress.com/2011/06/01/constraint-programming-example/
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object BowlsAndOranges extends CPModel with App {
  // Data
  val n = 40
  val m = 9
  // Variables
  val x = Array.fill(m)(CPIntVar(1 to n))
  // Constraints
  add(allDifferent(x), Strong)
  // increasing x
  for (i <- 1 until m) {
    add(x(i - 1) < x(i))
  }
  for (i <- 0 until m) {
    for (j <- 0 until i) {
      for (k <- 0 until j) {
        add(x(j) - x(i) !== x(k) - x(j))
      }
    }
  }
  search { binaryMaxDegree(x) }
  onSolution { println(x.mkString("")) }
  println(start())
}
