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
package oscar.cp.examples.hakank

import oscar.cp._
import scala.io.Source._
import scala.math._
/*
  Coins puzzle in Oscar.
  From "Constraint Logic Programming using ECLiPSe"
  pages 99f and 234 ff.
  The solution in ECLiPSe is at page 236.
  """
  What is the minimum number of coins that allows one to pay _exactly_
  any amount smaller than one Euro? Recall that there are six different
  euro cents, of denomination 1, 2, 5, 10, 20, 50
  """
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object Coins3 extends CPModel with App {
  //
  // data
  //
  val n = 6 // number of different coins
  val variables = Array(1, 2, 5, 10, 25, 50)
  //
  // variables
  //
  val x = Array.fill(n)(CPIntVar(0 to 99))
  val num_coins = sum(x)
  //
  // constraints
  //
  minimize(num_coins)
  // Check that all changes from 1 to 99 can be made.
  for (j <- 1 until 100) {
    val tmp = Array.fill(n)(CPIntVar(0 to 99))
    add(weightedSum(variables, tmp) === j)
    for (i <- 0 until n) {
      add(tmp(i) <= x(i))
    }
  }
  search {
    binaryStatic(x)
  }
  onSolution {
    println("\nSolution:")
    println("num_coins : " + num_coins)
    println("x: " + x.mkString(""))
    println()
  }
  println(start())
}
