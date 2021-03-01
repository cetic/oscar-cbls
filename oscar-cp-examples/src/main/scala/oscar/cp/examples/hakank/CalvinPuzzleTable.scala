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
 * *****************************************************************************/
package oscar.cp.examples.hakank

import oscar.cp._
import oscar.cp.constraints._
import scala.jdk.CollectionConverters._
import scala.math._
/**
 *
 * Calvin puzzle in Oscar.
 *
 * From "An Exercise for the Mind: A 10 by 10 Math Puzzle:
 *       A Pattern Recognition Game: Meditation on an Open Maze"
 * http://www.chycho.com/?q=Puzzle
 * """
 * The Purpose of the Game
 *
 * To take a 10 by 10 grid, representing 100 squares, and completely fill every square
 * based on two types of movements.
 * ...
 *
 * Movement Type I)  If the next number in the sequence is going to be placed vertically
 * or horizontally, then it must be placed exactly three squares away from the previous
 * number (there must be a two square gap between the numbers).
 *
 * Movement Type II) If the next number in the sequence is going to be placed diagonally,
 * then it must be placed exactly two squares away from the previous number (there must
 * be a one square gap between the numbers).
 * """
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object CalvinPuzzleTable extends CPModel with App {
  //
  // data
  //     
  val n = if (args.length > 0) args(0).toInt else 6;
  val num_to_show = if (args.length > 1) args(1).toInt else 1;
  println("n: " + n + " num_to_show: " + num_to_show)
  val RANGE = 0 until n
  // create the valid tuples
  val s = Set(-3, -2, 0, 2, 3)
  val valid =
    (for {
      i <- 0 until n;
      j <- 0 until n;
      a <- s;
      b <- s
      if (i + a >= 0 &&
        i + a < n &&
        j + b >= 0 &&
        j + b < n &&
        (
          (abs(a) == 2 && abs(b) == 2)
          ||
          (abs(a) == 3 && b == 0)
          ||
          (abs(b) == 3 && a == 0)
        )
      )
    } yield Array(i * n + j, (i + a) * n + (j + b))
    ).toArray;
  //
  // variables
  //
  val x = Array.fill(n, n)(CPIntVar(0 to n * n - 1))
  val x_flat = x.flatten
  //
  // constraints
  //
  var numSols = 0;
  // place all integers from 0..n*n-1
  add(allDifferent(x_flat), Strong)
  for (k <- 0 until n * n - 1) {
    add(table(Array(x_flat(k), x_flat(k + 1)), valid))
  }
  // symmetry breaking
  add(x(0)(0) === 0)
  search {
    binary(x_flat, -_.constraintDegree, _.randomValue)
  }
  onSolution {
    println("Solution:")
    // Now, get the values of the matrix
    var y = Array.tabulate(n)(i => Array.tabulate(n)(j => 0))
    for (k <- 0 until n * n) {
      val pos = x_flat(k).value
      y(pos / n)(pos % n) = k
    }
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        print("%4d".format(y(i)(j)))
      }
      println()
    }
    println()
  }
  println(start(nSols = num_to_show))
}
