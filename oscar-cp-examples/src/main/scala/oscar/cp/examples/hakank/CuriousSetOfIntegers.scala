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
/**
 *
 * Problem from Martin Gardner (February 1967):
 * """
 * The integers 1,3,8, and 120 form a set with a remarkable property: the
 * product of any two integers is one less than a perfect square. Find
 * a fifth number that can be added to the set without destroying
 * this property.
 * """
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object CuriousSetOfIntegers extends CPModel with App {

  def increasing(y: Array[CPIntVar]) = {
    for (i <- 1 until y.length) {
      add(y(i - 1) <= y(i))
    }
  }

  //
  // data
  // 
  val n = 5
  val max_val = 10000
  //
  // decision variables
  // 
  val x = Array.fill(n)(CPIntVar(0 to max_val))
  var numSols = 0

  add(allDifferent(x), Strong)
  for (i <- 0 until n - 1) {
    for (j <- i + 1 until n) {
      val p = CPIntVar(0 to max_val)
      add((p * p - 1) === x(i) * x(j))
    }
  }
  // Symmetry breaking
  increasing(x)
  // This is the original problem:
  // The given numbers are {1,3,8,120},
  // Which is the fifth number?
  add(
    ((x(0) ?< 1) && (x(1) ?=== 1) && (x(2) ?=== 3) && (x(3) ?=== 8) && (x(4) ?=== 120))
      ||
      ((x(0) ?=== 1) && (x(1) ?=== 3) && (x(2) ?=== 8) && (x(3) ?=== 120) && (x(4) ?> 120))
  )
  search {
    binaryStatic(x)
  }
  onSolution {
    println(x.mkString(""))
    val s = Set(1, 3, 8, 120)
    for (i <- 0 until n) {
      val v = x(i).value
      if (!s.contains(v)) {
        println("The fifth number is " + v)
      }
    }
    numSols += 1
  }
  println(start())
}
