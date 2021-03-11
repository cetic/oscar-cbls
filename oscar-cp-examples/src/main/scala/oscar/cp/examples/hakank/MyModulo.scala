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
  A simple modulo decomposition in Oscar.
  As of writing, OscaR supports these two versions
  of modulo
    modulo(CPIntVar, Int, CPIntVar) : Constraint
    modulo(CPIntVar, Int, Int) : Constraint
  but not
    modulo(CPIntVar, CPIntVar, CPIntVar)
  here is an implementation (decomposition)
  of the latter version.
  This implementation is based on the ECLiPSe version
  mentioned in "A Modulo propagator for ECLiPSE"
  http://www.hakank.org/constraint_programming_blog/2010/05/a_modulo_propagator_for_eclips.html
  The ECLiPSe Prolog source code:
  http://www.hakank.org/eclipse/modulo_propagator.ecl
  Which in turn is inspired by MiniZinc's bounds consistency 
  predicate in
  http://www.g12.cs.mu.oz.au/mzn/div_mod/div_mod.mzn    
  A variant is implemented in the or-tools/C# model
  http://www.hakank.org/or-tools/divisible_by_9_through_1.cs
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object MyModulo extends CPModel with App {
  def mod(
    x: CPIntVar,
    y: CPIntVar): CPIntVar = {
    val mmin = min(x.min, y.min)
    val mmax = min(x.max, y.max)
    val r = CPIntVar(mmin, mmax)
    myMod(x, y, r)
    r
  }
  //
  // decomposition of modulo constraint:
  //    x % y == r
  //
  def myMod(
    x: CPIntVar,
    y: CPIntVar,
    r: CPIntVar) = {
    val lbx = x.min
    val ubx = x.max;
    val ubx_neg = -ubx;
    val lbx_neg = -lbx;
    val min_x = min(lbx, ubx_neg);
    val max_x = max(ubx, lbx_neg);
    val d = CPIntVar(min_x, max_x)
    // r >= 0
    add(r >= 0)
    // x*r >= 0
    add(x * r >= 0)
    // -abs(y) < r
    add(-y.abs < r)
    // r < abs(y)
    add(r < y.abs)
    // min_x <= d, i.e. d > min_x
    add(d > min_x)
    // d <= max_x
    add(d <= max_x)
    // x == y*d+r
    add(x - (y * d + r) === 0)
  }
  //
  // data
  //
  val n = 5
  //
  // variables
  //
  val x = Array.fill(n)(CPIntVar(0 to 9))
  val two = CPIntVar(2 to 2)
  val zero = CPIntVar(0 to 0)
  val one = CPIntVar(1 to 1)
  //
  // constraints
  //
  var numSols = 0

  for (i <- 1 until n) {
    // myMod(cp, x(i), x(i-1), one)
    add(mod(x(i), x(i - 1)) === one)
    //add(modulo(x(i), x(i-1)), 1)
  }
  search {
    binaryMaxDegree(x)
  }
  onSolution {
    println(x.mkString(" "))
    numSols += 1
  }
  println(start())
}
