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
  Olympic puzzle in Oscar.
  Benchmark for Prolog (BProlog)
  """
  File   : olympic.pl
  Author : Neng-Fa ZHOU
  Date   : 1993
  Purpose:a puzzle taken from Olympic Arithmetic Contest
  Given ten variables with the following configuration:
                  X7   X8   X9   X10
                     X4   X5   X6
                        X2   X3
                           X1
  We already know that X1 is equal to 3 and want to assign each variable
  with a different integer from {1,2,...,10} such that for any three
  variables
                         Xi   Xj
                            Xk
  the following constraint is satisfied:
                       |Xi-Xj| = Xk
  """
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object Olympic extends CPModel with App  {
    def abs_minus(x: CPIntVar, y: CPIntVar, z: CPIntVar) : Constraint = z.eq((x-y).abs)
    //
    // data
    //
    val n = 10
    //
    // variables
    //
    val x = Array.fill(n)(CPIntVar(1 to n))
    val Array(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) = x
    //
    // constraints
    //
    var numSols = 0
  
     add(allDifferent(x), Strong)
     add(x1 === 3)
     add(abs_minus(x2, x3, x1))
     add(abs_minus(x4, x5, x2))
     add(abs_minus(x5, x6, x3))
     add(abs_minus(x7, x8, x4))
     add(abs_minus(x8, x9, x5))
     add(abs_minus(x9, x10, x6))
    search{
      binary(x, _.size, _.min)
    }
onSolution {
      println("x: " + x.mkString(""))
      numSols += 1
   }
   println(start())
  }
