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
 * Least diff problem in Oscar.
 *
 * Minimize the difference ABCDE - FGHIJ
 * where A..J are distinct digits (0..9).
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object LeastDiff extends CPModel with App  {
    // variables
    val A = CPIntVar(0 to 9)
    val B = CPIntVar(0 to 9)
    val C = CPIntVar(0 to 9)
    val D = CPIntVar(0 to 9)
    val E = CPIntVar(0 to 9)
    val F = CPIntVar(0 to 9)
    val G = CPIntVar(0 to 9)
    val H = CPIntVar(0 to 9)
    val I = CPIntVar(0 to 9)
    val J = CPIntVar(0 to 9)
    val all = Array(A, B, C, D, E, F, G, H, I, J)
    val X = A * 10000 + B * 1000 + C * 100 + D * 10 + E
    val Y = F * 10000 + G * 1000 + H * 100 + I * 10 + J
    val Diff = X - Y
    // constraints
   minimize(Diff) 
     add(allDifferent(all), Strong)
     add(A > 0)
     add(F > 0)
     add(Diff > 0)
    search{
      binary(all ++ Array(X, Y, Diff), -_.constraintDegree, _.min)
    }
onSolution {
      println(Array(A, B, C, D, E).mkString("") + " -" +
              Array(F, G, H, I, J).mkString("") + " =" +
              Diff)
    }
    println(start())
  }
