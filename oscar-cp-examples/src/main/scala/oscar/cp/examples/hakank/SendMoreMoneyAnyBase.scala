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
import scala.math._
/**
  SEND+MORE=MONEY in "any" base in Oscar.
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 */
object SendMoreMoneyAnyBase extends CPModel with App  {
    var base = 10
    if (args.length > 0) {
      base = args(0).toInt
    }
    println("Base: " + base)
    val base2 = base - 1
    val b1 = base
    val b2 = pow(base, 2).toInt
    val b3 = pow(base, 3).toInt
    val b4 = pow(base, 4).toInt
    //
    // variables
    //
    val S = CPIntVar(0 to base2)
    val E = CPIntVar(0 to base2)
    val N = CPIntVar(0 to base2)
    val D = CPIntVar(0 to base2)
    val M = CPIntVar(0 to base2)
    val O = CPIntVar(0 to base2)
    val R = CPIntVar(0 to base2)
    val Y = CPIntVar(0 to base2)
    val all = Array(S,E,N,D,M,O,R,Y)
    //
    // constraints
    //
    var numSols = 0
  
       add(allDifferent(all), Strong)
       add(       S*b3 + E*b2 + N*b1 + D +
                      M*b3 + O*b2 + R*b1 + E ===
               M*b4 + O*b3 + N*b2 + E*b1 + Y
             )
       add(S > 0)
       add(M > 0)
     search{
       binaryFirstFail(all)
     }
onSolution {
       println(all.mkString(""))
       numSols += 1
     } 
     println(start())
   }
