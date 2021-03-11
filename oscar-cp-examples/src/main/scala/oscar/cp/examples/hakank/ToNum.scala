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
 *
 * ToNum in Oscar.
 *
 * Channelling between an array of variables and a variable (number).
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object ToNum extends CPModel with App  {
  // channeling between IntVar array t <=> IntVar s
  def toNum(t: Array[CPIntVar], base: Int=10) = sum(
      Array.tabulate(t.length)(i=> t(i)*pow(base, t.length-i-1).toInt))
      val n = 4
      val base = 10
      // variables
      val x = Array.tabulate(n)(i => CPIntVar(0 to base-1))
      val y = CPIntVar(0 to pow(n, base).toInt)
      var numSols = 0
    
       add(y === toNum(x))
        //add(y == 2143)
      search{
        binaryFirstFail(x)
      }
onSolution {
        println("x:" + x.mkString("") + "  y:" + y) 
        numSols += 1     
      } 
      println(start()) 
  }
