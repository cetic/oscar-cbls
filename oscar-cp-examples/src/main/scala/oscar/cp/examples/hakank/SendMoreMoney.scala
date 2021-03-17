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
 * SEND+MORE MONEY problem in Oscar.
 * 
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object SendMoreMoney extends CPModel with App  {
      // variables
      val S = CPIntVar(0 to 9)
      val E = CPIntVar(0 to 9)
      val N = CPIntVar(0 to 9)
      val D = CPIntVar(0 to 9)
      val M = CPIntVar(0 to 9)
      val O = CPIntVar(0 to 9)
      val R = CPIntVar(0 to 9)
      val Y = CPIntVar(0 to 9)
      val all = Array(S,E,N,D,M,O,R,Y)
    
        // constraints
       add(       S*1000 + E*100 + N*10 + D +
                      M*1000 + O*100 + R*10 + E ===
            M*10000 + O*1000 + N*100 + E*10 + Y)
       add(S > 0)
       add(M > 0)
       add(allDifferent(all), Strong)
      search{
         binaryFirstFail(all)
      }
onSolution {
         println(all.mkString(""))
      } 
      println(start())
  }
