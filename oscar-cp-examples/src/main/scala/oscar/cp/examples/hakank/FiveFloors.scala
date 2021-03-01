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
  Five floors problem in Oscar
  From Alexey Radul & Gerald Jay Sussman: 
  "The Art of Propagator", page 34
  """
  Baker, Cooper, Fletcher, Miller, and Smith live on the first
  five floors of this apartment house. Baker does not live on the
  fifth floor. Cooper does not live on the first floor. Fletcher
  does not live on either the fifth or the first floor. Miller lives
  on a higher floor than does Cooper. Smith does not live on a
  floor adjacent to Fletcher'. Fletcher does not live on a floor
  adjacent to Cooper's.
  """
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 */
object FiveFloors extends CPModel with App  {
    //
    // data
    //
    var n = 5
    //
    // decision variables
    //
    val x = Array.fill(n)(CPIntVar(1 to n))
    val Array(baker, cooper, fletcher, miller, smith) = x
    //
    // constraints
    //
    var numSols = 0
  
      add(allDifferent(x), Strong)
       // Baker does not live on the fifth floor.
      add(baker !== 5)
       // Cooper does not live on the first floor. 
      add(cooper !== 1)
       // Fletcher does not live on either the fifth or the first floor. 
      add(fletcher !== 5)
      add(fletcher !== 1)
       // Miller lives on a higher floor than does Cooper. 
      add(miller > cooper)
       // Smith does not live on a floor adjacent to Fletcher'. 
      add((smith-fletcher).abs > 1)
       // Fletcher does not live on a floor adjacent to Cooper's.
      add((fletcher-cooper).abs > 1)
     search{
       binaryMaxDegree(x)
     }
onSolution {
       println(x.mkString(""))
       numSols += 1
     } 
     println(start())
   }
