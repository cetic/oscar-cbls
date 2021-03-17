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
  Sequence sum in Oscar.
  Sum of each sequence in s-slices in an array of n elements should be m.
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 */
object SequenceSum extends CPModel with App  {
  // Sum the elements in y where each subsequence of length s
  // sums to m
  def sequence_sum(y: Array[CPIntVar], m: CPIntVar, s: Int) = {
    val n = y.length
    for(i <- 0 until n - s + 1) {
     add(sum( Range(i,i+s).map(j => y(j) ).toList) === m)
    }
  }
    val n = 6
    // val m = 10 // the sum
    val s = 3 // the sliding size
    // variables
    val x = Array.fill(n)(CPIntVar(1 to n))
    // the sum
    val m = CPIntVar(1 to n*n)
    //
    // constraints
    //
    var numSols = 0
  
      sequence_sum(x, m, s)
     add(m === 10)
      // symmetry breaking
      //add(x(0) == 1)
    search{
      binaryFirstFail(x)
    }
onSolution {  
      print("x: " + x.mkString(""))
      println("  m: " + m)
      numSols += 1
    } 
    println(start())
   }
