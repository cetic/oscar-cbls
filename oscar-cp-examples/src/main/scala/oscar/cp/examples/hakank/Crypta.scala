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
/**
 *
 * Cryptarithmetic puzzle in Oscar.
 * 
 * Prolog benchmark problem GNU Prolog (crypta.pl)
 * """
 * Name           : crypta.pl
 * Title          : crypt-arithmetic
 * Original Source: P. Van Hentenryck's book
 * Adapted by     : Daniel Diaz - INRIA France
 * Date           : September 1992
 *
 * Solve the operation:
 *
 *    B A I J J A J I I A H F C F E B B J E A
 *  + D H F G A B C D I D B I F F A G F E J E
 *  -----------------------------------------
 *  = G J E G A C D D H F A F J B F I H E E F
 * """
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Crypta extends CPModel with App  {
    //
    // data
    //
    val n = 10
    //
    // variables
    //
    val x = Array.fill(n)(CPIntVar(0 to 9))
    val Array(a,b,c,d,e,f,g,h,i,j) = x
    val sr1 = CPIntVar(0 to 1)
    val sr2 = CPIntVar(0 to 1)
    //
    // constraints
    //
  
       add(allDifferent(x), Strong)
       add(b >= 1)
       add(d >= 1)
       add(g >= 1)
       add((a + e*10 + j*100 + b*1000 + b*10000 + e*100000 + f*1000000 + 
                e + j*10 + e*100 + f*1000 + g*10000 + a*100000 + f*1000000) ===
               (f + e*10 + e*100 + h*1000 + i*10000 + f*100000 + b*1000000 + sr1*10000000))
       add((c + f*10 + h*100 + a*1000 + i*10000 + i*100000 + j*1000000 + 
                f + i*10 + b*100 + d*1000 + i*10000 + d*100000 + c*1000000 + sr1) ===
               (j + f*10 + a*100 + f*1000 + h*10000 + d*100000 + d*1000000 + sr2*10000000))
       add((a + j*10 + j*100 + i*1000 + a*10000 + b*100000 + 
                b + a*10 + g*100 + f*1000 + h*10000 + d*100000 + sr2) ===
               (c + a*10 + g*100 + e*1000 + j*10000 + g*100000))
    search{
      binaryFirstFail(x)
    }
onSolution {
      println("\nSolution:")
      println("x:" + x.mkString(""))
    } 
    println(start())
  }
