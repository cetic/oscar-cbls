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
  Crypto in Oscar.
  Standard alphametic problem in mathematical recreations, 
  constraint programming etc.
  This is an alternative approach compared to Crypto.scala.
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
*/
object Crypto2 extends CPModel with App  {
  // sums(all, "ballet", 45, ht)
  def sums(x: Array[CPIntVar], str: String, s: Int, m: Map[Char,Int]) =
    sum( for(v <- str.toCharArray.map(m(_))) yield(x(v)) ) === s
    //
    // data
    //
    val num = 26
    val ht = ('a' to 'z').zipWithIndex.toMap // char -> index
    //
    // variables
    //
    val all = Array.fill(num)(CPIntVar(1 to num))
    val Array(a,b,c,d,e,f,g,h,i,j,k,l,m) = all slice( 0, 13)
    val Array(n,o,p,q,r,s,t,u,v,w,x,y,z) = all slice(13, 26)
    //
    // constraints
    //
  
     add(allDifferent(all), Strong)
     add(sums(all, "ballet",     45, ht))
     add(sums(all, "cello",      43, ht))
     add(sums(all, "concert",    74, ht))
     add(sums(all, "flute",      30, ht))
     add(sums(all, "fugue",      50, ht))
     add(sums(all, "glee",       66, ht))
     add(sums(all, "jazz",       58, ht))
     add(sums(all, "lyre",       47, ht))
     add(sums(all, "oboe",       53, ht))
     add(sums(all, "opera",      65, ht))
     add(sums(all, "polka",      59, ht))
     add(sums(all, "quartet",    50, ht))
     add(sums(all, "saxophone", 134, ht))
     add(sums(all, "scale",      51, ht))
     add(sums(all, "solo",       37, ht))
     add(sums(all, "song",       61, ht))
     add(sums(all, "soprano",    82, ht))
     add(sums(all, "theme",      72, ht))
     add(sums(all, "violin",    100, ht))
     add(sums(all, "waltz",      34, ht))
    search{
      binaryMaxDegree(all)
    }
onSolution {
      println("all:" + all.mkString(""))
    } 
    println(start())
  }
