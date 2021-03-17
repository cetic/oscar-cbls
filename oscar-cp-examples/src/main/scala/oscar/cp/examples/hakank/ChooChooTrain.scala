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
 * CHOO+CHOO0=TRAIN problem in Oscar.
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object ChooChooTrain extends CPModel with App {
  // variables
  val C = CPIntVar(0 to 9)
  val H = CPIntVar(0 to 9)
  val O = CPIntVar(0 to 9)
  val T = CPIntVar(0 to 9)
  val R = CPIntVar(0 to 9)
  val A = CPIntVar(0 to 9)
  val I = CPIntVar(0 to 9)
  val N = CPIntVar(0 to 9)
  val all = Array(C, H, O, T, R, A, I, N)
  val all_str = Array("C", "H", "O", "T", "R", "A", "I", "N")
  // constraints
  // This give better pre-labeling results:
  // it find C: 5..9
  add((C * 1000 + H * 100 + O * 10 + O) * 2 === T * 10000 + R * 1000 + A * 100 + I * 10 + N)
  // than this, which just find C: 2..9
  //add(          C*1000 + H*100 + O*10 + O 
  //              +   C*1000 + H*100 + O*10 + O == 
  //        T*10000 + R*1000 + A*100 + I*10 + N)
  add(C !== 0)
  add(T !== 0)
  add(allDifferent(all), Strong)
  search {
    binaryFirstFail(all)
  }
  onSolution {
    println(all.mkString(""))
  }
  println("Before labeling: ")
  for (i <- 0 until all.length) {
    println("char: " + all_str(i) + ": " + all(i))
  }
  println()
  println(start())
}
