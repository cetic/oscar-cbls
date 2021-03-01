/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.examples.hakank

import oscar.cp._
/**
 * Grocery problem in Oscar.
 * From  Christian Schulte, Gert Smolka, Finite Domain
 * http://www.mozart-oz.org/documentation/fdt/
 * Constraint Programming in Oz. A Tutorial. 2001.
 * """
 * A kid goes into a grocery store and buys four items. The cashier
 * charges $7.11, the kid pays and is about to leave when the cashier
 * calls the kid back, and says "Hold on, I multiplied the four items
 * instead of adding them; I'll try again; Hah, with adding them the
 * price still comes to $7.11''. What were the prices of the four items?
 * """
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 */
object Grocery extends CPModel with App {
  def increasing(y: Array[CPIntVar]) = {
    for (i <- 1 until y.length) {
      add(y(i - 1) <= y(i), Strong)
    }
  }
  val n = 4
  val m = 711
  val m2 = m * 100 * 100 * 100
  println("m2: " + m2)
  // variables
  val item = Array.fill(n)(CPIntVar(1 to (m / 2).toInt))
  var numSols = 0

  add(allDifferent(item), Strong)
  increasing(item)
  add(sum(item) === m)
  // Note: yields overflow (though the solution is correct)
  // Alternative solutions:
  //add(item(0)*item(1)*item(2)*item(3) == m2) // this is slightly better
  //add(item(0).mul(item(1)).mul(item(2)).mul(item(3)) == m2)
  add(item.reduceLeft((acc, x) => acc * x) === m2, Strong)

  onSolution {
    binaryFirstFail(item)
  }
  onSolution {
    println(item.mkString(""))
    println()
    numSols += 1
  }
  println(start())
}
      
      
