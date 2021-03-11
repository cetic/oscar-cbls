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
package oscar.examples.dfo

import oscar.dfo.singleobjective.algos.NelderMead
import oscar.util.Interval

/**
 * author James Thompson
 */
object DataFit extends App {

  // Approximately normally distributed data separated by 1.0 in the x-dimension here
  // Array of (x,y) tuples (as Doubles)
  val data = Array[(Double, Double)](
    (0.0, 1.69329),
    (1.0, 2.8325),
    (2.0, 2.94977),
    (3.0, 4.51972),
    (4.0, 4.37691),
    (5.0, 3.999),
    (6.0, 3.48955),
    (7.0, 2.96215),
    (8.0, 2.50601),
    (9.0, 2.16529))

  // Primitive cost function which calculates the sum of the squares of the residuals of the data - fitting function(params).
  def costFunction(params: Array[Double]): Array[Double] = Array(data.map(p => sqr(p._2 - gauss(p._1, params(0), params(1), params(2), params(3)))).sum)

  // Fitting function f(x) = ...
  // Input x, and fitting parameters specified below
  // Called by cost function
  def gauss(x: Double, y0: Double, a: Double, x0: Double, width: Double): Double = y0 + a * math.exp(-sqr((x - x0) / width))

  // Utility function
  def sqr(d: Double) = d * d

  def singleObjCompare(a1: Array[Double], a2: Array[Double]): Boolean = a1(0) < a2(0)

  // Fitting parameters
  val rangeLimits = Array.fill(4)(Interval(0, 10.0)) // Four fitting params all ranges here equal to 0.0 -> 10.0
  val params = Array(1.0, 1.0, 1.0, 1.0) // y0, a, x0, width  INITIAL PARAMS

  // Nelder-Mead Object
  val nm = NelderMead(costFunction, 1, singleObjCompare, params, rangeLimits)

  // Evaluate method
  val nmAns = nm.sampledOptimize(100, math.pow(10, -3))

  println("Nelder-Mead: objective:" + nmAns._2(0) + " coordinates:" + (nmAns._1).mkString(",") + "\nAverage number of evaluations: " + nm.evalCount / 200.0)

}

