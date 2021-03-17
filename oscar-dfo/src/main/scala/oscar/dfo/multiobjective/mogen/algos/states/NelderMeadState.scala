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
package oscar.dfo.multiobjective.mogen.algos.states

import oscar.dfo.utils.MOOPoint
import oscar.dfo.utils.MOEvaluator
import oscar.dfo.utils.FeasibleRegion
import oscar.util.RandomGenerator

class NelderMeadState(simplexInit: Array[MOOPoint], val startIntervals: Array[(Double, Double)]) extends ComparativeAlgorithmState with Simplex[Double] {
  val simplex = simplexInit
  var bestPoint = simplex(0)

  var deltaR = 1
  var deltaE = 2
  var deltaOC = 0.5
  var deltaIC = -0.5
  var gammaS = 0.5
  
  def getNewState(newBestPoint: MOOPoint): ComparativeAlgorithmState = {
    val newState = NelderMeadState(simplex, startIntervals)
    newState.deltaR = this.deltaR
    newState.deltaE = this.deltaE
    newState.deltaOC = this.deltaOC
    newState.deltaIC = this.deltaIC
    newState.gammaS = this.gammaS
    newState.bestPoint = newBestPoint
    orderSimplex()
    newState
  }
  
  def getReflection(evaluator: MOEvaluator, feasibleReg: FeasibleRegion, centroid: Array[Double] = getCentroid): MOOPoint = getSinglePointTransformation(centroid, deltaR, evaluator, feasibleReg)
  
  def getExpansion(evaluator: MOEvaluator, feasibleReg: FeasibleRegion, centroid: Array[Double] = getCentroid): MOOPoint = getSinglePointTransformation(centroid, deltaE, evaluator, feasibleReg)
  
  def getInsideContraction(evaluator: MOEvaluator, feasibleReg: FeasibleRegion, centroid: Array[Double] = getCentroid): MOOPoint = getSinglePointTransformation(centroid, deltaIC, evaluator, feasibleReg)
  
  def getOutsideContraction(evaluator: MOEvaluator, feasibleReg: FeasibleRegion, centroid: Array[Double] = getCentroid): MOOPoint = getSinglePointTransformation(centroid, deltaOC, evaluator, feasibleReg)
  
  def getSinglePointTransformation(centroid: Array[Double], factor: Double, evaluator: MOEvaluator, feasibleReg: FeasibleRegion): MOOPoint = {
    val newCoordinates = arraySum(centroid, arrayProd(arrayDiff(centroid, worstPoint.coordinates), factor))
    evaluator.eval(newCoordinates, feasibleReg)
  }
  
  def applySinglePointTransformation(newPoint: MOOPoint) = {
    simplex(simplexSize - 1) = newPoint
    orderSimplex()
  }
  
  def getCentroid: Array[Double] = {
    val allButWorstCoordinates = simplex.map(mooP => mooP.coordinates).take(simplexSize - 1)
    arrayProd(allButWorstCoordinates.drop(1).foldLeft(allButWorstCoordinates(0))((acc, newCoords) => arraySum(acc, newCoords)), 1.0 / (simplexSize - 1))
  }
  
  def applyShrink(evaluator: MOEvaluator, feasibleReg: FeasibleRegion) = {
    val simplexCoordinates = simplex.map(mooP => mooP.coordinates)
    for (i <- 1 until simplexSize) {
      simplex(i) = evaluator.eval(arrayProd(simplexCoordinates(i), gammaS), feasibleReg)
    }
    orderSimplex()
  }
  
  def printSimplex = {
    println("=" * 80)
    for (i <- 0 until simplexSize)
      println(s"$i: ${simplex(i)}")
  }
  
}

object NelderMeadState {
  def apply(simplex: Array[MOOPoint], startIntervals: Array[(Double, Double)]) = new NelderMeadState(simplex, startIntervals)
  
  def apply(coordinates: Array[Double], startIntervals: Array[(Double, Double)], evaluator: MOEvaluator, feasReg: FeasibleRegion): ComparativeAlgorithmState = {
    val simplex = Array.tabulate(coordinates.length + 1){ index =>
      if (index == 0) coordinates
      else {
        val randPerturbation = startIntervals.map(e => (0.5 - RandomGenerator.nextDouble) * math.abs(e._2 - e._1) * 0.05)
        Array.tabulate(coordinates.length)(i => coordinates(i) + randPerturbation(i))
      }
    }
    NelderMeadState(simplex.map(coord => evaluator.eval(coord, feasReg)), startIntervals)
  }
}
