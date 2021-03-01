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

import oscar.util.RandomGenerator
import oscar.dfo.utils.MOEvaluator
import oscar.dfo.utils.MOOPoint
import oscar.dfo.utils.FeasibleRegion

class DirectionalDirectSearchState(initPoint: MOOPoint, val stepSizes: Array[Double], var basisSize: Int, val stepSizeIntervals: Array[(Double, Double)]) extends ComparativeAlgorithmState {
  val newDirectionProportion = 0.42
  var bestPoint = initPoint
  val currentBasis = getRandomBasis
  
  def getBestPoint = bestPoint
  
  def getPoints = List(bestPoint)
  
  def getNewState(newBestPoint: MOOPoint): ComparativeAlgorithmState = {
    DirectionalDirectSearchState(newBestPoint, stepSizeIntervals.clone, stepSizes.clone, basisSize)
  }
  
  def promoteDirection(directionIndex: Int): Unit = {
    var formerDirection = currentBasis(directionIndex)
    for (i <- 0 to directionIndex) {
      val formerDir = currentBasis(i)
      currentBasis(i) = formerDirection
      formerDirection = formerDir
    }
  }
  
  def getNewPoint(directionIndex: Int, evaluator: MOEvaluator, feasibleReg: FeasibleRegion): MOOPoint = {
    val newCoordinates = Array.tabulate(bestPoint.nbCoordinates)(i => bestPoint.coordinates(i) + currentBasis(directionIndex)(i))
    evaluator.eval(newCoordinates, feasibleReg)
  }
  
  def updateBasis = {
    val nbNewDirections = (newDirectionProportion * basisSize).toInt
    for (i <- (basisSize - nbNewDirections - 1) until basisSize) {
      currentBasis(i) = getNewRandomDirection
    }
  }
  
  def getRandomBasis: Array[Array[Double]] = {
    Array.tabulate(basisSize)(i => getNewRandomDirection)
  }
  
  def normalizeArray(coordinates: Array[Double]): Array[Double] = {
    val vectorLength = math.sqrt(coordinates.foldLeft(0.0)((acc, newDim) => acc + newDim * newDim))
    Array.tabulate(coordinates.length)(i => coordinates(i) / vectorLength)
  }
  
  def getNewRandomDirection: Array[Double] = {
    normalizeArray(Array.tabulate(initPoint.nbCoordinates)(i => 0.5 - RandomGenerator.nextDouble))
  }
  
  def getSmallestStepSize = {
    stepSizes.foldLeft(Double.MaxValue)((acc, step) => if (step <= acc) step else acc)
  }
  
  def reinitialize = {
    for (i <- 0 until stepSizes.length) {
      stepSizes(i) = (stepSizeIntervals(i)._2 - stepSizeIntervals(i)._1) / 5.0
    }
    for (i <- 0 until basisSize) {
      currentBasis(i) = getNewRandomDirection
    }
  }
  
  def increaseStepSizes = {
    for (i <- 0 until stepSizes.length) {
      stepSizes(i) = stepSizes(i) * DirectionalDirectSearchState.increaseFactor
    }
  }
  def decreaseStepSizes = {
    for (i <- 0 until stepSizes.length) {
      stepSizes(i) = stepSizes(i) * DirectionalDirectSearchState.decreaseFactor
    }
  }
}

object DirectionalDirectSearchState {
  var increaseFactor = 2.0
  var decreaseFactor = 0.5
  
  def apply(initPoint: MOOPoint, stepSizeIntervals: Array[(Double, Double)], stepSizes: Array[Double], basisSize: Int) = new DirectionalDirectSearchState(initPoint, stepSizes, basisSize, stepSizeIntervals)
  
  def apply(initPoint: MOOPoint, stepSizeIntervals: Array[(Double, Double)]): ComparativeAlgorithmState = {
    val basisSize = 2 * initPoint.nbCoordinates
    val stepSizes = Array.tabulate(initPoint.nbCoordinates)(i => (stepSizeIntervals(i)._2 - stepSizeIntervals(i)._1) / 5.0)
    DirectionalDirectSearchState(initPoint, stepSizeIntervals, stepSizes, basisSize)
  }
}
