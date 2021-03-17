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
package oscar.dfo.multiobjective.evolutionary

import oscar.algo.paretofront.ParetoFront
import oscar.dfo.utils.{MOEvaluator, MOOPoint}

import scala.collection.mutable.HashMap
import scala.math.Ordering.Double.TotalOrdering

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class SPEA2(val evaluator: MOEvaluator,
			val populationSize: Int,
			val archiveSize: Int,
			val mutationProba: Double
	  ) extends EvolutionaryAlgorithm {
  
  def optimizeMOO(maxIters: Int, maxEvals: Int): Set[MOOPoint] = {
    var nbIterations = 1
    while (nbIterations <= maxIters && evaluator.nbCallToEvalFunction <= maxEvals) {
      performIteration(nbIterations)
      nbIterations += 1
    }
    archive.map(elem => elem.getMOOPoint).toSet
  }
  
  def performIteration(iterationNumber: Int): Unit = {
    val (fitnessValues, distances) = getFitnessValues
    updateArchive(fitnessValues, distances)
    updatePopulation(fitnessValues: List[(EvolutionaryElement, Double)])
  }
  
  
  def updateArchive(fitnessValues: List[(EvolutionaryElement, Double)], distances: HashMap[EvolutionaryElement, (Double, Array[Double])]): Unit = {
    val potentialArchive = fitnessValues.filter(elem => elem._2 < 1).map(e => e._1)
    if (potentialArchive.length == archiveSize) {
      archive = potentialArchive
    }
    else if (potentialArchive.length < archiveSize) {
      archive = potentialArchive
      for (i <- potentialArchive.length until archiveSize) {
        archive ::= fitnessValues(i)._1
      }
    }
    else {
      def distCmp(e1: EvolutionaryElement, e2: EvolutionaryElement): Boolean = {
        def distCmpAux(index: Int): Boolean = {
          if (index >= e1.nbCoordinates) true
          else {
            if (distances(e1)._2(index) > distances(e2)._2(index)) true
            else if (distances(e1)._2(index) < distances(e2)._2(index)) false
            else distCmpAux(index + 1)
          }
        }
        distCmpAux(0)
      }
      val potArchive = potentialArchive.sortWith((e1, e2) => distCmp(e1, e2))
      archive = potArchive.take(archiveSize)      
    }
  }
  
  def rawFitness(allPoints: Array[EvolutionaryElement]): HashMap[EvolutionaryElement, Int] = {
    // Map linking every point P to a tuple (i, l) where i is the nb of points P dominates and l is the points dominating P.
    val dominanceMap = HashMap[EvolutionaryElement, (Int, List[EvolutionaryElement])]()
    for (e <- allPoints) dominanceMap += e -> (0, List[EvolutionaryElement]())
    for (i <- 0 until allPoints.length) {
      for (j <- i until allPoints.length) {
        val domi = allPoints(i).dominance(allPoints(j).getMOOPoint)
        if (domi > 0) {
          dominanceMap.update(allPoints(j), (dominanceMap(allPoints(j))._1, allPoints(i) :: dominanceMap(allPoints(j))._2))
          dominanceMap.update(allPoints(i), (dominanceMap(allPoints(i))._1 + 1, dominanceMap(allPoints(i))._2))
        }
        else if (domi < 0) {
          dominanceMap.update(allPoints(i), (dominanceMap(allPoints(i))._1, allPoints(j) :: dominanceMap(allPoints(i))._2))
          dominanceMap.update(allPoints(j), (dominanceMap(allPoints(j))._1 + 1, dominanceMap(allPoints(j))._2))
        }
      }
    }
    val rawFitnesses = dominanceMap.map(elem => {
      elem._1 -> elem._2._2.foldLeft(0)((acc, newElem) => acc + dominanceMap(newElem)._1)
    })
    rawFitnesses
  }
  
  def densityAndDistance(allPoints: Array[EvolutionaryElement]): HashMap[EvolutionaryElement, (Double, Array[Double])] = {
    def euclidianDistance(e1: EvolutionaryElement, e2:EvolutionaryElement): Double = {
      var sum = 0.0
      for (i <- 0 until e1.nbCoordinates) {
        sum += math.pow(e1.getCoordinates(i) - e2.getCoordinates(i), 2.0)
      }
      math.sqrt(sum)
    }
    val distanceMatrixPre = Array.fill(allPoints.length)(Array.fill(allPoints.length)(0.0))
    for (i <- 0 until allPoints.length; j <- i until allPoints.length) {
      val dist = euclidianDistance(allPoints(i), allPoints(j))
      distanceMatrixPre(i)(j) = dist
      distanceMatrixPre(j)(i) = dist
    }
    val distanceMatrix = distanceMatrixPre.map(e => e.sortWith((e1, e2) => e1 <= e2))
    val densityMap = HashMap[EvolutionaryElement, (Double, Array[Double])]()
    val k = math.sqrt(allPoints.length).toInt
    for (i <- 0 until allPoints.length) {
      densityMap += allPoints(i) -> (1.0 / (distanceMatrix(i)(k) + 2.0), distanceMatrix(i))
    }
    densityMap
  }
  
  // Returns a pair (l, a) where l is a sorted list of pairs (point, fitness value) and a
  // is the distance matrix sorted by minimalDistance.
  def getFitnessValues: (List[(EvolutionaryElement, Double)], HashMap[EvolutionaryElement, (Double, Array[Double])]) = {
    val allPoints = Array.tabulate(population.size + archive.size)(i => if (i < population.size) population(i) else archive(i - population.size))
    val rawFitnesses = rawFitness(allPoints)
    val densities = densityAndDistance(allPoints)
    val allPointList = allPoints.toList.map(point => (point, rawFitnesses(point) + densities(point)._1))
    (allPointList.sortBy(_._2), densities)
  }
}

object SPEA2 {
  def apply(
      evaluator: MOEvaluator,
      populationSize: Int,
      archiveSize: Int,
      mutationProba: Double
	): SPEA2 = new SPEA2(evaluator, populationSize, archiveSize, mutationProba)
  
  def apply(
      evaluator: MOEvaluator,
      populationSize: Int,
      archiveSize: Int
	): SPEA2 = new SPEA2(evaluator, populationSize, archiveSize, 0.1)
    
  var onArchChan: (ParetoFront[_, _]) => Unit = {newArchive: ParetoFront[_, _] => }
  
  def onArchiveChanged(newFun: ParetoFront[_, _] => Unit): Unit = {
	onArchChan = newFun
  }
  
}
