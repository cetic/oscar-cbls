// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.algo.generator

import GeneratorUtil.{inInterval, _}

import scala.collection.mutable
import scala.util.Random

object RoutingGenerator extends RoutingGenerator(0L, 1000L) {

  /** Generates random data for routing.
    *
    * @param nCities
    *   The number of cities to generate.
    * @param weightFactorForUnroutedCities
    *   A factor used to increase the cost of unrouted cities.
    * @param maxCostForUsingVehicle
    *   The maximal cost for using a new vehicle.
    * @return
    *   An array of positions for the cities, including the depot at index 0, a distances matrix,
    *   the cost for unrouted cities and a cost for using a new vehicle.
    */
  def generateRandomRoutingData(
    nCities: Int,
    weightFactorForUnroutedCities: Long,
    maxCostForUsingVehicle: Long
  ): (Array[(Long, Long)], Array[Array[Long]], Long, Long) = {
    val depot        = randomDepot
    val cities       = randomCities(nCities)
    val pos          = depot +: cities
    val dist         = distancesMatrix(pos)
    val unroutedCost = costForUnroutedCities(dist, weightFactorForUnroutedCities)
    val vehicleCost  = costForUsingVehicle(maxCostForUsingVehicle)

    (pos, dist, unroutedCost, vehicleCost)
  }

  /** Generates random date for routing. Each city is evenly distant from each other.
    *
    * @param nCities
    *   The number of cities to generate.
    * @param weightFactorForUnroutedCities
    *   A factor used to increase the cost of unrouted cities.
    * @param maxCostForUsingVehicle
    *   The maximal cost for using a new vehicle.
    * @return
    *   An array of positions for the cities, including the depot at index 0, a distances matrix,
    *   the cost for unrouted cities and a cost for using a new vehicle.
    */
  def generateEvenlySpacedRoutingData(
    nCities: Int,
    weightFactorForUnroutedCities: Long,
    maxCostForUsingVehicle: Long,
    cityDistance: Long
  ): (Array[(Long, Long)], Array[Array[Long]], Long, Long) = {
    val depot        = centerDepot
    val cities       = evenlySpacedCities(nCities, cityDistance)
    val pos          = depot +: cities
    val dist         = distancesMatrix(pos)
    val unroutedCost = costForUnroutedCities(dist, weightFactorForUnroutedCities)
    val vehicleCost  = costForUsingVehicle(maxCostForUsingVehicle)

    (pos, dist, unroutedCost, vehicleCost)
  }
}

/** @param minXY
  *   Lower bound on the coordinates of the points.
  * @param maxXY
  *   Upper bound on the coordinates of the points.
  */
protected class RoutingGenerator(var minXY: Long, var maxXY: Long) {
  // We are working on a square map
  private val side: Long = maxXY - minXY

  protected var _seed: Long = Random.nextLong()
  protected val rng: Random = new Random(_seed)
  GeneratorUtil.rng.setSeed(_seed)

  /** Return the seed used for random generator. */
  def seed: Long = _seed

  /** Sets the seed of random number generator with `s`. */
  def setSeed(s: Long): Unit = {
    rng.setSeed(s)
    _seed = s
    GeneratorUtil.rng.setSeed(s)
  }

  /** Generates a random position for the depot. */
  def randomDepot: (Long, Long) =
    randomPosition(minXY, maxXY, minXY, maxXY)

  /** Computes the center of the map. */
  def centerDepot: (Long, Long) = {
    val center: Long = (minXY + maxXY) / 2
    (center, center)
  }

  /** @param n
    *   The number of cities to generate.
    * @return
    *   `n` random position for cities.
    */
  def randomCities(n: Int): Array[(Long, Long)] =
    Array.fill(n)(randomPosition(minXY, maxXY, minXY, maxXY))

  /** @param numCluster
    *   The number of cluster of cities to generate.
    * @param citiesByCluster
    *   How many cluster have to be in a cluster.
    * @param clusterRadius
    *   The maximum distance between the cities in the same cluster.
    * @return
    *   `numCluster * citiesByCluster` random position grouped in clusters.
    */
  def clusteredCities(
    numCluster: Int,
    citiesByCluster: Int,
    clusterRadius: Int
  ): Array[(Long, Long)] = {
    val citiesPositions: mutable.Queue[(Long, Long)] = mutable.Queue()

    var currentCenter = randomPosition(minXY, maxXY, minXY, maxXY)
    for (_ <- 0 until numCluster) {
      for (_ <- 0 until citiesByCluster) {
        var pos: (Long, Long) = (0L, 0L)
        var tries: Int        = 0
        do {
          pos = randomPosition(minXY, maxXY, minXY, maxXY)
          tries += 1
        } while (distance(currentCenter, pos) > clusterRadius && tries < 10000)
        citiesPositions += pos
      }
      var tries = 0
      var newCenter = (0L, 0L)
      do {
        newCenter = randomPosition(minXY, maxXY, minXY, maxXY)
        tries += 1
      } while (distance(currentCenter, newCenter) <= clusterRadius && tries < 10000)
      currentCenter = newCenter
    }
    citiesPositions.toArray
  }

  /** @param n
    *   The number of cities to generate.
    * @param cityDistance
    *   The distance between two adjacent cities.
    * @return
    *   An array of cities two by two distant from `cityDistance`. The center of the map is reserved
    *   for the depot
    */
  def evenlySpacedCities(n: Int, cityDistance: Long): Array[(Long, Long)] = {
    val citiesPositions: mutable.Queue[(Long, Long)] = mutable.Queue()
    var lastCity: (Long, Long)                       = centerDepot

    val plusX     = (p: (Long, Long)) => (p._1 + cityDistance, p._2)
    val minusX    = (p: (Long, Long)) => (p._1 - cityDistance, p._2)
    val plusY     = (p: (Long, Long)) => (p._1, p._2 + cityDistance)
    val minusY    = (p: (Long, Long)) => (p._1, p._2 - cityDistance)
    var translate = mutable.ArraySeq(plusX, minusX, plusY, minusY)

    /** To be admissible, a city must be in the map and not already exist. */
    def isAdmissibleCity(city: (Long, Long)): Boolean =
      inInterval(city._1, minXY, maxXY) && inInterval(city._2, minXY, maxXY) && !citiesPositions
        .contains(city) && city != centerDepot

    /** Tries to find a city which is not encircled by four other cities. */
    def unblock(): Option[(Long, Long)] = {
      for (city <- citiesPositions) {
        for (t <- translate) {
          val newCity = t(city)
          if (isAdmissibleCity(newCity)) return Some(newCity)
        }
      }
      None
    }

    var i: Int              = 1
    var translateIndex: Int = 0

    while (i < n) {
      translate = rng.shuffle(translate)
      val newCity = translate(translateIndex)(lastCity)
      if (isAdmissibleCity(newCity)) { // We can add the new city
        citiesPositions += newCity
        lastCity = newCity
        i += 1
        translateIndex = 0
      } else if (translateIndex < translate.length) { // We need to try another translation
        translateIndex += 1
      } else { // We tried all the translation. The last city is blocked by other cities.
        unblock() match {
          case Some(city) => // We can restart the generation from another city
            citiesPositions += city
            lastCity = city
            i += 1
            translateIndex = 0
          case None => return citiesPositions.toArray // The map is full. We cannot add another city
        }
      }
    }
    citiesPositions.toArray
  }

  /** Computes the euclidean distance between  each pair of position in input. */
  def distancesMatrix(pos: Array[(Long, Long)]): Array[Array[Long]] =
    Array.tabulate(pos.length, pos.length)((i, j) => distance(pos(i), pos(j)))

  /** @param distances
    *   A matrix of distances between each cities including the depot.
    * @param weightFactor
    *   A factor used to increase the cost of unrouted cities.
    * @return
    *   A cost for unrouted cities based on the maximum distance from the input distance matrix.
    */
  def costForUnroutedCities(distances: Array[Array[Long]], weightFactor: Long): Long = {
    var maxDist: Long = 0L
    for (i <- distances.indices) {
      for (j <- distances(i).indices) {
        val d = distances(i)(j)
        if (d > maxDist) maxDist = d
      }
    }

    maxDist + rng.between(0L, side * weightFactor + 1L)
  }

  /** @param maxCost
    *   The maximal cost for using a new vehicle.
    * @return
    *   A random cost in `[0, maxCost]` for using a new vehicle.
    */
  def costForUsingVehicle(maxCost: Long): Long = rng.between(0L, maxCost + 1)
}
