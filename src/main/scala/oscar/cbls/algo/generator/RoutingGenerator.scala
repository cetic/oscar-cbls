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

import GeneratorUtil._

import scala.collection.mutable
import scala.math.{atan2, cos, pow, sin, sqrt}
import scala.util.Random

/** Object to generate data for Routing problem. */
object RoutingGenerator extends RoutingGenerator(0L, 1000L) {

  /** Generates random data for routing.
    *
    * @param numNodes
    *   The number of ''nodes to visit''.
    * @param weightFactorForUnroutedNodes
    *   A factor used to increase the cost of unrouted nodes.
    * @param maxCostForUsingVehicle
    *   The maximal cost for using a new vehicle.
    * @return
    *   An array of positions for the nodes, including the depot at index 0, a distances matrix, the
    *   cost for unrouted nodes and a cost for using a new vehicle.
    */
  def generateRandomRoutingData(
    numNodes: Int,
    weightFactorForUnroutedNodes: Long,
    maxCostForUsingVehicle: Long
  ): (Array[(Long, Long)], Array[Array[Long]], Long, Long) = {
    val depot        = randomDepot
    val nodes        = randomNodes(numNodes)
    val pos          = depot +: nodes
    val dist         = distancesMatrix(pos)
    val unroutedCost = costForUnroutedNodes(dist, weightFactorForUnroutedNodes)
    val vehicleCost  = costForUsingVehicle(maxCostForUsingVehicle)

    (pos, dist, unroutedCost, vehicleCost)
  }

  /** Generates random data for routing. Each node is evenly distant from each other. WARNING: If
    * `numNodes` and `nodeDistance` are too big considering the map's bounds, this generator cannot
    * guarentee to generate exactly `numNodes` nodes. In that case, the generator stops after
    * fulfilling the map.
    *
    * @param numNodes
    *   The number of ''node to visit''.
    * @param weightFactorForUnroutedNodes
    *   A factor used to increase the cost of unrouted nodes.
    * @param maxCostForUsingVehicle
    *   The maximal cost for using a new vehicle.
    * @return
    *   An array of positions for the nodes, including the depot at index 0, a distances matrix, the
    *   cost for unrouted nodes and a cost for using a new vehicle.
    */
  def generateEvenlySpacedRoutingData(
    numNodes: Int,
    weightFactorForUnroutedNodes: Long,
    maxCostForUsingVehicle: Long,
    nodeDistance: Long
  ): (Array[(Long, Long)], Array[Array[Long]], Long, Long) = {
    val depot        = centerDepot
    val nodes        = evenlySpacedNodes(numNodes, nodeDistance, depot)
    val pos          = depot +: nodes
    val dist         = distancesMatrix(pos)
    val unroutedCost = costForUnroutedNodes(dist, weightFactorForUnroutedNodes)
    val vehicleCost  = costForUsingVehicle(maxCostForUsingVehicle)

    (pos, dist, unroutedCost, vehicleCost)
  }

  /** Generates random data for routing. The generated nodes are grouped by clusters.
    *
    * @param numCluster
    *   The number of cluster of ''node to visit'' .
    * @param nodesByCluster
    *   How many cluster have to be in a cluster.
    * @param clusterRadius
    *   The maximum distance between the numNodes in the same cluster.
    * @param weightFactorForUnroutedNodes
    *   A factor used to increase the cost of unrouted nodes.
    * @param maxCostForUsingVehicle
    *   The maximal cost for using a new vehicle.
    * @return
    *   An array of `numCluster * nodeByCluster` positions for the nodes, including the depot at
    *   index 0, a distances matrix, the cost for unrouted nodes and a cost for using a new vehicle.
    */
  def generateClusteredRoutingDate(
    numCluster: Int,
    nodesByCluster: Int,
    clusterRadius: Int,
    weightFactorForUnroutedNodes: Long,
    maxCostForUsingVehicle: Long
  ): (Array[(Long, Long)], Array[Array[Long]], Long, Long) = {
    val depot        = randomDepot
    val nodes        = clusteredNodes(numCluster, nodesByCluster, clusterRadius)
    val pos          = depot +: nodes
    val dist         = distancesMatrix(pos)
    val unroutedCost = costForUnroutedNodes(dist, weightFactorForUnroutedNodes)
    val vehicleCost  = costForUsingVehicle(maxCostForUsingVehicle)

    (pos, dist, unroutedCost, vehicleCost)
  }

  /** Generates random data for Routing. The generated positions correspond to geographical
    * coordinates (latitude, longitude) in degrees.
    *
    * @param numNodes
    *   The number of ''node to visit''.
    * @param weightFactorForUnroutedNodes
    *   A factor used to increase the cost of unrouted nodes.
    * @param maxCostForUsingVehicle
    *   The maximal cost for using a new vehicle.
    * @param minLatitude
    *   The inclusive minimal latitude of the points in degrees.
    * @param maxLatitude
    *   The exclusive maximal latitude of the points in degrees.
    * @param minLongitude
    *   The inclusive maximal longitude of the points in degrees.
    * @param maxLongitude
    *   The exclusive maximal longitude of the points in degrees.
    * @return
    *   An array of positions (latitude, longitude) in degrees for the nodes, including the depot at
    *   index 0, a distances matrix (in meters), the cost for unrouted nodes and a cost for using a
    *   new vehicle.
    */
  def generateGeographicRoutingData(
    numNodes: Int,
    weightFactorForUnroutedNodes: Long,
    maxCostForUsingVehicle: Long,
    minLatitude: Double = -90.0,
    maxLatitude: Double = 90.0,
    minLongitude: Double = -180.0,
    maxLongitude: Double = 180.0
  ): (Array[(Double, Double)], Array[Array[Double]], Double, Long) = {
    // Positions for the nodes + the depot
    val (pos, dist) =
      geographicRandom(numNodes + 1, minLatitude, maxLatitude, minLongitude, maxLongitude)
    val unroutedCost = costForUnroutedNodes(dist, weightFactorForUnroutedNodes.toDouble)
    val vehicleCost  = costForUsingVehicle(maxCostForUsingVehicle)

    (pos, dist, unroutedCost, vehicleCost)
  }
}

/** @param minXY
  *   Inclusive lower bound on the coordinates of the points.
  * @param maxXY
  *   Inclusive upper bound on the coordinates of the points.
  */
class RoutingGenerator(var minXY: Long, var maxXY: Long) {
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
    *   The number of nodes to generate.
    * @return
    *   `n` random position for nodes.
    */
  def randomNodes(n: Int): Array[(Long, Long)] =
    Array.fill(n)(randomPosition(minXY, maxXY, minXY, maxXY))

  /** @param numCluster
    *   The number of cluster of nodes to generate.
    * @param nodesByCluster
    *   How many cluster have to be in a cluster.
    * @param clusterRadius
    *   The maximum distance between the nodes in the same cluster.
    * @return
    *   `numCluster * nodesByCluster` random position grouped in clusters.
    */
  def clusteredNodes(
    numCluster: Int,
    nodesByCluster: Int,
    clusterRadius: Int
  ): Array[(Long, Long)] = {
    val nodesPositions: mutable.Queue[(Long, Long)] = mutable.Queue()

    var currentCenter = randomPosition(minXY, maxXY, minXY, maxXY)
    for (_ <- 0 until numCluster) {
      for (_ <- 0 until nodesByCluster) {
        var pos: (Long, Long) = (0L, 0L)
        var tries: Int        = 0
        do {
          pos = randomPosition(minXY, maxXY, minXY, maxXY)
          tries += 1
        } while (distance(currentCenter, pos) > clusterRadius && tries < 10000)
        nodesPositions += pos
      }
      var tries     = 0
      var newCenter = (0L, 0L)
      do {
        newCenter = randomPosition(minXY, maxXY, minXY, maxXY)
        tries += 1
      } while (distance(currentCenter, newCenter) <= clusterRadius && tries < 10000)
      currentCenter = newCenter
    }
    nodesPositions.toArray
  }

  /** @param n
    *   The number of nodes to generate.
    * @param nodeDistance
    *   The distance between two adjacent nodes.
    * @param depotPos
    *   The position of the depot.
    * @return
    *   An array of nodes two by two distant from `nodeDistance`. The center of the map is reserved
    *   for the depot. WARNING: If `n` and `nodeDistance` are too big considering the map's bounds,
    *   this generator cannot guarentee to generate exactly `n` nodes. In that case, the generator
    *   stops after fulfilling the map.
    */
  def evenlySpacedNodes(n: Int, nodeDistance: Long, depotPos: (Long, Long)): Array[(Long, Long)] = {
    val nodesPositions: mutable.Queue[(Long, Long)] = mutable.Queue()
    var lastNode: (Long, Long)                      = depotPos

    val plusX     = (p: (Long, Long)) => (p._1 + nodeDistance, p._2)
    val minusX    = (p: (Long, Long)) => (p._1 - nodeDistance, p._2)
    val plusY     = (p: (Long, Long)) => (p._1, p._2 + nodeDistance)
    val minusY    = (p: (Long, Long)) => (p._1, p._2 - nodeDistance)
    var translate = mutable.ArraySeq(plusX, minusX, plusY, minusY)

    /** To be admissible, a node must be in the map and not already exist. */
    def isAdmissibleNode(node: (Long, Long)): Boolean =
      inInterval(node._1, minXY, maxXY) && inInterval(node._2, minXY, maxXY) && !nodesPositions
        .contains(node) && node != depotPos

    /** Tries to find a node which is not encircled by four other nodes. */
    def unblock(): Option[(Long, Long)] = {
      for (node <- nodesPositions) {
        for (t <- translate) {
          val newNode = t(node)
          if (isAdmissibleNode(newNode)) return Some(newNode)
        }
      }
      None
    }

    var i: Int              = 0
    var translateIndex: Int = 0

    while (i < n) {
      translate = rng.shuffle(translate)
      val newNode = translate(translateIndex)(lastNode)
      if (isAdmissibleNode(newNode)) { // We can add the new node
        nodesPositions += newNode
        lastNode = newNode
        i += 1
        translateIndex = 0
      } else if (translateIndex + 1 < translate.length) { // We need to try another translation
        translateIndex += 1
      } else { // We tried all the translation. The last node is blocked by other nodes.
        unblock() match {
          case Some(node) => // We can restart the generation from another node
            nodesPositions += node
            lastNode = node
            i += 1
            translateIndex = 0
          case None => return nodesPositions.toArray // The map is full. We cannot add another node
        }
      }
    }
    nodesPositions.toArray
  }

  /** Computes the euclidean distance between  each pair of position in input. */
  def distancesMatrix(pos: Array[(Long, Long)]): Array[Array[Long]] =
    Array.tabulate(pos.length, pos.length)((i, j) => distance(pos(i), pos(j)))

  /** @param distances
    *   A matrix of distances between each nodes including the depot.
    * @param weightFactor
    *   A factor used to increase the cost of unrouted nodes.
    * @return
    *   A cost for unrouted nodes based on the maximum distance from the input distance matrix.
    */
  def costForUnroutedNodes(distances: Array[Array[Long]], weightFactor: Long): Long = {
    var maxDist: Long = 0L
    for (i <- distances.indices) {
      for (j <- distances(i).indices) {
        val d = distances(i)(j)
        if (d > maxDist) maxDist = d
      }
    }

    maxDist + rng.between(0L, side * weightFactor + 1L)
  }

  /** @param distances
    *   A matrix of distances between each nodes including the depot.
    * @param weightFactor
    *   A factor used to increase the cost of unrouted nodes.
    * @return
    *   A cost for unrouted nodes based on the maximum distance from the input distance matrix.
    */
  def costForUnroutedNodes(distances: Array[Array[Double]], weightFactor: Double): Double = {
    var maxDist: Double = 0.0
    for (i <- distances.indices) {
      for (j <- distances(i).indices) {
        val d = distances(i)(j)
        if (d > maxDist) maxDist = d
      }
    }

    maxDist + rng.between(0.0, side * weightFactor)
  }

  /** @param maxCost
    *   The maximal cost for using a new vehicle.
    * @return
    *   A random cost in `[0, maxCost]` for using a new vehicle.
    */
  def costForUsingVehicle(maxCost: Long): Long = rng.between(0L, maxCost + 1)

  /** Generates points with geographical coordinates (in degrees) and the associated distance
    * matrix. The Earth is supposed to be perfectly spherical.
    *
    * @param n
    *   The number of points to generate.
    * @param minLatitude
    *   The inclusive minimal latitude of the points in degrees.
    * @param maxLatitude
    *   The exclusive maximal latitude of the points in degrees.
    * @param minLongitude
    *   The inclusive maximal longitude of the points in degrees.
    * @param maxLongitude
    *   The exclusive maximal longitude of the points in degrees.
    */
  def geographicRandom(
    n: Int,
    minLatitude: Double,
    maxLatitude: Double,
    minLongitude: Double,
    maxLongitude: Double
  ): (Array[(Double, Double)], Array[Array[Double]]) = {

    def randomLatitude: Double  = rng.between(minLatitude, maxLatitude)
    def randomLongitude: Double = rng.between(minLongitude, maxLongitude)

    /** Compute the great-circle distance between the two points. */
    def distance(coord1: (Double, Double), coord2: (Double, Double)): Double = {
      val (latitude1: Double, longitude1: Double) = coord1
      val (latitude2: Double, longitude2: Double) = coord2

      val r: Double = 6371e3 // Earth radius in meters

      val phi1        = latitude1.toRadians
      val phi2        = latitude2.toRadians
      val deltaPhi    = (phi2 - phi1).abs
      val deltaLambda = (longitude2 - longitude1).abs.toRadians

      // Haversine formula to compute the chord length
      val chord = pow(sin(deltaPhi / 2), 2) + cos(phi1) * cos(phi2) * pow(sin(deltaLambda / 2), 2)
      // Central angle. We use atan2 to be sure to have an angle in [0, pi] radians
      val sigma = 2 * atan2(sqrt(chord), sqrt(1 - chord))

      // Arc length
      r * sigma // meters
    }

    val pos: Array[(Double, Double)] = Array.fill(n)((randomLatitude, randomLongitude))
    val distanceMatrix: Array[Array[Double]] =
      Array.tabulate(n, n)((i, j) => distance(pos(i), pos(j)))

    (pos, distanceMatrix)
  }

}
