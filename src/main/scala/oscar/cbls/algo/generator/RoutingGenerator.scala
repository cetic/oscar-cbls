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
import scala.math.{atan2, cos, max, pow, round, sin, sqrt}
import scala.util.Random

/** Object for generating data for Routing problem. By default, the coordinates are generated in
  * `[0;` `1000] * [0; 1000]`. See [[setMapDimensions]] to change it.
  */
object RoutingGenerator {

  /** Inclusive lower bound on the coordinates of the points. */
  private var minXY: Long = 0L

  /** Inclusive upper bound on the coordinates of the points. */
  private var maxXY: Long = 1000L
  private var side: Long  = maxXY - minXY

  /** Set the bounds of the coordinates. */
  def setMapDimensions(newMinXY: Long, newMaxXY: Long): Unit = {
    minXY = newMinXY
    maxXY = newMaxXY
    side = newMaxXY - newMinXY
  }

  /** Generates random data for routing.
    *
    * @param numDepot
    *   The number of depot to generate.
    * @param numNodes
    *   The number of ''nodes to visit''.
    * @param weightFactorForUnroutedNodes
    *   A factor used to increase the cost of unrouted nodes.
    * @param maxCostForUsingVehicle
    *   The maximal cost for using a new vehicle.
    * @param seed
    *   Seed of the random number generator. By default, the seed is a random value.
    * @return
    *   1. An array of `numDepot + numNodes` positions. The first `numDepot` positions are supposed
    *      to be the depots. The other ones are nodes to visit.
    *   1. A distances matrix.
    *   1. The cost for unrouted nodes.
    *   1. A cost for using a new vehicle.
    */
  def generateRandomRoutingData(
    numDepot: Int,
    numNodes: Int,
    weightFactorForUnroutedNodes: Long,
    maxCostForUsingVehicle: Long,
    seed: Long = Random.nextLong()
  ): (Array[(Long, Long)], Array[Array[Long]], Long, Long) = {
    val rng          = new Random(seed)
    val pos          = randomNodes(numDepot, numNodes, rng)
    val dist         = distancesMatrix(pos)
    val unroutedCost = costForUnroutedNodes(dist, weightFactorForUnroutedNodes, rng)
    val vehicleCost  = costForUsingVehicle(maxCostForUsingVehicle, rng)

    (pos, dist, unroutedCost, vehicleCost)
  }

  /** Generates random data for routing. Each node is evenly distant from each other. '''WARNING''':
    * If `numDepot + numNodes` and `nodeDistance` are too big considering the map's bounds, this
    * generator cannot guarentee to generate exactly `numDepot + numNodes` nodes. In that case, the
    * generator stops after fulfilling the map.
    *
    * @param numDepot
    *   The number of depots to generate.
    * @param numNodes
    *   The number of ''nodes to visit''.
    * @param weightFactorForUnroutedNodes
    *   A factor used to increase the cost of unrouted nodes.
    * @param maxCostForUsingVehicle
    *   The maximal cost for using a new vehicle.
    * @param nodeDistance
    *   The distance between two adjacent nodes.
    * @param seed
    *   Seed of the random number generator. By default, the seed is a random value.
    * @return
    *   1. An array of `numDepot + numNodes` positions (if the map is not full). The first
    *      `numDepot` positions are supposed to be the depots. The other ones are nodes to visit.
    *   1. A distances matrix.
    *   1. The cost for unrouted nodes.
    *   1. A cost for using a new vehicle.
    */
  def generateEvenlySpacedRoutingData(
    numDepot: Int,
    numNodes: Int,
    weightFactorForUnroutedNodes: Long,
    maxCostForUsingVehicle: Long,
    nodeDistance: Long,
    seed: Long = Random.nextLong()
  ): (Array[(Long, Long)], Array[Array[Long]], Long, Long) = {
    val rng          = new Random(seed)
    val depot        = centerDepot
    val pos          = evenlySpacedNodes(numDepot, numNodes, nodeDistance, depot, rng)
    val dist         = distancesMatrix(pos)
    val unroutedCost = costForUnroutedNodes(dist, weightFactorForUnroutedNodes, rng)
    val vehicleCost  = costForUsingVehicle(maxCostForUsingVehicle, rng)

    (pos, dist, unroutedCost, vehicleCost)
  }

  /** Generates random data for routing. The generated nodes are grouped by clusters. '''WARNING''':
    * According to the dimension of the map and the input values, the generator can stop before
    * generating `numDepot + numCluster * nodesByCluster` nodes if the map is full.
    *
    * @param numDepot
    *   The number of depot to generate.
    * @param numCluster
    *   The number of cluster of ''node to visit'' .
    * @param nodesByCluster
    *   How many cluster have to be in a cluster.
    * @param clusterSize
    *   The nodes are clustered in squares. `clusterSize` defines the side's length of these
    *   squares.
    * @param weightFactorForUnroutedNodes
    *   A factor used to increase the cost of unrouted nodes.
    * @param maxCostForUsingVehicle
    *   The maximal cost for using a new vehicle.
    * @param seed
    *   Seed of the random number generator. By default, the seed is a random value.
    * @return
    *   1. An array of `numDepot + numCluster * nodesByCluster` positions (if the map is not full).
    *      The first `numDepot` positions are supposed to be the depots. The other ones are nodes to
    *      visit.
    *   1. A distances matrix.
    *   1. The cost for unrouted nodes.
    *   1. A cost for using a new vehicle.
    */
  def generateClusteredRoutingData(
    numDepot: Int,
    numCluster: Int,
    nodesByCluster: Int,
    clusterSize: Int,
    weightFactorForUnroutedNodes: Long,
    maxCostForUsingVehicle: Long,
    seed: Long = Random.nextLong()
  ): (Array[(Long, Long)], Array[Array[Long]], Long, Long) = {
    val rng          = new Random(seed)
    val pos          = clusteredNodes(numDepot, numCluster, nodesByCluster, clusterSize, rng)
    val dist         = distancesMatrix(pos)
    val unroutedCost = costForUnroutedNodes(dist, weightFactorForUnroutedNodes, rng)
    val vehicleCost  = costForUsingVehicle(maxCostForUsingVehicle, rng)

    (pos, dist, unroutedCost, vehicleCost)
  }

  /** Generates random data for Routing. The generated positions correspond to geographical
    * coordinates (latitude, longitude) in degrees.
    *
    * @param numDepot
    *   The number of depot to generate.
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
    * @param seed
    *   Seed of the random number generator. By default, the seed is a random value.
    * @return
    *   1. An array of positions (latitude, longitude) in degrees for the nodes. The first positions
    *      are supposed to be the depots
    *   1. A distances matrix (in meters). These distances has been rounded.
    *   1. The cost for unrouted nodes
    *   1. A cost for using a new vehicle.
    */
  def generateGeographicRoutingData(
    numDepot: Int,
    numNodes: Int,
    weightFactorForUnroutedNodes: Long,
    maxCostForUsingVehicle: Long,
    minLatitude: Double = -90.0,
    maxLatitude: Double = 90.0,
    minLongitude: Double = -180.0,
    maxLongitude: Double = 180.0,
    seed: Long = Random.nextLong()
  ): (Array[(Double, Double)], Array[Array[Long]], Long, Long) = {
    val rng = new Random(seed)
    // Positions for the nodes + the depot
    val (pos, dist) =
      geographicRandom(
        numDepot + numNodes,
        minLatitude,
        maxLatitude,
        minLongitude,
        maxLongitude,
        rng
      )
    val unroutedCost = costForUnroutedNodes(dist, weightFactorForUnroutedNodes, rng)
    val vehicleCost  = costForUsingVehicle(maxCostForUsingVehicle, rng)

    (pos, dist, unroutedCost, vehicleCost)
  }

  /** Computes the center of the map. */
  def centerDepot: (Long, Long) = {
    val center: Long = (minXY + maxXY) / 2
    (center, center)
  }

  /** @param d
    *   The number of depots to generate.
    * @param n
    *   The number of nodes to generate.
    * @param rng
    *   The random number generator used to generate values.
    * @return
    *   `d + n` random positions for nodes. The first `d` positions are supposed to be the depots'
    *   ones.
    */
  def randomNodes(d: Int, n: Int, rng: Random = Random): Array[(Long, Long)] =
    Array.fill(d + n)(randomPosition(minXY, maxXY, minXY, maxXY, rng))

  /** @param numDepot
    *   The number of depot to generate.
    * @param numCluster
    *   The number of cluster of nodes to generate.
    * @param nodesByCluster
    *   How many cluster have to be in a cluster.
    * @param clusterSize
    *   The nodes are clustered in squares. `clusterSize` defines the side's length of these
    *   squares.
    * @param rng
    *   The random number generator used to generate values.
    * @return
    *   `numDepot + numCluster * nodesByCluster` random positions grouped in clusters. The first
    *   `numDepot` positions are supposed to be the depots. '''WARNING''': According to the
    *   dimension of the map and the input values, the generator can stop before generating
    *   `numDepot + numCluster * nodesByCluster` nodes if the map is full.
    */
  def clusteredNodes(
    numDepot: Int,
    numCluster: Int,
    nodesByCluster: Int,
    clusterSize: Int,
    rng: Random = Random
  ): Array[(Long, Long)] = {
    val nodesPositions: mutable.Queue[(Long, Long)] = mutable.Queue()

    // Generates n positions belonging to the current cluster
    def generateCluster(center: (Long, Long), n: Int): Unit = {
      val currentMin: Long = center._1 - clusterSize
      val currentMax: Long = center._1 + clusterSize
      for (_ <- 0 until n) {
        val pos: (Long, Long) = randomPosition(currentMin, currentMax, currentMin, currentMax, rng)
        nodesPositions += pos
      }

    }

    // Returns a new center which do not belong to the last cluster.
    def generateNewCenter(currentCenter: (Long, Long)): (Long, Long) = {
      val currentMin: Long = currentCenter._1 - clusterSize
      val currentMax: Long = currentCenter._1 + clusterSize
      var tries            = 0
      var newCenter        = (0L, 0L)
      do {
        newCenter = randomPosition(minXY, maxXY, minXY, maxXY, rng)
        tries += 1
      } while (!inInterval(newCenter._1, currentMin, currentMax)
        && !inInterval(newCenter._2, currentMin, currentMax)
        && tries < 10000)

      newCenter
    }

    var currentCenter = randomPosition(minXY, maxXY, minXY, maxXY, rng)
    // Generates a cluster of depot.
    generateCluster(currentCenter, numDepot)
    currentCenter = generateNewCenter(currentCenter)
    // Generates clusters of nodes to visit.
    for (_ <- 0 until numCluster) {
      generateCluster(currentCenter, nodesByCluster)
      currentCenter = generateNewCenter(currentCenter)
    }

    nodesPositions.toArray
  }

  /** @param numDepots
    *   The number of depots to generate.
    * @param numNodes
    *   The number of nodes to generate.
    * @param nodeDistance
    *   The distance between two adjacent nodes.
    * @param firstDepotPos
    *   The position of the first depot.
    * @param rng
    *   The random number generator used to generate values.
    * @return
    *   An array of nodes two by two distant from `nodeDistance`. The first `numDepot` nodes are
    *   supposed to be depots. '''WARNING''': If `numDepots + numNodes` and `nodeDistance` are too
    *   big considering the map's bounds, this generator cannot guarentee to generate exactly
    *   `numDepots + numNodes` nodes. In that case, the generator stops after fulfilling the map.
    */
  def evenlySpacedNodes(
    numDepots: Int,
    numNodes: Int,
    nodeDistance: Long,
    firstDepotPos: (Long, Long),
    rng: Random = Random
  ): Array[(Long, Long)] = {
    require(numDepots >= 1, "This generator needs to generate at least 1 depot.")

    val nodesPositions: mutable.Queue[(Long, Long)] = mutable.Queue(firstDepotPos)
    var lastNode: (Long, Long)                      = firstDepotPos

    // Given an integer center, the following four points are always integer for all integer radii
    val plusX     = (p: (Long, Long)) => (p._1 + nodeDistance, p._2)
    val minusX    = (p: (Long, Long)) => (p._1 - nodeDistance, p._2)
    val plusY     = (p: (Long, Long)) => (p._1, p._2 + nodeDistance)
    val minusY    = (p: (Long, Long)) => (p._1, p._2 - nodeDistance)
    var translate = mutable.ArraySeq(plusX, minusX, plusY, minusY)

    /** To be admissible, a node must be included in the map bounds and not already exist. */
    def isAdmissibleNode(node: (Long, Long)): Boolean =
      inInterval(node._1, minXY, maxXY) && inInterval(node._2, minXY, maxXY) && !nodesPositions
        .contains(node) && node != firstDepotPos

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

    var i: Int              = 1
    var translateIndex: Int = 0

    while (i < numDepots + numNodes) {
      translate = rng.shuffle(translate)
      val newNode = translate(translateIndex)(lastNode)
      if (isAdmissibleNode(newNode)) { // We can add the new node
        nodesPositions += newNode
        lastNode = newNode
        i += 1
        translateIndex = 0
      } else if (translateIndex + 1 < translate.length) { // We need to try another translation
        translateIndex += 1
      } else { // We tried all the translations. The last node is blocked by other nodes.
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

  /** Computes the euclidean distance between each pair of positions in input. */
  def distancesMatrix(pos: Array[(Long, Long)]): Array[Array[Long]] =
    Array.tabulate(pos.length, pos.length)((i, j) => distance(pos(i), pos(j)))

  /** @param distances
    *   A matrix of distances between each nodes including the depot.
    * @param weightFactor
    *   A factor used to increase the cost of unrouted nodes.
    * @param rng
    *   The random number generator used to generate values.
    * @return
    *   A cost for unrouted nodes based on the maximum distance from the input distance matrix.
    */
  def costForUnroutedNodes(
    distances: Array[Array[Long]],
    weightFactor: Long,
    rng: Random = Random
  ): Long = {
    var maxDist: Long = 0L
    for (i <- distances.indices) {
      for (j <- distances(i).indices) {
        maxDist = max(maxDist, distances(i)(j))
      }
    }

    maxDist + rng.between(0L, side * weightFactor + 1L)
  }

  /** @param maxCost
    *   The maximal cost for using a new vehicle.
    * @param rng
    *   The random number generator used to generate values.
    * @return
    *   A random cost in `[0, maxCost]` for using a new vehicle.
    */
  def costForUsingVehicle(maxCost: Long, rng: Random = Random): Long = rng.between(0L, maxCost + 1)

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
    * @param rng
    *   The random number generator used to generate values.
    */
  def geographicRandom(
    n: Int,
    minLatitude: Double,
    maxLatitude: Double,
    minLongitude: Double,
    maxLongitude: Double,
    rng: Random = Random
  ): (Array[(Double, Double)], Array[Array[Long]]) = {

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

      // Haversine formula to compute the half-chord length
      val chord = pow(sin(deltaPhi / 2), 2) + cos(phi1) * cos(phi2) * pow(sin(deltaLambda / 2), 2)
      // Central angle. We use atan2 to be sure to have an angle in [0, pi] radians
      val sigma = 2 * atan2(sqrt(chord), sqrt(1 - chord))

      // Arc length
      r * sigma // meters
    }

    val pos: Array[(Double, Double)] = Array.fill(n)((randomLatitude, randomLongitude))
    val distanceMatrix: Array[Array[Long]] =
      Array.tabulate(n, n)((i, j) => round(distance(pos(i), pos(j))))

    (pos, distanceMatrix)
  }
}
