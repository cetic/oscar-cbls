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

import oscar.cbls.algo.generator.GeneratorUtil.{distance, randomPosition}

import scala.collection.mutable
import scala.util.Random

/** Object to generate data for Warehouses Location Problem. */
object WarehouseLocationGenerator {

  /** Inclusive lower bound on the coordinates of the points. */
  private var minXY: Long = 0L

  /** Inclusive upper bound on the coordinates of the points. */
  private var maxXY: Long = 120L
  private var side: Long  = maxXY - minXY

  /** Set the bounds of the coordinates. */
  def setMapDimensions(newMinXY: Long, newMaxXY: Long): Unit = {
    minXY = newMinXY
    maxXY = newMaxXY
    side = newMaxXY - newMinXY
  }

  /** Generates random data for a WLP.
    *
    * @param numWarehouses
    *   Number of warehouse to have in the problem.
    * @param numDelivery
    *   Number of delivery points to have in the problem.
    * @param weightFactorForOpeningWarehouseCost
    *   A factor used to increase the cost of opening a warehouse.
    * @param seed
    *   Seed of the random number generator. By default, the seed is a random value.
    * @return
    *   1. An array containing the costs for opening the warehouses.
    *   1. The positions of the warehouses.
    *   1. The positions of the delivery points.
    *   1. A matrix of distance between the delivery points and the warehouses.
    *   1. A matrix of distances between each pair of warehouses.
    */
  def generateRandomWLP(
    numWarehouses: Int,
    numDelivery: Int,
    weightFactorForOpeningWarehouseCost: Long = 3L,
    seed: Long = Random.nextLong()
  ): (
    Array[Long],
    Array[(Long, Long)],
    Array[(Long, Long)],
    Array[Array[Long]],
    Array[Array[Long]]
  ) = {
    val rng = new Random(seed)
    val openingCosts =
      costsForOpeningWarehouse(numWarehouses, weightFactorForOpeningWarehouseCost, rng)
    val warehouses = randomPositions(numWarehouses, rng)
    val delivery   = randomPositions(numDelivery, rng)
    val dToWDist   = distanceCosts(warehouses, delivery)
    val wToWDist   = warehouseToWareHouseDistances(warehouses)

    (openingCosts, warehouses, delivery, dToWDist, wToWDist)
  }

  /** Generates data for the WLP. The locations' map is checked with `numTilesOnSide`^2^ square
    * tiles. Warehouses positions are generated uniformly on all tiles.
    *
    * @param numWarehouses
    *   Number of warehouse to have in the problem.
    * @param numDelivery
    *   Number of delivery points to have in the problem.
    * @param weightFactorForOpeningWarehouseCost
    *   A factor used to increase the cost of opening a warehouse.
    * @param numTilesOnSide
    *   The number of tiles along the grid side. The map is supposed to be square.
    * @param seed
    *   Seed of the random number generator. By default, the seed is a random value.
    * @return
    *   1. An array containing the costs for opening the warehouses.
    *   1. The positions of the warehouses.
    *   1. The positions of the delivery points.
    *   1. A matrix of distance between the delivery points and the warehouses.
    *   1. A matrix of distances between each pair of warehouses.
    */
  def generateWLPOnGrid(
    numWarehouses: Int,
    numDelivery: Int,
    weightFactorForOpeningWarehouseCost: Long = 3L,
    numTilesOnSide: Long = 2L,
    seed: Long = Random.nextLong()
  ): (
    Array[Long],
    Array[(Long, Long)],
    Array[(Long, Long)],
    Array[Array[Long]],
    Array[Array[Long]]
  ) = {
    val rng = new Random(seed)
    val openingCosts =
      costsForOpeningWarehouse(numWarehouses, weightFactorForOpeningWarehouseCost, rng)
    val warehouses = gridWarehousesPositions(numWarehouses, numTilesOnSide, rng)
    val delivery   = randomPositions(numDelivery, rng)
    val dToWDist   = distanceCosts(warehouses, delivery)
    val wToWDist   = warehouseToWareHouseDistances(warehouses)

    (openingCosts, warehouses, delivery, dToWDist, wToWDist)
  }

  /** Generates random data for WLP. Here, the warehouses are guaranteed to be distant from at least
    * `minDistanceBetweenWarehouses`. '''WARNING''': if this distance is to large in relation to the
    * `numWarehouse`, generates all the warehouses' positions will be impossible and result to an
    * infinite loop.
    *
    * @param numWarehouses
    *   Number of warehouse to have in the problem.
    * @param numDelivery
    *   Number of delivery points to have in the problem.
    * @param minDistanceBetweenWarehouses
    *   The minimal distance between two warehouses.
    * @param weightFactorForOpeningWarehouseCost
    *   A factor used to increase the cost of opening a warehouse.
    * @param seed
    *   Seed of the random number generator. By default, the seed is a random value.
    * @return
    *   1. An array containing the costs for opening the warehouses.
    *   1. The positions of the warehouses.
    *   1. The positions of the delivery points.
    *   1. A matrix of distance between the delivery points and the warehouses.
    *   1. A matrix of distances between each pair of warehouses.
    */
  def generateWLPWithMinDist(
    numWarehouses: Int,
    numDelivery: Int,
    minDistanceBetweenWarehouses: Long,
    weightFactorForOpeningWarehouseCost: Long = 3L,
    seed: Long = Random.nextLong()
  ): (
    Array[Long],
    Array[(Long, Long)],
    Array[(Long, Long)],
    Array[Array[Long]],
    Array[Array[Long]]
  ) = {
    val rng = new Random(seed)
    val openingCosts =
      costsForOpeningWarehouse(numWarehouses, weightFactorForOpeningWarehouseCost, rng)
    val warehouses = minDistWarehouses(numWarehouses, minDistanceBetweenWarehouses, rng)
    val delivery   = randomPositions(numDelivery, rng)
    val dToWDist   = distanceCosts(warehouses, delivery)
    val wToWDist   = warehouseToWareHouseDistances(warehouses)

    (openingCosts, warehouses, delivery, dToWDist, wToWDist)
  }

  /** @param numWarehouse
    *   The number of warehouses to associate a cost for opening.
    * @param weightFactor
    *   A factor used to increase the cost of opening a warehouse.
    * @param rng
    *   The random number generator used to generates values.
    * @return
    *   An array of costs for opening each warehouse.
    */
  def costsForOpeningWarehouse(
    numWarehouse: Int,
    weightFactor: Long,
    rng: Random = Random
  ): Array[Long] =
    Array.fill(numWarehouse)((minXY + rng.nextDouble() * side * weightFactor).toLong)

  /** @param n
    *   The number of positions to generate.
    * @param rng
    *   The random number generator used to generate values.
    * @return
    *   `n` random positions.
    */
  def randomPositions(n: Int, rng: Random = Random): Array[(Long, Long)] =
    Array.fill(n)(randomPosition(minXY, maxXY, minXY, maxXY, rng))

  /** @param numWarehouses
    *   Number of warehouse to have in the problem.
    * @param numTilesOnSide
    *   The number of tiles along the grid side. The map is supposed to be square.
    * @param rng
    *   The random number generator used to generate values.
    * @return
    *   An array of positions. The locations' map is checked with `numTilesOnSide`^2^ square tiles.
    *   Warehouses positions are generated uniformly on all tiles.
    */
  def gridWarehousesPositions(
    numWarehouses: Int,
    numTilesOnSide: Long,
    rng: Random = Random
  ): Array[(Long, Long)] = {
    val totalNumberOfTiles: Long = numTilesOnSide * numTilesOnSide
    val numUniformWareHouseOnGrid: Long =
      numWarehouses.toLong - (numWarehouses.toLong % totalNumberOfTiles)
    val tileLength: Long                        = side / numTilesOnSide
    val warehousePositions: Array[(Long, Long)] = new Array[(Long, Long)](numWarehouses)

    var (x, y): (Long, Long) = (0L, 0L)
    var n: Long              = 0L
    // First, each tile receives the same number of warehouse.
    while (n < numUniformWareHouseOnGrid) {
      y = (n % numTilesOnSide) * tileLength
      warehousePositions(n.toInt) = randomPosition(x, x + tileLength, y, y + tileLength, rng)
      n += 1L
      // If each tile of the grid received a warehouse, we restart from the tile 0
      if (n % totalNumberOfTiles == 0L) x = 0L
      // If each tile of a column received a warehouse, we move to the following column
      else if (n % numTilesOnSide == 0L) x += tileLength
    }

    // The remainder of the warehouse are placed randomly.
    for (i <- numUniformWareHouseOnGrid.toInt until numWarehouses)
      warehousePositions(i) = randomPosition(minXY, maxXY, minXY, maxXY, rng)

    warehousePositions
  }

  /** @param numWarehouses
    *   Number of warehouse to have in the problem.
    * @param minDistanceBetweenWarehouses
    *   The minimal distance between two warehouses. '''WARNING''': if this distance is to large in
    *   relation to the `numWarehouse`, generates all the warehouses' positions will be impossible
    *   and result to an infinite loop.
    * @param rng
    *   The random number generator used to generate values.
    * @return
    *   An array of positions. The warehouses are guaranteed to be * distant from at least
    *   `minDistanceBetweenWarehouses`.
    */
  def minDistWarehouses(
    numWarehouses: Int,
    minDistanceBetweenWarehouses: Long,
    rng: Random = Random
  ): Array[(Long, Long)] = {
    val warehousesPositions: mutable.Queue[(Long, Long)] =
      mutable.Queue[(Long, Long)](randomPosition(minXY, maxXY, minXY, maxXY, rng))
    for (_ <- 1 until numWarehouses) {
      var pos: (Long, Long) = (0L, 0L)
      var tries: Int        = 0
      do {
        pos = randomPosition(minXY, maxXY, minXY, maxXY, rng)
        tries += 1
      } while (warehousesPositions.exists(p => distance(p, pos) <= minDistanceBetweenWarehouses)
        && tries < 10000)
      warehousesPositions.append(pos)
    }

    warehousesPositions.toArray
  }

  /** Generates, for each delivery point, the distance to each warehouse.
    *
    * @param warehousesPositions
    *   The positions of the warehouses.
    * @param deliveryPositions
    *   The positions of the delivery points.
    */
  def distanceCosts(
    warehousesPositions: Array[(Long, Long)],
    deliveryPositions: Array[(Long, Long)]
  ): Array[Array[Long]] = {
    Array.tabulate(deliveryPositions.length, warehousesPositions.length)((d, w) =>
      distance(warehousesPositions(w), deliveryPositions(d))
    )
  }

  /** Generates, for each warehouse, the distance to other warehouses.
    *
    * @param warehousesPositions
    *   The positions of the warehouses.
    */
  def warehouseToWareHouseDistances(
    warehousesPositions: Array[(Long, Long)]
  ): Array[Array[Long]] = {
    Array.tabulate(warehousesPositions.length, warehousesPositions.length)((w1, w2) =>
      distance(warehousesPositions(w1), warehousesPositions(w2))
    )
  }

}
