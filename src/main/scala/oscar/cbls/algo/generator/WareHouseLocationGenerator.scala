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

import scala.math.{pow, round, signum, sqrt}
import scala.util.Random

object WareHouseLocationGenerator {
  private var seed: Long  = Random.nextLong()
  private val rng: Random = Random

  def setSeed(s: Long): Unit = {
    seed = s
    rng.setSeed(s)
  }

  def getSeed: Long = seed

  /** Method generating data for a WLP.
    *
    * @param numWarehouse
    *   Number of warehouse to have in the problem.
    * @param numDelivery
    *   Number of delivery points to have in the problem.
    * @param minXY
    *   Lower bound on the coordinates of the points.
    * @param maxXY
    *   Upper bound on the coordinates of the points.
    * @param weightForOpeningWarehouseCost
    *   Weight used to generate cost for opening warehouses.
    * @return
    *   An array containing the costs for opening the warehouses, a matrix of distance between the
    *   delivery points and the warehouses, the positions of the warehouses, the positions of the
    *   delivery points and a matrix of distances between each pair of warehouses.
    */
  def generateWLPWithPosition(
    numWarehouse: Int,
    numDelivery: Int,
    minXY: Long = 0L,
    maxXY: Long = 100L,
    weightForOpeningWarehouseCost: Long = 3L
  ): (
    Array[Long],
    Array[Array[Long]],
    Array[(Long, Long)],
    Array[(Long, Long)],
    Array[Array[Long]]
  ) = {
    require(minXY < maxXY, "minXy must be strictly smaller than maxXY")

    val side: Long = maxXY - minXY

    val costsForOpeningWarehouse: Array[Long] = Array.fill(numWarehouse)(
      (minXY + rng.nextDouble() * side * weightForOpeningWarehouseCost).toLong
    )

    val warehousePositions: Array[(Long, Long)] =
      Array.fill(numWarehouse)(randomPosition(minXY, maxXY, minXY, maxXY))

    val deliveryPositions: Array[(Long, Long)] =
      Array.fill(numDelivery)(randomPosition(minXY, maxXY, minXY, maxXY))

    // For each delivery point, the distance to each warehouse
    val distanceCosts: Array[Array[Long]] = Array.tabulate(numDelivery, numWarehouse)((d, w) =>
      distance(warehousePositions(w), deliveryPositions(d))
    )

    val warehouseToWarehouseDistances: Array[Array[Long]] =
      Array.tabulate(numWarehouse, numWarehouse)((w1, w2) =>
        distance(warehousePositions(w1), warehousePositions(w2))
      )

    (
      costsForOpeningWarehouse,
      distanceCosts,
      warehousePositions,
      deliveryPositions,
      warehouseToWarehouseDistances
    )

  }

  /** Method generating data for a WLP.
    *
    * @param numWarehouse
    *   Number of warehouse to have in the problem.
    * @param numDelivery
    *   Number of delivery points to have in the problem.
    * @param minXY
    *   Lower bound on the coordinates of the points.
    * @param maxXY
    *   Upper bound on the coordinates of the points.
    * @param weightForOpeningWarehouseCost
    *   Weight used to generate cost for opening warehouses.
    * @return
    *   An array containing the costs for opening the warehouses and a matrix of distance between
    *   the
    */
  def generateWLP(
    numWarehouse: Int,
    numDelivery: Int,
    minXY: Long = 0L,
    maxXY: Long = 100L,
    weightForOpeningWarehouseCost: Long = 3L
  ): (Array[Long], Array[Array[Long]]) = {
    val (warehousesCosts, distCosts, _, _, _) = generateWLPWithPosition(
      numWarehouse,
      numDelivery,
      minXY,
      maxXY,
      weightForOpeningWarehouseCost
    )

    (warehousesCosts, distCosts)
  }

  /** Return an random tuple of coordinates.
    *
    * @param xMin
    *   Inclusive lower bound of the X coordinate.
    * @param xMax
    *   Inclusive upper bound of the X coordinate.
    * @param yMin
    *   Inclusive lower bound of the Y coordinate.
    * @param yMax
    *   Inclusive upper bound of the Y coordinate.
    * @return
    *   A tuple `(x, y)` such that `xMin <= x <= xMax` and `yMin <= y <= yMax`
    */
  private def randomPosition(xMin: Long, xMax: Long, yMin: Long, yMax: Long): (Long, Long) =
    (Random.between(xMin, xMax + 1), Random.between(yMin, yMax + 1))

  private def distance(from: (Long, Long), to: (Long, Long)): Long =
    round(sqrt(pow((from._1 - to._1).toDouble, 2.0) + pow((from._2 - to._2).toDouble, 2.0)))

}
