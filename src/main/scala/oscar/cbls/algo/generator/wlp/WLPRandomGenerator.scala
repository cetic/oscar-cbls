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

package oscar.cbls.algo.generator.wlp

import scala.util.Random

/** Class used to generate random data for a Warehouse Location Problem *
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
  */
class WLPRandomGenerator(
  numWarehouse: Int,
  numDelivery: Int,
  minXY: Long = 0L,
  maxXY: Long = 100L,
  weightForOpeningWarehouseCost: Long = 3L
) extends WLPGenerator(numWarehouse, numDelivery, minXY, maxXY, weightForOpeningWarehouseCost) {

  protected var seed: Long = Random.nextLong()
  protected val rng        = new Random(seed)

  override def generateCostsForOpeningWarehouse: Array[Long] = Array.fill(numWarehouse)(
    (minXY + rng.nextDouble() * side * weightForOpeningWarehouseCost).toLong
  )

  override def generateWarehousesPositions: Array[(Long, Long)] =
    Array.fill(numWarehouse)(randomPosition(minXY, maxXY, minXY, maxXY))

  override def generateDeliveryPositions: Array[(Long, Long)] = {
    Array.fill(numDelivery)(randomPosition(minXY, maxXY, minXY, maxXY))
  }

  override def generateDistanceCosts(
    warehousesPositions: Array[(Long, Long)],
    deliveryPositions: Array[(Long, Long)]
  ): Array[Array[Long]] = Array.tabulate(numDelivery, numWarehouse)((d, w) =>
    distance(warehousesPositions(w), deliveryPositions(d))
  )

  override def generateWarehouseToWareHouseDistances(
    warehousesPositions: Array[(Long, Long)]
  ): Array[Array[Long]] = Array.tabulate(numWarehouse, numWarehouse)((w1, w2) =>
    distance(warehousesPositions(w1), warehousesPositions(w2))
  )

  def setSeed(s: Long): Unit = {
    seed = s
    rng.setSeed(s)
  }

  def getSeed: Long = seed

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
  protected def randomPosition(xMin: Long, xMax: Long, yMin: Long, yMax: Long): (Long, Long) =
    (Random.between(xMin, xMax + 1), Random.between(yMin, yMax + 1))

}
