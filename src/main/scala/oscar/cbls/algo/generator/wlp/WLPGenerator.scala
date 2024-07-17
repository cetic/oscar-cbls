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

import scala.math.{pow, round, sqrt}

/** Abstract helper class to define a generator of data for Warehouse Location problem
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
  */
private[generator] abstract class WLPGenerator(
  numWarehouse: Int,
  numDelivery: Int,
  minXY: Long = 0L,
  maxXY: Long = 100L,
  weightForOpeningWarehouseCost: Long = 3L
) {

  // The locations are randomly put on a square map
  protected val side: Long = maxXY - minXY

  /** Generates a cost of opening for each warehouse. */
  def generateCostsForOpeningWarehouse: Array[Long]

  /** Generates a position for each warehouse. */
  def generateWarehousesPositions: Array[(Long, Long)]

  /** Generates a position for each delivery point. */
  def generateDeliveryPositions: Array[(Long, Long)]

  /** Generates, for each delivery point, the distance to each warehouse. */
  def generateDistanceCosts(
    warehousesPositions: Array[(Long, Long)],
    deliveryPositions: Array[(Long, Long)]
  ): Array[Array[Long]]

  /** Generates, for each warehouse, the distance to other warehouses. */
  def generateWarehouseToWareHouseDistances(
    warehousesPositions: Array[(Long, Long)]
  ): Array[Array[Long]]

  /** Generates all the date */
  def generate: (
    Array[Long],
    Array[(Long, Long)],
    Array[(Long, Long)],
    Array[Array[Long]],
    Array[Array[Long]]
  ) = {
    val openingCost         = generateCostsForOpeningWarehouse
    val warehousesPositions = generateWarehousesPositions
    val deliveryPositions   = generateDeliveryPositions
    val distanceCosts       = generateDistanceCosts(warehousesPositions, deliveryPositions)
    val warehousesDistances = generateWarehouseToWareHouseDistances(warehousesPositions)

    (openingCost, warehousesPositions, deliveryPositions, distanceCosts, warehousesDistances)
  }

  protected def distance(from: (Long, Long), to: (Long, Long)): Long =
    round(sqrt(pow((from._1 - to._1).toDouble, 2.0) + pow((from._2 - to._2).toDouble, 2.0)))
}
