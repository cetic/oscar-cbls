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

import oscar.cbls.algo.generator.GeneratorUtil.{distance, randomPosition}

import scala.collection.mutable

/** Helper class which generates random data for the WLP. Here, the warehouses are guaranteed to be
  * distant from at least `minDistanceBetweenWarehouses`.
  *
  * @param numWarehouse
  *   Number of warehouse to have in the problem.
  * @param numDelivery
  *   Number of delivery points to have in the problem.
  * @param minDistanceBetweenWarehouses
  *   The minimal distance between two warehouses. Warning: if this distance is to large in relation
  *   to the `numWarehouse`, generates all the warehouses' positions will be impossible and result
  *   to an infinite loop.
  * @param minXY
  *   Lower bound on the coordinates of the points.
  * @param maxXY
  *   Upper bound on the coordinates of the points.
  * @param weightForOpeningWarehouseCost
  *   Weight used to generate cost for opening warehouses.
  */
class WLPMinDistance(
  numWarehouse: Int,
  numDelivery: Int,
  minDistanceBetweenWarehouses: Long,
  minXY: Long = 0L,
  maxXY: Long = 100L,
  weightForOpeningWarehouseCost: Long = 3L
) extends WLPRandomGenerator(
      numWarehouse,
      numDelivery,
      minXY,
      maxXY,
      weightForOpeningWarehouseCost
    ) {

  override def generateWarehousesPositions: Array[(Long, Long)] = {
    val warehousesPositions: mutable.Queue[(Long, Long)] =
      mutable.Queue[(Long, Long)](randomPosition(minXY, maxXY, minXY, maxXY))
    for (_ <- 1 until numWarehouse) {
      var pos: (Long, Long) = (0L, 0L)
      var tries: Int        = 0
      do {
        pos = randomPosition(minXY, maxXY, minXY, maxXY)
        tries += 1
      } while (warehousesPositions.exists(p => distance(p, pos) <= minDistanceBetweenWarehouses)
        && tries < 10000)
      warehousesPositions.append(pos)
    }

    warehousesPositions.toArray
  }
}
