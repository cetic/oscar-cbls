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

import oscar.cbls.algo.generator.GeneratorUtil.randomPosition

/** Helper class which generates random data for the WLP. The locations' map is checked with
  * `numTilesOnSide`^2^ square tiles. Warehouses positions are generated uniformly on all tiles.
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
  * @param numTilesOnSide
  *   The number of tiles along the grid side. The map is supposed to be square.
  */
class WLPOnGrid(
  numWarehouse: Int,
  numDelivery: Int,
  minXY: Long = 0L,
  maxXY: Long = 120L,
  weightForOpeningWarehouseCost: Long = 3L,
  numTilesOnSide: Long = 2L
) extends WLPRandomGenerator(
      numWarehouse,
      numDelivery,
      minXY,
      maxXY,
      weightForOpeningWarehouseCost
    ) {

  override def generateWarehousesPositions: Array[(Long, Long)] = {
    val totalNumberOfTiles: Long = numTilesOnSide * numTilesOnSide
    val numUniformWareHouseOnGrid: Long =
      numWarehouse.toLong - (numWarehouse.toLong % totalNumberOfTiles)
    val tileLength: Long                        = side / numTilesOnSide
    val warehousePositions: Array[(Long, Long)] = new Array[(Long, Long)](numWarehouse)

    var (x, y): (Long, Long) = (0L, 0L)
    var n: Long              = 0L
    // First, each tile receives the same number of warehouse.
    while (n < numUniformWareHouseOnGrid) {
      y = (n % numTilesOnSide) * tileLength
      warehousePositions(n.toInt) = randomPosition(x, x + tileLength, y, y + tileLength)
      n += 1L
      // If each tile of the grid received a warehouse, we restart from the tile 0
      if (n % totalNumberOfTiles == 0L) x = 0L
      // If each tile of a column received a warehouse, we move to the following column
      else if (n % numTilesOnSide == 0L) x += tileLength
    }

    // The remainder of the warehouse are placed randomly.
    for (i <- numUniformWareHouseOnGrid.toInt until numWarehouse)
      warehousePositions(i) = randomPosition(minXY, maxXY, minXY, maxXY)

    warehousePositions
  }
}
