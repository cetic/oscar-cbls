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

import oscar.cbls.algo.generator.wlp.{WLPOnGrid, WLPRandomGenerator}

/** Factory to create Warehouses Location Problem data generator */
object WarehouseLocationGenerator {

  /** Generates data for a WLP.
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
  def generateRandomWLP(
    numWarehouse: Int,
    numDelivery: Int,
    minXY: Long = 0L,
    maxXY: Long = 100L,
    weightForOpeningWarehouseCost: Long = 3L
  ): WLPRandomGenerator =
    new WLPRandomGenerator(numWarehouse, numDelivery, minXY, maxXY, weightForOpeningWarehouseCost)

  /** Generates data for the WLP. The locations' map is checked with `numTilesOnSide`^2^ square
    * tiles. Warehouses positions are generated uniformly on all tiles.
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
    * @return
    *   An array containing the costs for opening the warehouses, a matrix of distance between the
    *   delivery points and the warehouses, the positions of the warehouses, the positions of the
    *   delivery points and a matrix of distances between each pair of warehouses.
    */
  def generateWLPOnGrid(
    numWarehouse: Int,
    numDelivery: Int,
    minXY: Long = 0L,
    maxXY: Long = 120L,
    weightForOpeningWarehouseCost: Long = 3L,
    numTilesOnSide: Long = 2L
  ): WLPOnGrid = new WLPOnGrid(
    numWarehouse,
    numDelivery,
    minXY,
    maxXY,
    weightForOpeningWarehouseCost,
    numTilesOnSide
  )

  def main(args: Array[String]): Unit = {
    val gen = generateWLPOnGrid(15, 30)
    val (_, w, _, _, _) = gen.generate
    println(w.mkString("\n"))

  }

}
