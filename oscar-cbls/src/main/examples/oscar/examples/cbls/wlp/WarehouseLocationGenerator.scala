/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
package oscar.examples.cbls.wlp

/**
 * Created by rdl on 23/03/2015.
 */
object WarehouseLocationGenerator {

  /**
   * this is a generator of instance for the warehouse location problem
   * @param W
   * @param D
   * @param minXY
   * @param maxXY
   * @param weightingForOpeningWarehouseCost cost for openingwarehouse = rand(0,1)*side*weightingForOpeningWarehouseCost
   * @return
   */
  def apply(W:Int,D:Int,minXY:Int = 0,maxXY:Int = 100, weightingForOpeningWarehouseCost:Int = 3):(Array[Long],Array[Array[Long]]) = {
    // we put the locations randomly on a square map
    val side = maxXY - minXY

    val costForOpeningWarehouse: Array[Long] =
      Array.tabulate(W)(w => (math.random * side * weightingForOpeningWarehouseCost).toLong)

    //we generate te cost distance matrix
    def randomXY: Long = (minXY + (math.random * side)).toLong
    def randomPosition = (randomXY, randomXY)
    val warehousePositions: Array[(Long, Long)] = Array.tabulate(W)(w => randomPosition)
    val deliveryPositions: Array[(Long, Long)] = Array.tabulate(D)(d => randomPosition)
    def distance(from: (Long, Long), to: (Long, Long)) =
      math.sqrt(math.pow(from._1 - to._1, 2) + math.pow(from._2 - to._2, 2)).toLong

    //for each delivery point, the distance to each warehouse
    val distanceCost = Array.tabulate(D)(
      d => Array.tabulate(W)(
        w => distance(warehousePositions(w), deliveryPositions(d))))

    (costForOpeningWarehouse,distanceCost)
  }

  /**
   * this is a generator of instance for the warehouse location problem
   * @param W
   * @param D
   * @param minXY
   * @param maxXY
   * @param weightingForOpeningWarehouseCost cost for openingwarehouse = rand(0,1)*side*weightingForOpeningWarehouseCost
   * @return (costForOpeningWarehouse,distanceCost,warehousePositions,deliveryPositions,warehouseToWarehouseDistances)
   */
  def problemWithPositions(W:Int,D:Int,minXY:Int = 0,maxXY:Int = 100, weightingForOpeningWarehouseCost:Int = 3):(Array[Long],Array[Array[Long]],Array[(Long,Long)],Array[(Long,Long)],Array[Array[Long]]) = {
    // we put the locations randomly on a square map
    val side = maxXY - minXY

    val costForOpeningWarehouse: Array[Long] =
      Array.tabulate(W)(w => (math.random * side * weightingForOpeningWarehouseCost).toLong)

    //we generate te cost distance matrix
    def randomXY: Long = (minXY + (math.random * side)).toLong
    def randomPosition = (randomXY, randomXY)
    val warehousePositions: Array[(Long, Long)] = Array.tabulate(W)(w => randomPosition)
    val deliveryPositions: Array[(Long, Long)] = Array.tabulate(D)(d => randomPosition)
    def distance(from: (Long, Long), to: (Long, Long)) =
      math.sqrt(math.pow(from._1 - to._1, 2) + math.pow(from._2 - to._2, 2)).toLong

    //for each delivery point, the distance to each warehouse
    val distanceCost = Array.tabulate(D)(
      d => Array.tabulate(W)(
        w => distance(warehousePositions(w), deliveryPositions(d))))

    val warehouseToWarehouseDistances = Array.tabulate(W)(
      w1 => Array.tabulate(W)(
        w2 => distance(warehousePositions(w1), warehousePositions(w2))))

    (costForOpeningWarehouse,distanceCost,warehousePositions,deliveryPositions,warehouseToWarehouseDistances)
  }

}
