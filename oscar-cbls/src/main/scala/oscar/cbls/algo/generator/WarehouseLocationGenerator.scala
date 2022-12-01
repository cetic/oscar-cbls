package oscar.cbls.algo.generator

import scala.util.Random

/**
 * Created by rdl on 23/03/2015.
 */
object WarehouseLocationGenerator {
  val random = new Random(42)

  /**
   * this is a generator of instances for the warehouse location problem
   *
   * @param W
   * @param D
   * @param minXY
   * @param maxXY
   * @param weightingForOpeningWarehouseCost cost for openingwarehouse = rand(0,1)*side*weightingForOpeningWarehouseCost
   * @return
   */
  def apply(W: Int, D: Int, minXY: Int = 0, maxXY: Int = 100, weightingForOpeningWarehouseCost: Int = 3): (Array[Long], Array[Array[Long]]) = {
    // we put the locations randomly on a square map
    val side = maxXY - minXY

    val costForOpeningWarehouse: Array[Long] =
      Array.tabulate(W)(w => (random.nextDouble() * side * weightingForOpeningWarehouseCost).toLong)

    //we generate te cost distance matrix
    def randomXY: Long = (minXY + (random.nextDouble() * side)).toLong

    def randomPosition = (randomXY, randomXY)

    val warehousePositions: Array[(Long, Long)] = Array.tabulate(W)(w => randomPosition)
    val deliveryPositions: Array[(Long, Long)] = Array.tabulate(D)(d => randomPosition)

    def distance(from: (Long, Long), to: (Long, Long)): Long =
      math.sqrt(math.pow((from._1 - to._1).toDouble, 2.0) + math.pow((from._2 - to._2).toDouble, 2.0)).toLong

    //for each delivery point, the distance to each warehouse
    val distanceCost = Array.tabulate(D)(
      d => Array.tabulate(W)(
        w => distance(warehousePositions(w), deliveryPositions(d))
      )
    )

    (costForOpeningWarehouse, distanceCost)
  }

  /**
   * this is a generator of instance for the warehouse location problem
   *
   * @param W
   * @param D
   * @param minXY
   * @param maxXY
   * @param weightingForOpeningWarehouseCost cost for openingwarehouse = rand(0,1)*side*weightingForOpeningWarehouseCost
   * @return (costForOpeningWarehouse,distanceCost,warehousePositions,deliveryPositions,warehouseToWarehouseDistances)
   */
  def problemWithPositions(W: Int, D: Int, minXY: Int = 0, maxXY: Int = 100, weightingForOpeningWarehouseCost: Int = 3): (Array[Long], Array[Array[Long]], Array[(Long, Long)], Array[(Long, Long)], Array[Array[Long]]) = {
    // we put the locations randomly on a square map
    val side = maxXY - minXY

    val costForOpeningWarehouse: Array[Long] =
      Array.tabulate(W)(_ => (math.random() * side * weightingForOpeningWarehouseCost).toLong)

    //we generate te cost distance matrix
    def randomXY: Long = (minXY + (random.nextDouble() * side)).toLong

    def randomPosition = (randomXY, randomXY)

    val warehousePositions: Array[(Long, Long)] = Array.tabulate(W)(_ => randomPosition)
    val deliveryPositions: Array[(Long, Long)] = Array.tabulate(D)(_ => randomPosition)

    def distance(from: (Long, Long), to: (Long, Long)): Long =
      math.sqrt(math.pow((from._1 - to._1).toDouble, 2.0) + math.pow((from._2 - to._2).toDouble, 2.0)).toLong

    //for each delivery point, the distance to each warehouse
    val distanceCost = Array.tabulate(D)(
      d => Array.tabulate(W)(
        w => distance(warehousePositions(w), deliveryPositions(d))
      )
    )

    //For each warehouse, the distance to other warehouses
    val warehouseToWarehouseDistances = Array.tabulate(W)(
      w1 => Array.tabulate(W)(
        w2 => distance(warehousePositions(w1), warehousePositions(w2))
      )
    )

    (costForOpeningWarehouse, distanceCost, warehousePositions, deliveryPositions, warehouseToWarehouseDistances)
  }

}
