package oscar.cbls.lib.search.neighborhoods.vlsn

import scala.collection.immutable.SortedMap
import scala.util.Random

abstract class VLSNPartition() {
  /**
   *
   * @param fromNode the moved node
   * @param fromVehicle the from vehicle; -1 if unrouted
   * @param toNode the ejected node; -1 is no such node
   * @param toVehicle the target vehicle; -1 if unrouting the fromNode
   * @return the level of partition for the move to be acceptable WRT. the partitioning
   */
  def moveToPartitionLevel(fromNode:Int,fromVehicle:Int,toNode:Int,toVehicle:Int):Int

  def sameSizePartitions(allNodes:List[Int], nbPartition:Int):Array[Int] = {
    val nodes = Random.shuffle(allNodes)
    val maxId = allNodes.max
    require(allNodes.min >=0)
    val toReturn = Array.fill(maxId)(-1)
    var currentPartition = nbPartition-1
    for(node <- nodes){
      toReturn(node) = currentPartition
      if(currentPartition == 0) currentPartition = nbPartition
      currentPartition = currentPartition-1
    }
    toReturn
  }

  def vehiclePartition(vehicleToNodeToMove:SortedMap[Int,List[Int]],
                               unroutedNodeToInsert:List[Int]):(Array[Int],Int) = {

    val allNodes = unroutedNodeToInsert ::: vehicleToNodeToMove.values.flatten.toList
    val maxId = allNodes.max
    require(allNodes.min >=0)
    val toReturn = Array.fill(maxId)(-1)

    var nextPartitionId = 0
    for((_,nodes) <- vehicleToNodeToMove){
      for(node <- nodes) {
        toReturn(node) = nextPartitionId
      }
      nextPartitionId += 1
    }
    for(node <- unroutedNodeToInsert){
      toReturn(node) = nextPartitionId
    }

    (toReturn,nextPartitionId+1)
  }
}

class DivideAndConquerPartition(vehicleToNodeToMove:SortedMap[Int,List[Int]],
                                unroutedNodeToInsert:List[Int],
                                nbPartitions:Int)
  extends VLSNPartition {


  //put vehicles randomply in an array
  val vehicleArray = Random.shuffle(vehicleToNodeToMove.keys).zipWithIndex.map(a => a)

  //split according to vehicles; ventilate unoutedNodes on upper partition levels

  def moveToPartitionLevel(fromNode:Int,fromVehicle:Int,toNode:Int,toVehicle:Int):Int =
}

class RollingPartition(vehicleToNodeToMove:SortedMap[Int,List[Int]],
                       unroutedNodeToInsert:List[Int],
                       nbPartitions:Int)
  extends VLSNPartition {


  //put vehicles randomply in an array
  val vehicleArray = Random.shuffle(vehicleToNodeToMove.keys).zipWithIndex.map(a => a)

  //split according to vehicles; ventilate unoutedNodes on upper partition levels

  def moveToPartitionLevel(fromNode:Int,fromVehicle:Int,toNode:Int,toVehicle:Int):Int =
}
