package oscar.cbls.lib.search.neighborhoods.vlsn

import scala.collection.immutable.SortedMap
import scala.util.Random

class EnrichmentScheme(base: BasePartitionScheme,
                       enrich: VLSNEnrichmentScheme){
  def moveToLevel(fromNode:Int, fromVehicle:Int, toNode:Int, toVehicle:Int):Int = {
    enrich.partitionToLevel(
      base.nodeToPartitionId(fromNode,fromVehicle))(
      base.nodeToPartitionId(toNode,toVehicle))
  }
}

abstract class BasePartitionScheme(){
  def nodeToPartitionId(node:Int,vehicle:Int):Int
  def nbPartition:Int
}

class SameSizePartitions(allNodes:List[Int], override val nbPartition:Int)
  extends BasePartitionScheme() {

  val nodeToPartitionId: Array[Int] = {
    val nodes = Random.shuffle(allNodes)
    val maxId = allNodes.max
    require(allNodes.min >= 0)
    val toReturn = Array.fill(maxId)(-1)
    var currentPartition = nbPartition - 1
    for (node <- nodes) {
      toReturn(node) = currentPartition
      if (currentPartition == 0) currentPartition = nbPartition
      currentPartition = currentPartition - 1
    }
    toReturn
  }

  override def nodeToPartitionId(node:Int,vehicle:Int):Int = {
    if(node == -1) 0 //just to say something.
    else nodeToPartitionId(node)
  }
}

class VehiclePartition(vehicleToNodeToMove:SortedMap[Int,List[Int]],
                       unroutedNodeToInsert:List[Int])
  extends BasePartitionScheme() {
  //TODO: we might consider ventilating unrouted nodes onto partitions of some vehicles as well?

  val (myNodeToPartitionId,nbPartition): (Array[Int],Int) = {
    val allNodes = unroutedNodeToInsert ::: vehicleToNodeToMove.values.flatten.toList
    val maxId = allNodes.max
    require(allNodes.min >= 0)
    val toReturn = Array.fill(maxId)(-1)

    var nextPartitionId = 0
    for ((_, nodes) <- vehicleToNodeToMove) {
      for (node <- nodes) {
        toReturn(node) = nextPartitionId
      }
      nextPartitionId += 1
    }
    for (node <- unroutedNodeToInsert) {
      toReturn(node) = nextPartitionId //unrouted nodes get partition nbPartition-1
    }

    (toReturn, nextPartitionId + 1)
  }

  override def nodeToPartitionId(node:Int,vehicle:Int):Int = {
    if(node == -1) (nbPartition-1) //that's the urouted partition
    else myNodeToPartitionId(node)
  }
}

abstract class VLSNEnrichmentScheme() {
  val partitionToLevel : Array[Array[Int]]
  val maxLevel:Int
}

class DivideAndConquerScheme(nbPartition:Int)
  extends VLSNEnrichmentScheme {

  override val (partitionToLevel,maxLevel) : (Array[Array[Int]],Int) = {

    val toReturn = Array.tabulate(nbPartition)(_ => Array.fill(nbPartition)(-1))

    def labelAndReturnLevel(partitions:List[Int]): Int = {
      val size = partitions.size
      if (size <= 2) {
        for(a <- partitions){
          for(b <- partitions){
            toReturn(a)(b) = 1
            toReturn(b)(a) = 1
          }
        }
        1
      }else {
        val (left, right) = partitions.splitAt(size / 2)
        val myLevel = (labelAndReturnLevel(left) max labelAndReturnLevel(right)) + 1

        for (a <- left) {
          for (b <- right) {
            toReturn(a)(b) = myLevel
            toReturn(b)(a) = myLevel
          }
        }
        myLevel
      }
    }

    labelAndReturnLevel(Random.shuffle((0 until nbPartition).toList))

    toReturn
  }
}


class RandomScheme(nbPartition:Int, nbSteps:Int)
  extends VLSNEnrichmentScheme() {
  override val maxLevel: Int = nbSteps

  override val partitionToLevel: Array[Array[Int]] = {
    val toReturn = Array.tabulate(nbPartition)(_ => Array.fill(nbPartition)(-1))

    val partitionCoupleList:List[(Int,Int)] = (0 to nbPartition).toList.flatMap(i => (0 until i).toList.map(j => (i,j)))

    var randomizedpartitionCoupleList = Random.shuffle(partitionCoupleList)
    val nbCouplePerIt:Int = randomizedpartitionCoupleList.size / nbSteps

    var level = 0
    while(randomizedpartitionCoupleList.nonEmpty){
      var nbCouplesToDo = nbCouplePerIt
      while(nbCouplePerIt >0 && randomizedpartitionCoupleList.nonEmpty){
        val (i,j) = randomizedpartitionCoupleList.head
        randomizedpartitionCoupleList = randomizedpartitionCoupleList.tail
        toReturn(i)(j) = level
        toReturn(i)(i) = level
      }
      level += 1
    }
    while(randomizedpartitionCoupleList.nonEmpty){
      val (i,j) = randomizedpartitionCoupleList.head
      randomizedpartitionCoupleList = randomizedpartitionCoupleList.tail
      toReturn(i)(j) = maxLevel
      toReturn(i)(i) = maxLevel
    }
    toReturn
  }

}

/*
class CaterpillarScheme(nbPartition:Int)
  extends VLSNEnrichmentScheme() {

  override val (partitionToLevel,maxLevel) : (Array[Array[Int]],Int) = {
    val toReturn = Array.tabulate(nbPartition)(_ => Array.fill(nbPartition)(-1))

    val partitionList = Random.shuffle((0 until nbPartition).toList).toArray

    for(shift <- 0 until nbPartition){

      if(shift%2 == 0){
        val partitionList
      }else{
      }
    }
  }
}
*/