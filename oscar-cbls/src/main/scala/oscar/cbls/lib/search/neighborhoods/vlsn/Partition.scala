package oscar.cbls.lib.search.neighborhoods.vlsn

import scala.collection.immutable.SortedMap
import scala.util.Random

class EnrichmentScheme(base: BasePartitionScheme,
                       enrich: VLSNEnrichmentScheme,
                       shiftInsert:Int = 0){
  def moveToLevel(fromNode:Int, fromVehicle:Int, toNode:Int, toVehicle:Int):Int = {
    val baseLevel = enrich.partitionToLevel(
      base.nodeToPartitionId(fromNode,fromVehicle))(
      base.nodeToPartitionId(toNode,toVehicle))
    if(fromVehicle == -1) baseLevel + shiftInsert else baseLevel
  }
  def maxLevel:Int = enrich.maxLevel + shiftInsert
}


abstract class BasePartitionScheme(){
  def nodeToPartitionId(node:Int,vehicle:Int):Int
  def nbPartition:Int
}

class SameSizeRandomPartitions(allNodes:List[Int], override val nbPartition:Int)
  extends BasePartitionScheme() {

  val nodeToPartitionId: Array[Int] = {
    val nodes = Random.shuffle(allNodes)
    val maxId = allNodes.max
    require(allNodes.min >= 0)
    val toReturn = Array.fill(maxId+1)(-1)
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

class VehiclePartition(vehicleToNodeToMove:SortedMap[Int,Iterable[Int]],
                       unroutedNodeToInsert:Iterable[Int])
  extends BasePartitionScheme() {
  //TODO: we might consider ventilating unrouted nodes onto partitions of some vehicles as well?

  val (myNodeToPartitionId,nbPartition): (Array[Int],Int) = {
    val allNodes = unroutedNodeToInsert.toList ::: vehicleToNodeToMove.values.flatten.toList
    val maxId = allNodes.max
    require(allNodes.min >= 0)
    val toReturn = Array.fill(maxId+1)(-1)

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
    if(node == -1) 0
    else myNodeToPartitionId(node)
  }
}


case class VehicleStructuredSameSizePartitionsSpreadUnrouted(vehicleToNodeToMove:SortedMap[Int,Iterable[Int]],
                                                             unroutedNodeToInsert:Iterable[Int],
                                                             nbPartitionUpper:Int)
  extends BasePartitionScheme() {

  override def nodeToPartitionId(node:Int,vehicle:Int):Int = {
    if(node == -1) 0
    else myNodeToPartitionId(node)
  }

  val (myNodeToPartitionId,nbPartition): (Array[Int],Int) = {
    val routedNodesList:List[Int] = vehicleToNodeToMove.flatMap({case (vehicle:Int,nodes:Iterable[Int]) => nodes}).toList
    val nbRoutedNodes = routedNodesList.size
    val nbUnroutedNodeToInsert = unroutedNodeToInsert.size
    val unroutedNodeToInsertList = unroutedNodeToInsert.toList

    val ratio:Float = nbUnroutedNodeToInsert.toFloat / nbRoutedNodes.toFloat

    def randomMerge(size1:Int,list1:List[Int],size2:Int,list2:List[Int]):List[Int] = {
      (list1,list2) match{
        case (Nil,x) => x
        case (x,Nil) => x
        case (h1::t1,h2::t2) =>
          val ratioNow:Float = size1.toFloat / size2.toFloat
          if(ratioNow<ratio){
            h1::randomMerge(size1-1,t1,size2,list2)
          }else{
            h2::randomMerge(size1,list1,size2-1,t2)
          }
      }
    }

    val sortedNodes = randomMerge(nbRoutedNodes:Int,routedNodesList,nbUnroutedNodeToInsert,unroutedNodeToInsertList)

    val maxId = sortedNodes.max
    val toReturn = Array.fill(maxId+1)(-1)

    val nodesPerPartition:Int = ((nbRoutedNodes.toFloat + nbUnroutedNodeToInsert.toFloat) / nbPartitionUpper.toFloat).ceil.toInt
    var toReturnNbPartition:Int = -1

    def labelPartitions(list:List[Int],currentPartition:Int):Unit = {
      if(list.nonEmpty){
        labelCurrentPartition(nodesPerPartition,list,currentPartition)
      }
    }

    def labelCurrentPartition(nbNodesToLabel:Int,list:List[Int],currentPartition:Int):Unit = {
      if(nbNodesToLabel == 0) {
        if(list.nonEmpty){
          labelCurrentPartition(nodesPerPartition,list,currentPartition+1)
        }else{
          toReturnNbPartition = currentPartition
        }
      } else {
        list match {
          case Nil =>
            toReturnNbPartition = currentPartition
          case h :: t =>
            toReturn(h) = currentPartition
            labelCurrentPartition(nbNodesToLabel-1,t,currentPartition)
        }
      }
    }

    labelPartitions(sortedNodes,0)

    println("nodeToPartition:" + toReturn.mkString(","))
    (toReturn,toReturnNbPartition)

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

    val maxLevel = labelAndReturnLevel(Random.shuffle((0 until nbPartition).toList))

    (toReturn,maxLevel)
  }
}


case class SinglePassScheme(nbPartition:Int) extends VLSNEnrichmentScheme(){
  override val partitionToLevel: Array[Array[Int]] = Array.tabulate(nbPartition)(_ => Array.fill(nbPartition)(0))
  override val maxLevel: Int = 0
}

case class RandomScheme(nbPartition:Int, nbSteps:Int)
  extends VLSNEnrichmentScheme() {
  override val maxLevel: Int = nbSteps

  override val partitionToLevel: Array[Array[Int]] = {
    val toReturn = Array.tabulate(nbPartition)(_ => Array.fill(nbPartition)(-1))

    val partitionCoupleList:List[(Int,Int)] = (0 until nbPartition).toList.flatMap(i => (0 until i).toList.map(j => (i,j)))

    var randomizedpartitionCoupleList = Random.shuffle(partitionCoupleList)
    val nbCouplePerIt:Int = randomizedpartitionCoupleList.size / nbSteps

    var level = 0
    while(randomizedpartitionCoupleList.nonEmpty){
      var nbCouplesToDo = if(level ==0) nbCouplePerIt - toReturn.length else nbCouplePerIt
      while(nbCouplesToDo > 0 && randomizedpartitionCoupleList.nonEmpty){
        val (i,j) = randomizedpartitionCoupleList.head
        randomizedpartitionCoupleList = randomizedpartitionCoupleList.tail
        toReturn(i)(j) = level
        toReturn(j)(i) = level
        nbCouplesToDo -= 1
      }
      level += 1
    }
    while(randomizedpartitionCoupleList.nonEmpty){
      val (i,j) = randomizedpartitionCoupleList.head
      randomizedpartitionCoupleList = randomizedpartitionCoupleList.tail
      toReturn(i)(j) = maxLevel
      toReturn(j)(i) = maxLevel
    }

    for(i <- toReturn.indices){
      toReturn(i)(i) = 0
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