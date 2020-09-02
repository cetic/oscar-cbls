package oscar.cbls.lib.search.neighborhoods.vlsn

import scala.collection.immutable.SortedMap
import scala.util.Random


abstract class VLSNEnrichmentSchemeSpec(){
  def instantiate(vehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                  unroutedNodesToInsert: Set[Int]):EnrichmentScheme

}


/**
 * To prevent any gradual enrichment process in the VLSN,
 * the whole VLSN graph is then explored prior to cycle detection.
 * @param shiftInsert to shift the insert by some level of enrichment, compared to other moves.
 *                    should be >=0
 *                    This will result in the number of enrichment level to increase by shiftInsert.
 */
case class NoEnrichment(shiftInsert:Int = 0) extends VLSNEnrichmentSchemeSpec(){
  override def instantiate(vehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                           unroutedNodesToInsert: Set[Int]): EnrichmentScheme = {
    new EnrichmentScheme(
      SameSizeRandomPartitionsSpec(1).instantiate(vehicleToRoutedNodesToMove,unroutedNodesToInsert),
      SinglePassScheme(1),
      shiftInsert)
  }
}

/**
 * create a composite enrichment scheme, where the nodes are first partitioned
 * according to some BasePartitionSchemeSpec,
 * and some EnrichmentSchemeSpec specifies
 * what edges are accepted between nodes of different partitions from the base,
 * depending on the current enrichment level.
 *
 * It is also possible to specify that the insert moves can be shifted by some enrichment level,
 * compared to the node moves. This can reduce the tendency to load many nodes first before optimizing.
 * This tendency can increase the appearance of local minima.
 *
 * @param base the base partition specifying how nodes are partitioned
 * @param enrich specifies for each level of enrichment, what edges should be explored or not,
 *               based on the partitioning
 * @param shiftInsert to shift the insert by some level of enrichment, compared to other moves.
 *                    should be >=0
 *                    This will result in the number of enrichment level to increase by shiftInsert.
 */
case class CompositeEnrichmentSchemeSpec(base: BasePartitionSchemeSpec,
                                         enrich: EnrichmentSchemeSpec,
                                         shiftInsert:Int = 0) extends VLSNEnrichmentSchemeSpec(){
  override def instantiate(vehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                           unroutedNodesToInsert: Set[Int]): EnrichmentScheme = {
    require(shiftInsert >=0, "you cannot shift insert by negative number")
    val baseInst = base.instantiate(vehicleToRoutedNodesToMove,unroutedNodesToInsert)
    new EnrichmentScheme(
      baseInst,
      enrich.instantiate(baseInst.nbPartition + shiftInsert),
      shiftInsert
    )
  }
}

abstract class BasePartitionSchemeSpec(){
  def instantiate(vehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                  unroutedNodesToInsert: Set[Int]):BasePartitionScheme
}

/**
 * Specifies that the initial partitions must be based on vehicle routes:
 * one partition per route, plus one partition for unrouted nodes.
 */
case class VehiclePartitionSpec() extends BasePartitionSchemeSpec(){
  override def instantiate(vehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                           unroutedNodesToInsert: Set[Int]): BasePartitionScheme = {
    new VehiclePartition(vehicleToRoutedNodesToMove, unroutedNodesToInsert)
  }
}

case class SpreadVehiclePartitionSpec(nbPartition:Int) extends BasePartitionSchemeSpec(){
  override def instantiate(vehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                           unroutedNodesToInsert: Set[Int]): BasePartitionScheme = {
    new SpreadVehiclePartition(
      vehicleToRoutedNodesToMove,
      unroutedNodesToInsert,
      nbPartition)
  }
}


/**
 * randomly spreads the nodes (routed and unrouted) into nbPartition sets, of moreless the same size.
 *
 * @param nbPartitions the number of partitions to generate
 */
case class SameSizeRandomPartitionsSpec(nbPartitions:Int) extends BasePartitionSchemeSpec(){
  override def instantiate(vehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                           unroutedNodesToInsert: Set[Int]): BasePartitionScheme = {
    val allNodes = unroutedNodesToInsert.toList ::: vehicleToRoutedNodesToMove.toList.flatMap(_._2.toList)
    new SameSizeRandomPartitions(allNodes,nbPartitions)
  }
}

/**
 * spreads the nodes (routed and unrouted) into nbPartition sets, of more less the same size.
 * nodes of the same vehicle will have a tendency to be in the same partitions,
 * and unrouted nodes are spread evenly among all partitions
 *
 * @param nbPartitions the number of partitions to generate
 */
case class VehicleStructuredSameSizePartitionsSpreadUnroutedSpec(nbPartitions:Int) extends BasePartitionSchemeSpec(){
  override def instantiate(vehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                           unroutedNodesToInsert: Set[Int]): BasePartitionScheme = {
    VehicleStructuredSameSizePartitionsSpreadUnrouted(vehicleToRoutedNodesToMove,
      unroutedNodesToInsert,
      nbPartitions:Int)
  }
}


/**
 * this is the degenerated case where each node has its own partition,
 * so that partitions are singletons
 */
case class SingletonPartitionSpec() extends BasePartitionSchemeSpec() {
  override def instantiate(vehicleToRoutedNodesToMove: Map[Int, Set[Int]],
                           unroutedNodesToInsert: Set[Int]): BasePartitionScheme = {
    val allNodes = unroutedNodesToInsert.toList ::: vehicleToRoutedNodesToMove.toList.flatMap(_._2.toList)
    new SingletonPartition(allNodes)
  }
}

class SingletonPartition(allNodes:List[Int]) extends BasePartitionScheme{
  private val maxNodeID = allNodes.max
  private val nodeToPartition = Array.fill[Int](maxNodeID+1)(-1)
  private var remainingNodes = allNodes
  private var nextPartition:Int = 0
  while(remainingNodes.nonEmpty){
    nodeToPartition(remainingNodes.head) = nextPartition
    nextPartition += 1
    remainingNodes = remainingNodes.tail
  }
  override def nodeToPartitionId(node: Int, vehicle: Int): Int = {
    if(node == -1) 0 //just to say something.
    else nodeToPartition(node)
  }
  override def nbPartition: Int = nextPartition
}


abstract class EnrichmentSchemeSpec(){
  def instantiate(nbPartitions:Int):VLSNEnrichmentScheme
}

/**
 * all edges are to be explored in a single pass.
 * this is nearly the same as using no enrichment,
 * except that if using a single pass scheme, you can set a shiftInsert
 */
case class SinglePassSchemeSpec() extends EnrichmentSchemeSpec(){
  override def instantiate(nbPartitions: Int): VLSNEnrichmentScheme = {
    SinglePassScheme(nbPartitions)
  }
}

/**
 * specifies a random enrichment process;
 * at each level, it selects a set of pairs of partitions,
 * and all edges between any f the selected pairs of partition are allowed.
 * in addition to all the edges allowed at lower enrichment levels
 *
 * This also ensures that at all level,
 * the same number of allowed edge is more less the same
 *
 * @param nbEnrichmentLevels the number of enrichment levels to achieve
 */
case class LinearRandomSchemeSpec(nbEnrichmentLevels:Int) extends EnrichmentSchemeSpec(){
  override def instantiate(nbPartitions: Int): VLSNEnrichmentScheme = {
    val realNbSteps = nbEnrichmentLevels min (nbPartitions*(nbPartitions-1)/2)
    if(realNbSteps == 0)  SinglePassScheme(nbPartitions)
    else RandomScheme(nbPartitions,realNbSteps)
  }
}

/**
 * at each level, nodes are grouped into sets;
 * and all edges between nodes belonging to the same sets are allowed
 * in addition to all the edges allowed at lower enrichment levels
 *
 *  * the initial sets are specified by the base partitioning,
 *  and at each level, sets are merged two by two.
 *  thus going from one level eto the nest one will divide the number of sets by two,
 *  ame force the number of edges to be explored to double,
 *  compared to the number explored at the previous level
 */
case class DivideAndConquerSchemeSpec() extends EnrichmentSchemeSpec(){
  override def instantiate(nbPartitions: Int): VLSNEnrichmentScheme = {
    new DivideAndConquerScheme(nbPartitions)
  }
}




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

  enrich.checkLevel()
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

class SpreadVehiclePartition(vehicleToNodeToMove:Map[Int,Iterable[Int]],
                             unroutedNodeToInsert:Iterable[Int],
                             val nbPartition:Int)
  extends BasePartitionScheme() {
  //TODO: we might consider ventilating unrouted nodes onto partitions of some vehicles as well?

  override def nodeToPartitionId(node:Int,vehicle:Int):Int = {
    if(node == -1) 0
    else myNodeToPartitionId(node)
  }

  val myNodeToPartitionId: Array[Int] = {
    val allNodes = unroutedNodeToInsert.toList ::: vehicleToNodeToMove.values.flatten.toList
    val maxId = allNodes.max
    require(allNodes.min >= 0)
    val toReturn = Array.fill(maxId+1)(-1)

    var nextPartitionId:Int = 0
    for ((_, nodes) <- vehicleToNodeToMove) {
      for (node <- nodes) {
        toReturn(node) = nextPartitionId
        nextPartitionId += 1
        if(nextPartitionId >= nbPartition) nextPartitionId = 0
      }
    }
    for (node <- unroutedNodeToInsert) {
      toReturn(node) = nextPartitionId
      nextPartitionId += 1
      if(nextPartitionId >= nbPartition) nextPartitionId = 0
    }

    toReturn
  }
}

class VehiclePartition(vehicleToNodeToMove:Map[Int,Iterable[Int]],
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


case class VehicleStructuredSameSizePartitionsSpreadUnrouted(vehicleToNodeToMove:Map[Int,Iterable[Int]],
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

    //println("nodeToPartition:" + toReturn.mkString(","))

    (toReturn,toReturnNbPartition+1)

  }
}


abstract class VLSNEnrichmentScheme() {
  val partitionToLevel : Array[Array[Int]]
  val maxLevel:Int

  def checkLevel(): Unit ={
    require(partitionToLevel.forall(_.forall(_ <= maxLevel)))
  }
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
      if(level > maxLevel) level = maxLevel
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

    //println("enrichment scheme")
    //println(toReturn.map(_.mkString(",")).mkString("\n"))

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