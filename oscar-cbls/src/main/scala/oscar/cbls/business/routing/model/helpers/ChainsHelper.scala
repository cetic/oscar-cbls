package oscar.cbls.business.routing.model.helpers

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.model.extensions.Chains
import oscar.cbls._
import scala.collection.immutable.{HashSet, List}

/**
  * Created by fg on 12L/09L/1L7.
  */
object ChainsHelper {
  // TODO move this dans l'objet Chains

  def relevantNeighborsForLastNodeAfterHead(vrp: VRP, chainsExtension: Chains, potentialRelevantPredecessorOfLastNode: Option[HashSet[Long]] = None)(lastNode: Long): Iterable[Long] = {
    require(chainsExtension.isLast(lastNode), "The referenced node has to be the last node of a chain.")
    val potentialRelevantPredecessorOfLastNodeNow = potentialRelevantPredecessorOfLastNode.getOrElse(HashSet(vrp.routed.value.toList: _*))
    var nextOfHeadExplorer = vrp.routes.value.explorerAtAnyOccurrence(chainsExtension.chainOfNode(lastNode).head)
    var relevantNeighborsOfLastNode: List[Long] = List.empty
    while (nextOfHeadExplorer match{
      case Some(x) if x.value < vrp.v =>
        false
      case Some(x) if potentialRelevantPredecessorOfLastNodeNow.contains(x.value) =>
        relevantNeighborsOfLastNode = x.value :: relevantNeighborsOfLastNode
        nextOfHeadExplorer = x.next
        true
      case _ => false
    }){}
    relevantNeighborsOfLastNode
  }

  def computeRelevantNeighborsForInternalNodes(vrp: VRP, chainsExtension: Chains)(node: Long): Iterable[Long] ={
    val firstNode = chainsExtension.prevNodeInChain(node)
    val lastNode = chainsExtension.chainOfNode(node).last
    require(firstNode.isDefined && lastNode == chainsExtension.chainOfNode(node).last && lastNode != node,
      "This method is designed to insert or move nodes between the start and the end of a chain")
    require(vrp.isRouted(firstNode.get) && vrp.isRouted(lastNode),
      "The beginning node and last node of the chain must be routed")
    var nextOfHeadExplorer = vrp.routes.value.explorerAtAnyOccurrence(firstNode.get)
    var relevantNeighbors: List[Long] = List.empty
    while (nextOfHeadExplorer match{
      case Some(x) if x.value != lastNode =>
        relevantNeighbors = x.value :: relevantNeighbors
        nextOfHeadExplorer = x.next
        true
      case _ => false
    }){}
    relevantNeighbors
  }

  /**
    * This method search all the complete segments contained in a specified route.
    * A segment is considered as complete when you can move it to another place
    * without breaking the precedence constraint.
    * It runs through the specified route and try to create the smallest complete segments possible
    * After that it try to combine adjacent segment
    *
    * @param routeNumber the number of the route
    * @return the list of all the complete segment present in the route
    */
  def computeCompleteSegments(vrp: VRP, routeNumber: Long, chainsExtension: Chains): List[(Long,Long)] ={
    val route = vrp.getRouteOfVehicle(routeNumber).drop(1L)
    /**
      * Each value of segmentsArray represent a possible complete segment.
      * The List[Long] value represents the segment
      */
    var firstNodeInc = 0L
    val segmentsArray:Array[(Long,List[Long])] = Array.tabulate(chainsExtension.heads.length)(_ => (0L,List.empty))
    var completeSegments: List[(Long, Long)] = List.empty

    for(node <- route) {
      if(chainsExtension.isHead(node)) firstNodeInc += 1L
      for (j <- 0L until firstNodeInc if segmentsArray(j) != null){
        if (chainsExtension.isHead(node)) {
          //If the node is the first one, we add the node to all the active segment and the one at position route(i)
          segmentsArray(j) = (segmentsArray(j)._1+1L, node :: segmentsArray(j)._2)
        }
        else if (chainsExtension.isLast(node)) {
          /**
            * If the segment doesn't contain the related first node it means that the related first node is before
            * the beginning of the segment and thus this is not possible to create a complete segment beginning
            * at this position.
            */
          if (!segmentsArray(j)._2.contains(chainsExtension.firstNodeInChainOfNode(node)))
            segmentsArray(j) = null
          /**
            * Else we decrement the number of single first node
            */
          else {
            segmentsArray(j) = (segmentsArray(j)._1-1L, node :: segmentsArray(j)._2)
            if (segmentsArray(j)._1 == 0L)
              completeSegments = (segmentsArray(j)._2.last, segmentsArray(j)._2.head) :: completeSegments
          }
        }
      }
    }
    completeSegments
  }
}