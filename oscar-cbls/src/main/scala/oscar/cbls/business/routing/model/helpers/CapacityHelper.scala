package oscar.cbls.business.routing.model.helpers

import oscar.cbls.business.routing.invariants.capa.ForwardCumulativeConstraintOnVehicle
import oscar.cbls.business.routing.model.VRP
import oscar.cbls._
import scala.collection.immutable.HashSet

/**
  * Created by fg on 12L/09L/1L7.
  */
object CapacityHelper{

  /**
    * This method generates a method that can be used to determine whether or not their is enough space to insert a node after a neighbor.
    * It uses the freeSpaceAtNodes method of the capacityInvariant constraint to know the actual free space after a specific node.
    *
    * @param n The amount of node in the problem
    * @param capacityInvariant A ForwardCumulativeConstraintOnVehicle representing the content invariant
    * @return a method (Long,Long,Array[Long]) => Boolean
    */
  // TODO remove this when the future global constraint is implemented. This method first purpose was to avoid calling the capacity constraint method which was very slow
  def enoughSpaceAfterNeighbor(n:Long, capacityInvariant: ForwardCumulativeConstraintOnVehicle): (Long,Long,Array[Long]) => Boolean ={
    val freeSpaceAtNodeNow = capacityInvariant.freeSpaceAtNodes
    (node: Long, neighbor: Long, contentsFlow: Array[Long]) => freeSpaceAtNodeNow(neighbor) >= contentsFlow(node)
  }

  /**
    * Given the max capacity of each vehicle and the content linked to each node, prune the relevant predecessor of a node.
    * Relevant if :
    *   - content(node) <= maxCapacities(vehicle)
    *   - content(node) + content(predecessor) <= maxCapacities.max
    * @param vrp
    * @param maxCapacity
    * @param contentsFlow
    * @return
    */

  // TODO : Move this in the companion object of the future global constraint
  def relevantPredecessorsOfNodes(vrp: VRP, maxCapacity : Long, vehiclesSize: Array[Long], contentsFlow: Array[Long]): Map[Long,HashSet[Long]] ={
    Array.tabulate(vrp.n)(node => intToLong(node) -> HashSet(vrp.nodes.collect {
      case predecessor if
      ((if(predecessor < vrp.v) maxCapacity - vehiclesSize(predecessor) else contentsFlow(predecessor)) + contentsFlow(node)) <= maxCapacity &&
        predecessor != node => intToLong(predecessor)
    }: _*)).toMap
  }

}
