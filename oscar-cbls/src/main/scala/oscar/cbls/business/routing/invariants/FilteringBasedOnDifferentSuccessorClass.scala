
package oscar.cbls.business.routing.invariants

import oscar.cbls.SetValue
import oscar.cbls.business.routing.invariants.capa.{BackwardIntegerDimensionOnVehicle, ForwardCumulativeIntegerDimensionOnVehicle}
import oscar.cbls.core.computation.{CBLSIntConst, ChangingSeqValue}
import oscar.cbls.lib.invariant.logic.Filter

/**
 * Given a routing problem where each node have a class (an integer)
 * it maintains the set of route nodes such that their successor belongs to another class
 * This invariant is very useful for pre-filterig application, as a speedup for complex neighborhoods.
 */
object FilteringBasedOnDifferentSuccessorClass {
  /**
   * @param routes a route
   * @param v the number of vehicles
   * @param nodeClass the class of each node
   * @return the set of all routed nodes such that the successor belongs to another class
   */
  def apply(routes: ChangingSeqValue,
            v: Int,
            nodeClass: Int => Int): SetValue = {

    val nodeHasSuccessorOfDifferentClass = BackwardIntegerDimensionOnVehicle(routes: ChangingSeqValue,
      v,
      routes.model,
      defaultForUnrouted = 0,
      finalValueAtReturn = Array.fill(v)(0),
      fonc = {
        case (node, nextNode, _) => (if (nodeClass(node) != nodeClass(nextNode)) 1 else 0)
      }).contentAtEachNode

    Filter(nodeHasSuccessorOfDifferentClass)
  }
}

//This is DRAFT
object FilteringBasedOnDifferentPredecessorClass {
  /**
   * @param routes a route
   * @param v the number of vehicles
   * @param nodeClass the class of each node
   * @return the set of all routed nodes such that the predecessor belongs to another class
   */
  def apply(routes: ChangingSeqValue,
            n:Int,
            v: Int,
            nodeClass: Int => Int): SetValue = {

    val (nodeHasPredecessorOfDifferentClass,_,_,_) =
      ForwardCumulativeIntegerDimensionOnVehicle(routes,
        n = n,
        v = n,
        op = {
          case (node, nextNode, _) => (if (nodeClass(node) != nodeClass(nextNode)) 1 else 0)},
        contentAtStart = Array.fill(v)(CBLSIntConst(0)),
        defaultForUnroutedNodes = 0)

    Filter(nodeHasPredecessorOfDifferentClass)
  }
}
