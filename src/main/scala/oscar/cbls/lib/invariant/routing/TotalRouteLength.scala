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

package oscar.cbls.lib.invariant.routing

import oscar.cbls.model.routing.VRP
import oscar.cbls.core.computation.Invariant
import oscar.cbls.core.computation.seq._
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.algo.sequence.IntSequenceExplorer
import oscar.cbls.algo.sequence.RootIntSequenceExplorer
import scala.annotation.tailrec
import oscar.cbls.algo.sequence.IntSequence

/** Companion object for class TotalRouteLength
  */
object TotalRouteLength {

  /** Creates a TotalRouteLength invariant
    *
    * The TotalRouteLength invariant maintains the sum of all the route of all the vehicles
    *
    * @param vrp
    *   The object that represents the Vehicle Routing Problem
    * @param distanceMatrix
    *   The distance matrix between the points of the problem
    * @return
    *   The TotalRouteLength invariant
    */
  def apply(vrp: VRP, distanceMatrix: Array[Array[Long]]): TotalRouteLength = {
    val routeLength: IntVariable = IntVariable(vrp.model, 0)
    new TotalRouteLength(vrp, routeLength, distanceMatrix)
  }
}

/** An invariant that maintains the total route length of a vehicle routing problem
  *
  * The total route length is the sum of the routes of all the vehicles
  *
  * @param vrp
  *   The object that represents the Vehicle Routing Problem
  * @param routeLength
  *   The distance matrix between the points of the problem
  * @param distanceMatrix
  *   The TotalRouteLength invariant
  */
class TotalRouteLength(vrp: VRP, val routeLength: IntVariable, distanceMatrix: Array[Array[Long]])
    extends Invariant(vrp.model, Some("Incremental Total Route Length"))
    with SeqNotificationTarget {

  require(distanceMatrix.length == vrp.n,"The distance matrix do not contain all the distance between all the nodes")
  distanceMatrix.foreach(line => 
    require(line.length == vrp.n,"The distance matrix do not contain all the distance between all the nodes")
  )
  for (i <- 0 until vrp.n) {
    for (j <- i until vrp.n) {
      require(distanceMatrix(i)(j) == distanceMatrix(j)(i),"The distance matrix shall be symetrical")
    }
  }


  private val routes = vrp.routes
  private val v      = vrp.v
  case class CheckpointValue(level: Int, value: Long)
  private var checkpointValues: List[CheckpointValue] = List()
  private var currentValue                            = computeRouteLengthFromScratch(routes.value)

  routeLength.setDefiningInvariant(this)
  routes.registerStaticallyAndDynamicallyListeningElement(this)

  routeLength := currentValue

  private def computeRouteLengthFromScratch(seq: IntSequence): Long = {
    val exp = seq.explorerAtPosition(0).get.next
    @tailrec
    def computeLength(exp: IntSequenceExplorer = exp, prevNode: Int = 0, length: Long = 0): Long = {
      exp match {
        case _: RootIntSequenceExplorer => length
        case exp: IntSequenceExplorer =>
          if (exp.value < v) { // This is a new vehicle node
            computeLength(exp.next, exp.value, length + distanceMatrix(prevNode)(exp.value - 1))
          } else { // This is a normal node
            computeLength(exp.next, exp.value, length + distanceMatrix(prevNode)(exp.value))
          }
      }
    }
    computeLength()
  }

  override def notifySeqChanges(
    v: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit = {
    currentValue = digestUpdate(changes)
  }

  private[this] def digestUpdate(changes: SeqUpdate): Long = {
    def getNodeAfter(exp: IntSequenceExplorer): Int = {
      exp.next match {
        case _: RootIntSequenceExplorer => v - 1
        case exp: IntSequenceExplorer =>
          if (exp.value < v)
            exp.value - 1
          else
            exp.value
      }
    }
    changes match {
      case SeqUpdateInsert(insertedNode, insertAfterExp, prev) =>
        val nodeBefore = insertAfterExp.value
        val nodeAfter  = getNodeAfter(insertAfterExp)
        val delta =
          distanceMatrix(nodeBefore)(insertedNode) + distanceMatrix(insertedNode)(nodeAfter)
        digestUpdate(prev) + delta
      case SeqUpdateRemove(removedNodeExp, prev) =>
        assert(removedNodeExp.value != 0, "node 0 is a vehicle and cannot be removed")
        val nodeBefore = removedNodeExp.prev.value
        val nodeAfter  = getNodeAfter(removedNodeExp)
        val delta = distanceMatrix(nodeBefore)(nodeAfter) - distanceMatrix(nodeBefore)(
          removedNodeExp.value
        ) - distanceMatrix(removedNodeExp.value)(nodeAfter)
        digestUpdate(prev) + delta
      case SeqUpdateMove(fromExp, toExp, afterExp, flip, prev) =>
        val nodeBeforeSource = fromExp.prev.value
        val nodeAfterSource  = getNodeAfter(toExp)
        val nodeBeforeDest   = afterExp.value
        val nodeAfterDest    = getNodeAfter(afterExp)
        val (startSeg, endSeg) =
          if (flip) (toExp.value, fromExp.value) else (fromExp.value, toExp.value)
        val delta = distanceMatrix(nodeBeforeDest)(startSeg) +
          distanceMatrix(endSeg)(nodeAfterDest) -
          distanceMatrix(nodeBeforeSource)(fromExp.value) -
          distanceMatrix(toExp.value)(nodeAfterSource)
        digestUpdate(prev) + delta
      case assign: SeqUpdateAssign => computeRouteLengthFromScratch(assign.newSequence)
      case SeqUpdateDefineCheckpoint(prev, level) =>
        val length = digestUpdate(prev)
        checkpointValues = CheckpointValue(level, length) :: checkpointValues
        length
      case _: SeqUpdateLastNotified => currentValue
      case update: SeqUpdateReleaseTopCheckpoint =>
        checkpointValues = checkpointValues.tail
        digestUpdate(update.prev)
      case update: SeqUpdateRollBackToTopCheckpoint =>
        assert(checkpointValues.head.level == update.level)
        checkpointValues.head.value
    }
  }

  override def checkInternals(): Unit = {
    routeLength.pendingValue == computeRouteLengthFromScratch(routes.pendingValue)
  }

}
