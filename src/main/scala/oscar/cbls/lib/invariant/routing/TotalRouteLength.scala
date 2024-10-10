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

  private def isSymmetrical(n: Int, distanceMatrix: Int => Int => Long): Boolean = {
    var res = true
    for (i <- 0 until n) {
      for (j <- i until n) {
        res = res & distanceMatrix(i)(j) == distanceMatrix(j)(i)
      }
    }
    res
  }

  /** Creates a TotalRouteLength invariant.
    *
    * The TotalRouteLength invariant maintains the sum of all the route of all the vehicles.
    *
    * @param vrp
    *   The object that represents the Vehicle Routing Problem.
    * @param distanceMatrix
    *   The distance matrix between the points of the problem.
    * @return
    *   The TotalRouteLength invariant.
    */
  def apply(vrp: VRP, distanceMatrix: Int => Int => Long): TotalRouteLength = {
    val routeLength: IntVariable = IntVariable(vrp.model, 0)
    val matrixIsSymmetrical      = isSymmetrical(vrp.n, distanceMatrix)
    new TotalRouteLength(vrp, routeLength, distanceMatrix, matrixIsSymmetrical)
  }

  /** Creates a TotalRouteLength invariant.
    *
    * The TotalRouteLength invariant maintains the sum of all the route of all the vehicles.
    *
    * @param vrp
    *   The object that represents the Vehicle Routing Problem.
    * @param distanceMatrix
    *   The distance matrix between the points of the problem.
    * @return
    *   The TotalRouteLength invariant.
    */
  def apply(vrp: VRP, distanceMatrix: Array[Array[Long]]): TotalRouteLength = {
    val routeLength: IntVariable = IntVariable(vrp.model, 0)
    val matrixIsSymmetrical = isSymmetrical(vrp.n, (i: Int) => (j: Int) => distanceMatrix(i)(j))
    new TotalRouteLength(
      vrp,
      routeLength,
      (i: Int) => (j: Int) => distanceMatrix(i)(j),
      matrixIsSymmetrical
    )
  }

  /** Creates a TotalRouteLength invariant.
    *
    * The TotalRouteLength invariant maintains the sum of all the route of all the vehicles.
    *
    * @param vrp
    *   The object that represents the Vehicle Routing Problem.
    * @param distanceFunction
    *   A function that, given two nodes, returns the distance between the two nodes.
    * @return
    *   The TotalRouteLength invariant.
    */
  def apply(
    vrp: VRP,
    distanceFunction: Int => Int => Long,
    matrixIsSymmetrical: Boolean
  ): TotalRouteLength = {
    val routeLength: IntVariable = IntVariable(vrp.model, 0)
    new TotalRouteLength(vrp, routeLength, distanceFunction, matrixIsSymmetrical)
  }

  /** Creates a TotalRouteLength invariant.
    *
    * The TotalRouteLength invariant maintains the sum of all the route of all the vehicles.
    *
    * @param vrp
    *   The object that represents the Vehicle Routing Problem.
    * @param distanceFunction
    *   A function that, given two nodes, returns the distance between the two nodes.
    * @return
    *   The TotalRouteLength invariant.
    */
  def apply(
    vrp: VRP,
    distanceMatrix: Array[Array[Long]],
    matrixIsSymmetrical: Boolean
  ): TotalRouteLength = {
    val routeLength: IntVariable = IntVariable(vrp.model, 0)
    new TotalRouteLength(
      vrp,
      routeLength,
      (i: Int) => (j: Int) => distanceMatrix(i)(j),
      matrixIsSymmetrical
    )
  }
}

/** An invariant that maintains the summed length of all the routes of the VRP.
  *
  * The total route length is the sum of the routes of all the vehicles. Beware, this route length
  * constraint is more efficient if the distance function is '''symmetrical''' (i.e. given two nodes
  * `i` and `j`, `distanceFunction(i)(j) == distanceFunction(j)(i)`).
  *
  * The nodes may have a weight that is added to the length of the routen. In this case, the weights
  * of the nodes should be put in the '''diagonal''' of the distance function (i.e. the weight of
  * the node `i` is given by `distanceFunction(i)(i)`).
  *
  * @param vrp
  *   The object that represents the Vehicle Routing Problem.
  * @param routeLength
  *   The [[oscar.cbls.core.computation.IntVariable]] that is maintained by the invariant.
  * @param distanceFunction
  *   A function that, given two nodes, returns the distance between the two nodes.
  * @param matrixIsSymmetrical
  *   A flag that says if the matrix is symmetrical.
  */
class TotalRouteLength(
  vrp: VRP,
  val routeLength: IntVariable,
  distanceFunction: Int => Int => Long,
  matrixIsSymmetrical: Boolean
) extends Invariant(vrp.model, Some("Incremental Total Route Length"))
    with SeqNotificationTarget {

  if (matrixIsSymmetrical) {
    for (i <- 0 until vrp.n) {
      for (j <- i until vrp.n) {
        require(
          distanceFunction(i)(j) == distanceFunction(j)(i),
          "The distance matrix shall be symmetrical"
        )
      }
    }
  }

  private val routes = vrp.routes
  private val v      = vrp.v
  // Defining a structure for the checkpoints
  case class CheckpointValue(level: Int, value: Long)
  private var checkpointValues: List[CheckpointValue] = List()

  // Saving the current value
  private var currentValue = computeRouteLengthFromScratch(routes.value())

  routeLength.setDefiningInvariant(this)
  routes.registerStaticallyAndDynamicallyListeningElement(this)

  routeLength := currentValue

  /** Computes the route length from scratch (without incremental computing, by going through the
    * sequence).
    *
    * This method can compute either the entire route length or the route length on a segment of the
    * route. The start and end of this segment can be given as parameter. If the flag backward is
    * true, the distance is computed backward in the sequence.
    *
    * @param seq
    *   The sequence representing the route.
    * @param fromExpl
    *   The optional start of the segment.
    * @param toExpl
    *   The optinal end of the segment.
    * @param backward
    *   A flag that says if the distance shall be computed backward.
    * @return
    *   The length of the route.
    */
  private def computeRouteLengthFromScratch(
    seq: IntSequence,
    fromExpl: Option[IntSequenceExplorer] = None,
    toExpl: Option[IntSequenceExplorer] = None,
    backward: Boolean = false
  ): Long = {
    val startExp = fromExpl.getOrElse(seq.explorerAtPosition(0).get)
    val exp      = if (backward) startExp.prev else startExp.next
    @tailrec
    def computeLength(
      exp: IntSequenceExplorer = exp,
      prevNode: Int = startExp.value,
      length: Long = distanceFunction(startExp.value)(startExp.value)
    ): Long = {
      exp match {
        case _: RootIntSequenceExplorer =>
          length + (if (prevNode != vrp.v - 1) distanceFunction(prevNode)(vrp.v - 1) else 0)
        case exp: IntSequenceExplorer =>
          val nextExp = if (backward) exp.prev else exp.next
          toExpl match {
            case None =>
              if (exp.value < v) { // This is a new vehicle node
                computeLength(
                  nextExp,
                  exp.value,
                  length +
                    distanceFunction(exp.value)(exp.value) +
                    (if (prevNode != exp.value - 1)
                       distanceFunction(prevNode)(exp.value - 1)
                     else 0)
                )
              } else { // This is a normal node
                computeLength(
                  nextExp,
                  exp.value,
                  length +
                    distanceFunction(prevNode)(exp.value) +
                    distanceFunction(exp.value)(exp.value)
                )
              }
            case Some(e) =>
              if (e.value == exp.value) {
                length + distanceFunction(e.value)(e.value) + distanceFunction(prevNode)(e.value)
              } else {
                if (exp.value < v) { // This is a new vehicle node
                  computeLength(
                    nextExp,
                    exp.value,
                    length +
                      distanceFunction(exp.value)(exp.value) +
                      (if (prevNode != exp.value - 1)
                         distanceFunction(prevNode)(exp.value - 1)
                       else
                         0)
                  )
                } else { // This is a normal node
                  computeLength(
                    nextExp,
                    exp.value,
                    length +
                      distanceFunction(prevNode)(exp.value) +
                      distanceFunction(exp.value)(exp.value)
                  )
                }

              }
          }
      }
    }
    computeLength()
  }

  /** Notifies this invariant that the listened [[oscar.cbls.core.computation.seq.SeqVariable]] has
    * changed.
    *
    * @param v
    *   The listened SeqVariable.
    * @param contextualVarIndex
    *   The optional index of the SeqVariable in the context of the listening Invariant. Default -1.
    * @param changes
    *   A stacked list of SeqUpdate, the first one represents the latest update. Use its prev value
    *   to get the previous SeqUpdate...
    */
  override def notifySeqChanges(
    v: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit = {
    currentValue = digestUpdate(changes)
    routeLength := currentValue
  }

  /** Handles the updates.
    *
    * This methods digests inductively the previous updates before treating a given update. In some
    * cases (mainly when rolling back to checkpoint), we do not care about computing the deltas: in
    * this case, the flag `computeDelta` is false.
    *
    * @param changes
    *   The inductive changes that had been made on the sequence.
    * @param computeDelta
    *   A flag that says if the method shall compute the delta.
    * @return
    *   The new length of the sequence.
    */
  private[this] def digestUpdate(changes: SeqUpdate, computeDelta: Boolean = true): Long = {
    changes match {
      case SeqUpdateInsert(insertedNode, insertAfterExp, prev) =>
        if (computeDelta) {
          val nodeBefore = insertAfterExp.value
          val nodeAfter  = vrp.nextNodeInRouting(insertAfterExp)
          val delta: Long =
            distanceFunction(nodeBefore)(insertedNode) +
              distanceFunction(insertedNode)(nodeAfter) -
              (if (nodeBefore != nodeAfter) distanceFunction(nodeBefore)(nodeAfter) else 0) +
              distanceFunction(insertedNode)(insertedNode)
          digestUpdate(prev) + delta
        } else {
          digestUpdate(prev)
        }

      case SeqUpdateRemove(removedNodeExp, prev) =>
        if (computeDelta) {
          assert(removedNodeExp.value != 0, "node 0 is a vehicle and cannot be removed")
          val nodeBefore = removedNodeExp.prev.value
          val nodeAfter  = vrp.nextNodeInRouting(removedNodeExp)
          val delta =
            (if (nodeBefore != nodeAfter) distanceFunction(nodeBefore)(nodeAfter) else 0) -
              distanceFunction(nodeBefore)(removedNodeExp.value) -
              distanceFunction(removedNodeExp.value)(nodeAfter) -
              distanceFunction(removedNodeExp.value)(removedNodeExp.value)
          digestUpdate(prev) + delta
        } else {
          digestUpdate(prev)
        }

      case m @ SeqUpdateMove(fromExp, toExp, afterExp, flip, prev) =>
        if (computeDelta) {
          val nodeBeforeSource = fromExp.prev.value
          val nodeAfterSource  = vrp.nextNodeInRouting(toExp)
          val nodeBeforeDest   = afterExp.value
          val nodeAfterDest =
            if (nodeBeforeSource == afterExp.value)
              vrp.nextNodeInRouting(toExp)
            else
              vrp.nextNodeInRouting(afterExp)
          val (startSeg, endSeg) =
            if (flip) (toExp.value, fromExp.value) else (fromExp.value, toExp.value)
          val deltaSeg = if (matrixIsSymmetrical) {
            0
          } else {
            if (flip && fromExp.value != toExp.value) {
              val distBefore =
                computeRouteLengthFromScratch(
                  fromExp.intSequence,
                  Some(fromExp),
                  Some(toExp),
                  backward = false
                )
              val distAfter =
                computeRouteLengthFromScratch(
                  fromExp.intSequence,
                  Some(toExp),
                  Some(fromExp),
                  backward = true
                )
              distAfter - distBefore
            } else {
              0
            }
          }
          val delta = distanceFunction(nodeBeforeDest)(startSeg) +
            distanceFunction(endSeg)(nodeAfterDest) -
            (if (nodeBeforeDest != nodeAfterDest)
               distanceFunction(nodeBeforeDest)(nodeAfterDest)
             else 0) -
            distanceFunction(nodeBeforeSource)(fromExp.value) -
            distanceFunction(toExp.value)(nodeAfterSource) +
            (if (nodeBeforeSource != nodeAfterSource)
               distanceFunction(nodeBeforeSource)(nodeAfterSource)
             else 0) +
            deltaSeg
          digestUpdate(prev) + delta
        } else {
          digestUpdate(prev)
        }

      case assign: SeqUpdateAssign => computeRouteLengthFromScratch(assign.newSequence)

      case SeqUpdateDefineCheckpoint(prev, level) =>
        val length = digestUpdate(prev)
        checkpointValues = CheckpointValue(level, length) :: checkpointValues
        length

      case _: SeqUpdateLastNotified => currentValue

      case update: SeqUpdateReleaseTopCheckpoint =>
        val length = digestUpdate(update.prev)
        checkpointValues = checkpointValues.tail
        length

      case update: SeqUpdateRollBackToTopCheckpoint =>
        digestUpdate(update.prev, false)
        assert(
          checkpointValues.head.level == update.level,
          s"checkpoint levels are not coherent " +
            s"(sequence level: ${update.level} != this invariant level: ${checkpointValues.head.level})"
        )
        currentValue = checkpointValues.head.value
        currentValue

    }
  }

  override def checkInternals(): Unit = {
    val routeFromScratch = computeRouteLengthFromScratch(routes.pendingValue)

    val vehicleTrips: String = vrp.mapVehicleToRoute
      .map({ case (vehicle, route) =>
        val routeWithV = route.appended(vehicle)
        val routeString = routeWithV.tail.tail
          .foldLeft(List((routeWithV.head, routeWithV.tail.head)))({ case (trips, toTrip) =>
            val fromTrip = trips.last._2
            trips.appended((fromTrip, toTrip))
          })
          .map(trip => s"${trip._1} --${distanceFunction(trip._1)(trip._2)}")
          .mkString("--> ")
        routeString + s"--> ${routeWithV.head}"
      })
      .toList
      .mkString("\n")

    vrp.mapVehicleToRoute.map(vAndRoute => vAndRoute._2.appended(vAndRoute._1)).toList

    require(
      routeLength.pendingValue == routeFromScratch,
      s"Incremental route length computing failed.\n" +
        s"Got ${routeLength.pendingValue} instead of $routeFromScratch expected\n" +
        s"Current Routes: \n$vehicleTrips"
    )
  }

}
