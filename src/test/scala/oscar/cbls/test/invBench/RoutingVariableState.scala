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

package oscar.cbls.test.invBench

import org.scalacheck.Gen
import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer, RootIntSequenceExplorer}
import oscar.cbls.modeling.routing.VRP

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable

/** This class holds the internal state of a SeqVariable used for a routing problem.
  *
  * It is used to determine the applicable movements given this State with respect to routing
  * convention constraints.
  *
  * @param id
  *   The test id of the SeqVariable to which this State is linked.
  * @param currentState
  *   The current State of the SeqVariable.
  * @param vrp
  *   The VRP instance to which the SeqVariable is linked.
  * @param unrouted
  *   A set of unrouted nodes.
  * @param routes
  *   A sequence modeling the current route.
  */
case class RoutingVariableState(
  id: Int,
  currentState: SeqVariableStackableState,
  vrp: VRP,
  unrouted: HashSet[Int],
  routes: IntSequence
) extends VariableState(id) {

  private val size: Int = currentState.seqSize

  /** Returns an HashMap which maps each vehicle to its route. */
  private val mapVehicleToRoute: HashMap[Int, List[Int]] =
    HashMap.from((0 until vrp.v).map((vehicle: Int) => vehicle -> routeOfVehicle(vehicle)))
  // Flags determining if a given move is allowed or not
  private val moveAllowed: Boolean = size >= vrp.v + 2
  private val swapAllowed: Boolean =
    mapVehicleToRoute.count(vehicleAndRoute => vehicleAndRoute._2.length > 1) >= 2
  private val flipAndRemoveAllowed: Boolean = size >= vrp.v + 1
  private val releaseAllowed: Boolean =
    currentState.seqOperationSinceLastCheckpoint == 0 && currentState.seqCheckpointLevel > -1
  private val rollBackAllowed: Boolean = currentState.seqCheckpointLevel > -1
  private val assignAllowed: Boolean   = currentState.seqCheckpointLevel == -1
  private val insertAllowed: Boolean   = unrouted.nonEmpty

  private val emptyRoute: Boolean = routes.forall(_ < vrp.v)

  private val vehiclePosition: Array[Int] =
    Array.tabulate(vrp.v)(routes.positionOfAnyOccurrence(_).get)

  private val positionOfRoutedNodesExceptVehicles: List[Int] = {
    var toReturn: List[Int] = List.empty
    var i: Int              = 0
    for (node <- routes) {
      if (node >= vrp.v) toReturn = i :: toReturn
      i += 1
    }
    toReturn
  }

  private val lastUsedVehiclePosition: Int = {
    val usedV = mapVehicleToRoute.filter(vehicleAndRoute => vehicleAndRoute._2.length > 1).keys
    if (usedV.nonEmpty) vehiclePosition(usedV.max)
    else 0
  }

  private def genRoutingInsert: Gen[RoutingInsertUpdate] = {
    for {
      value <- Gen.oneOf(unrouted)
      after <- Gen.choose(0, size - 1)
    } yield RoutingInsertUpdate(id, value, after)
  }

  private def genRoutingRemove: Gen[RoutingRemoveUpdate] = {
    for {
      removePos <- Gen.oneOf(positionOfRoutedNodesExceptVehicles)
    } yield RoutingRemoveUpdate(id, removePos)
  }

  private def genRoutingMove: Gen[RoutingMoveUpdate] = {
    for {
      from <- Gen.oneOf(positionOfRoutedNodesExceptVehicles)
      fromVehicle    = vehicleReachingPos(from)
      nextVehiclePos = findNextVehiclePos(fromVehicle)
      to    <- Gen.choose(from, nextVehiclePos - 1)
      after <- Gen.oneOf((0 until from) ++ (to + 1 until size))
      flip  <- Gen.prob(0.5)
    } yield RoutingMoveUpdate(id, from, to, after, flip)
  }

  private def genRoutingFlip: Gen[RoutingFlipUpdate] = {
    for {
      from <- Gen.oneOf(positionOfRoutedNodesExceptVehicles)
      fromVehicle    = vehicleReachingPos(from)
      nextVehiclePos = findNextVehiclePos(fromVehicle)
      to <- Gen.choose(from, nextVehiclePos - 1)
    } yield RoutingFlipUpdate(id, from, to)
  }

  private def genRoutingSwap: Gen[RoutingSwapUpdate] = {
    for {
      from1 <- Gen.oneOf(positionOfRoutedNodesExceptVehicles.filter(_ < lastUsedVehiclePosition))
      from1Vehicle    = vehicleReachingPos(from1)
      nextVehicle1Pos = findNextVehiclePos(from1Vehicle, size - 1)
      to1   <- Gen.choose(from1, nextVehicle1Pos - 1)
      flip1 <- Gen.prob(0.5)
      from2 <- Gen.oneOf(positionOfRoutedNodesExceptVehicles.filter(_ > to1))
      from2Vehicle    = vehicleReachingPos(from2)
      nextVehicle2Pos = findNextVehiclePos(from2Vehicle)
      to2   <- Gen.choose(from2, nextVehicle2Pos - 1)
      flip2 <- Gen.prob(0.5)
    } yield RoutingSwapUpdate(id, from1, to1, flip1, from2, to2, flip2)
  }

  private def genRoutingAssign: Gen[RoutingAssignUpdate] = {
    for {
      routedNodes <- Gen.someOf(vrp.v until vrp.n - 1)
      vehiclePos  <- Gen.listOfN(vrp.v - 1, Gen.choose(0, routedNodes.size))
    } yield RoutingAssignUpdate(
      id,
      0 :: insertVehicleNodes(routedNodes.toList, vehiclePos.sorted.reverse)
    )

  }

  private def genRoutingDefineCheckpoint: Gen[RoutingDefineCheckpointUpdate] =
    Gen.const(RoutingDefineCheckpointUpdate(id))

  private def genRoutingReleaseTopCheckpoint: Gen[RoutingReleaseTopCheckpointUpdate] =
    Gen.const(RoutingReleaseTopCheckpointUpdate(id))

  private def genRoutingRollBack: Gen[RoutingRollBackToTopCheckpointUpdate] =
    Gen.const(RoutingRollBackToTopCheckpointUpdate(id))

  override def generateMove(): Gen[VariableMove] = {
    var authMoves: List[(Int, Gen[VariableMove])] =
      List((1, genRoutingDefineCheckpoint))
    if (insertAllowed) authMoves = (5, genRoutingInsert) :: authMoves
    if (moveAllowed && !emptyRoute)
      authMoves = (5, genRoutingMove) :: authMoves
    if (!emptyRoute && swapAllowed) authMoves = (5, genRoutingSwap) :: authMoves
    if (flipAndRemoveAllowed && !emptyRoute)
      authMoves = authMoves ::: List((5, genRoutingFlip), (3, genRoutingRemove))
    if (releaseAllowed) authMoves = (2, genRoutingReleaseTopCheckpoint) :: authMoves
    if (rollBackAllowed) authMoves = (3, genRoutingRollBack) :: authMoves
    if (assignAllowed) authMoves = (2, genRoutingAssign) :: authMoves

    Gen.frequency(authMoves: _*)
  }

  override def canMake(m: VariableMove): Boolean = {
    m match {
      case i: RoutingInsertUpdate =>
        insertAllowed && i.after < size && i.after >= 0 && unrouted.contains(i.value)
      case r: RoutingRemoveUpdate =>
        !emptyRoute && flipAndRemoveAllowed && r.position < size && verifyRemove(r)
      case m: RoutingMoveUpdate =>
        !emptyRoute && moveAllowed && m.after < size && m.to < size && verifySegment(m.from, m.to)
      case s: RoutingSwapUpdate =>
        !emptyRoute && swapAllowed && s.to_1 < size && s.to_2 < size && verifySegment(
          s.from_1,
          s.to_1
        ) && verifySegment(s.from_2, s.to_2)
      case f: RoutingFlipUpdate =>
        !emptyRoute && flipAndRemoveAllowed && f.to < size && verifySegment(f.from, f.to)
      case _: RoutingReleaseTopCheckpointUpdate    => releaseAllowed
      case _: RoutingRollBackToTopCheckpointUpdate => rollBackAllowed
      case _: RoutingAssignUpdate                  => assignAllowed
      case _                                       => true
    }
  }

  override def toString: String = {
    s"""Route $id : size $size
       |Checkpoint lvl ${currentState.seqCheckpointLevel}
       |${currentState.seqOperationSinceLastCheckpoint} operations since last checkpoint
       |Current routed nodes: $routes
       |Current unrouted nodes: $unrouted
       |""".stripMargin
  }

  private def findNextVehiclePos(currentVehicle: Int, default: Int = size): Int = {
    if (currentVehicle + 1 == vrp.v) default
    else routes.positionOfAnyOccurrence(currentVehicle + 1).get
  }

  /** Finds the vehicle reaching a given position. */
  private def vehicleReachingPos(pos: Int): Int = {
    var upperVehicle         = vrp.v - 1
    var upperVehiclePosition = vehiclePosition(upperVehicle)

    if (pos >= upperVehiclePosition) return upperVehicle

    var lowerVehicle         = 0
    var lowerVehiclePosition = 0

    assert(routes.positionOfAnyOccurrence(lowerVehicle).get == 0)
    require(lowerVehiclePosition <= upperVehiclePosition)

    while (lowerVehicle + 1 < upperVehicle) {
      val midVehicle         = (lowerVehicle + upperVehicle) / 2
      val midVehiclePosition = vehiclePosition(midVehicle)
      if (midVehiclePosition == pos) {
        return midVehicle
      }
      if (midVehiclePosition <= pos) {
        lowerVehicle = midVehicle
        lowerVehiclePosition = midVehiclePosition
      } else {
        upperVehicle = midVehicle
        upperVehiclePosition = midVehiclePosition
      }
    }
    lowerVehicle
  }

  /** Returns the route of the input vehicle, not including the return to the depot.
    *
    * @param vehicle
    *   The vehicle for which the route is required
    * @return
    *   The list of node that are in the vehicle route
    */
  private def routeOfVehicle(vehicle: Int): List[Int] = {
    var currentVehicleExplorer = routes.explorerAtAnyOccurrence(vehicle).get.next
    var toReturn: List[Int]    = List(vehicle)
    while (currentVehicleExplorer.position < routes.size && currentVehicleExplorer.value >= vrp.v) {
      toReturn = currentVehicleExplorer.value :: toReturn
      currentVehicleExplorer = currentVehicleExplorer.next
    }

    toReturn.reverse
  }

  @tailrec
  private def insertVehicleNodes(
    routedNodes: List[Int],
    vehiclePositions: List[Int],
    currentVehicle: Int = vrp.v - 1
  ): List[Int] = {
    vehiclePositions match {
      case Nil => routedNodes
      case h :: t =>
        insertVehicleNodes(
          routedNodes.take(h) ::: (currentVehicle :: routedNodes.drop(h)),
          t,
          currentVehicle - 1
        )
    }
  }

  private def verifyRemove(r: RoutingRemoveUpdate): Boolean = {
    val explorer = routes.explorerAtPosition(r.position)
    explorer.nonEmpty && explorer.get.value >= vrp.v
  }

  private def verifySegment(from: Int, to: Int): Boolean = {
    val fromVehicle  = vehicleReachingPos(from)
    val toVehicle    = vehicleReachingPos(to)
    val fromExplorer = routes.explorerAtPosition(from)
    val toExplorer   = routes.explorerAtPosition(to)
    val fromOk       = fromExplorer.nonEmpty && fromExplorer.get.value >= vrp.v
    val toOk         = toExplorer.nonEmpty && toExplorer.get.value >= vrp.v
    fromOk && toOk && fromVehicle == toVehicle
  }
}
