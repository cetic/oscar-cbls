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

package oscar.cbls.lib.invariant.routing.capacityConstraint

import oscar.cbls.{IntVariable, VRP}
import oscar.cbls.algo.sequence.{IntSequence, RootIntSequenceExplorer}
import oscar.cbls.core.computation.genericConstraint.LogReducedGlobalConstraint
import oscar.cbls.core.computation.genericConstraint.logReducedSegment._
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.lib.invariant.routing.capacityConstraint.transferFunction._

import scala.annotation.tailrec

object GlobalCapacityConstraintWithLogReduction {

  /** Creates an invariant maintaining for each vehicle an [[IntVariable]] stating if there is a
    * capacity violation at some point in the vehicle's route. 0 means no violation, 1 means that
    * there is a violation.
    *
    * This invariant uses content variation, aka delta, to compute the actual content of the vehicle
    * at a given node.
    *
    * The content variation can be :
    *   - Equal to zero ==> nothing is done, content does not change.
    *   - Positive ==> Loading some objects/people/...
    *   - Negative ==> Unloading some object/people/...
    *
    * Content variation can be specified for vehicles :
    *   - contentVariationAtNode[vehicle] ==> The starting content of the vehicle (cannot be
    *     negative).
    *   - contentVariationBackAtDepot[vehicle] ==> The last content variation
    *     (zero/positive/negative).
    *
    * There is a violation of the capacity if :
    *   - At some point the content of the vehicle is negative.
    *   - At some point the content of the vehicle exceed the maximal capacity of the vehicle.
    *
    * @param vrp
    *   The object that represents the Vehicle Routing Problem.
    * @param vehiclesCapacity
    *   The maximal capacity of each vehicle.
    * @param contentVariationAtNode
    *   The variation of the content reaching a specific node or leaving from depot.
    * @param contentVariationBackAtDepot
    *   The variation of the content getting back to depot.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    vrp: VRP,
    vehiclesCapacity: Array[Long],
    contentVariationAtNode: Array[Long],
    contentVariationBackAtDepot: Option[Array[Long]] = None,
    name: Option[String] = None
  ): GlobalCapacityConstraintWithLogReduction = {
    val _contentVariationBackAtDepot = contentVariationBackAtDepot.getOrElse(Array.fill(vrp.v)(0L))
    val violationPerVehicle          = Array.fill(vrp.v)(IntVariable(vrp.model, 0))
    new GlobalCapacityConstraintWithLogReduction(
      vrp,
      vehiclesCapacity,
      contentVariationAtNode,
      violationPerVehicle,
      _contentVariationBackAtDepot,
      name
    )
  }
}

/** Invariant maintaining for each vehicle an [[IntVariable]] stating if there is a capacity
  * violation at some point in the vehicle's route. 0 means no violation, 1 means that there is a
  * violation.
  *
  * This invariant uses content variation, aka delta, to compute the actual content of the vehicle
  * at a given node.
  *
  * The content variation can be :
  *   - Equal to zero ==> nothing is done, content does not change.
  *   - Positive ==> Loading some objects/people/...
  *   - Negative ==> Downloading some object/people/...
  *
  * Content variation can be specified for vehicles :
  *   - contentVariationAtNode[vehicle] ==> The starting content of the vehicle (cannot be
  *     negative).
  *   - contentVariationBackAtDepot[vehicle] ==> The last content variation
  *     (zero/positive/negative).
  *
  * There is a violation of the capacity if :
  *   - At some point the content of the vehicle is negative.
  *   - At some point the content of the vehicle exceed the maximal capacity of the vehicle.
  *
  * @param vrp
  *   The object that represents the Vehicle Routing Problem.
  * @param vehiclesCapacity
  *   The maximal capacity of each vehicle.
  * @param contentVariationAtNode
  *   The variation of the content reaching a specific node or leaving from depot.
  * @param violationPerVehicle
  *   For each vehicle an IntVariable stating if there is a violation in the route.
  * @param contentVariationBackAtDepot
  *   The variation of the content getting back to depot.
  * @param name
  *   The (optional) name of the Invariant.
  */
class GlobalCapacityConstraintWithLogReduction(
  vrp: VRP,
  vehiclesCapacity: Array[Long],
  contentVariationAtNode: Array[Long],
  violationPerVehicle: Array[IntVariable],
  contentVariationBackAtDepot: Array[Long],
  name: Option[String]
) extends LogReducedGlobalConstraint[BidirectionalVehicleContentTF, Boolean](vrp, name) {

  require(!vehiclesCapacity.exists(x => x < 0), "Vehicle capacity can not be negative.")
  require(
    !contentVariationAtNode.take(vrp.v).exists(x => x < 0),
    "Vehicle can not start with a negative content."
  )

  violationPerVehicle.foreach(violation => violation.setDefiningInvariant(this))

  // The TF of each individual node.
  private val contentTFAtNode: Array[BidirectionalVehicleContentTF] =
    Array.tabulate(vrp.n)(node =>
      BidirectionalVehicleContentTF(
        UnidirectionalContentTF(node, contentVariationAtNode(node)),
        UnidirectionalContentTF(node, contentVariationAtNode(node))
      )
    )

  // For the vehicle return value we consider that by default nothing is loaded/unloaded at the depot
  // (it's a fictive node)
  private val contentTFForVehicleReturn: Array[BidirectionalVehicleContentTF] =
    Array.tabulate(vrp.v)(vehicle =>
      BidirectionalVehicleContentTF(
        UnidirectionalContentTF(vehicle, contentVariationBackAtDepot(vehicle)),
        UnidirectionalContentTF(vehicle, contentVariationBackAtDepot(vehicle))
      )
    )

  /** Returns the violation variable of each vehicle.
   *
   * @return the violation variable of each vehicle.
   */
  def apply(): Array[IntVariable] = violationPerVehicle

  /** Returns the violation variable of the specified vehicle.
    *
    * @param vehicle
    *   The vehicle of which we want to know the violation.
    * @return
    *   The violation variable of the vehicle.
    */
  def apply(vehicle: Int): IntVariable = {
    violationPerVehicle(vehicle)
  }

  /** Composes two transfer function to form a bigger one.
    *
    * @param f1
    *   The first transfer function.
    * @param f2
    *   The second transfer function.
    * @return
    *   The resulting composition.
    */
  private def composeVehicleContentTFs(
    f1: VehicleContentTransferFunction,
    f2: VehicleContentTransferFunction
  ): VehicleContentTransferFunction = {
    if (f1.isEmpty || f2.isEmpty) return EmptyContentTF
    val from = f1.from
    val to   = f2.to
    val max = Math.max(f1.maxContentIfStartAt0, f1.contentAtEndIfStartAt0 + f2.maxContentIfStartAt0)
    val min = Math.min(f1.minContentIfStartAt0, f1.contentAtEndIfStartAt0 + f2.minContentIfStartAt0)
    val end = f1.contentAtEndIfStartAt0 + f2.contentAtEndIfStartAt0
    UnidirectionalContentTF(from, to, max, min, end)
  }

  override def nodeValue(node: Int): BidirectionalVehicleContentTF = contentTFAtNode(node)

  override def endNodeValue(vehicle: Int): BidirectionalVehicleContentTF =
    contentTFForVehicleReturn(vehicle)

  override def composeSteps(
    firstStep: BidirectionalVehicleContentTF,
    secondStep: BidirectionalVehicleContentTF
  ): BidirectionalVehicleContentTF = {
    val flipped = composeVehicleContentTFs(secondStep.flippedFunction, firstStep.flippedFunction)
    val nonFLipped =
      composeVehicleContentTFs(firstStep.nonFlippedFunction, secondStep.nonFlippedFunction)
    BidirectionalVehicleContentTF(nonFLipped, flipped)
  }

  override def computeVehicleValueComposed(
    vehicle: Int,
    segments: List[LogReducedSegment[BidirectionalVehicleContentTF]]
  ): Boolean = {
    @tailrec
    def composeStepsList(
      vehicleContentFunctions: List[BidirectionalVehicleContentTF],
      lastOutputContent: Long,
      flipped: Boolean
    ): Long = {
      vehicleContentFunctions match {
        case Nil => lastOutputContent
        case head :: tail =>
          val twoWaysVehicleContentFunction = head
          val newOutputContent =
            lastOutputContent + twoWaysVehicleContentFunction.contentAtEndIfStartAt0(flipped)
          val isConstraintViolated =
            twoWaysVehicleContentFunction(lastOutputContent, vehiclesCapacity(vehicle), flipped)
          if (isConstraintViolated) -1
          else composeStepsList(tail, newOutputContent, flipped)
      }

    }

    @tailrec
    def composeLogReducedSegments(
      logReducedSegments: List[LogReducedSegment[BidirectionalVehicleContentTF]],
      lastOutputContent: Long = 0L
    ): Boolean = {
      logReducedSegments match {
        case Nil => false
        case head :: tail =>
          val newOutputContent: Long = head match {
            case LogReducedPreComputedSegment(_, _, steps) =>
              composeStepsList(steps, lastOutputContent, flipped = false)

            case LogReducedFlippedPreComputedSegment(_, _, steps) =>
              composeStepsList(steps.reverse, lastOutputContent, flipped = true)

            case LogReducedNewNode(_, value) =>
              val isConstraintViolated =
                value(lastOutputContent, vehiclesCapacity(vehicle), flipped = true)
              if (isConstraintViolated) -1
              else lastOutputContent + value.contentAtEndIfStartAt0(true)

            case x =>
              throw new Error(s"Unhandled match with $x")
          }
          (newOutputContent < 0) || composeLogReducedSegments(tail, newOutputContent)
      }
    }
    composeLogReducedSegments(segments)
  }

  override protected def assignVehicleValue(vehicle: Int, value: Boolean): Unit = {
    if (value) violationPerVehicle(vehicle) := 1 else violationPerVehicle(vehicle) := 0
  }

  override protected def computeVehicleValueFromScratch(
    vehicle: Int,
    routes: IntSequence
  ): Boolean = {
    var explorer       = routes.explorerAtAnyOccurrence(vehicle).get
    var currentContent = contentVariationAtNode(vehicle)
    val maxCapacity    = vehiclesCapacity(vehicle)

    // The vehicle content at start is greater than the max capacity.
    if (currentContent > maxCapacity) return true
    explorer = explorer.next
    var continue = true
    while (continue) {
      explorer match {
        case explorer: RootIntSequenceExplorer =>
          if (explorer.prev.value < vrp.v) return false // => empty route
          else continue = false
        case _ =>
          if (explorer.value < vrp.v) {
            if (explorer.prev.value < vrp.v) return false // => empty route
            else continue = false
          } else {
            currentContent += contentVariationAtNode(explorer.value)
            if (currentContent > maxCapacity || currentContent < 0) return true
            explorer = explorer.next
          }
      }
    }

    currentContent += contentVariationBackAtDepot(vehicle)
    if (currentContent > maxCapacity || currentContent < 0) return true
    false
  }
}
