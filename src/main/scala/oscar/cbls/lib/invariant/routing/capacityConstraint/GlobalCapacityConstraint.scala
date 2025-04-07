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

import oscar.cbls.VRS
import oscar.cbls.algo.sequence.{IntSequence, RootIntSequenceExplorer}
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.lib.invariant.routing.abstractGenericConstraint.TransferFunctionBasedGlobalConstraint
import oscar.cbls.lib.invariant.routing.abstractGenericConstraint.transferFunction.TransferFunction
import oscar.cbls.lib.invariant.routing.capacityConstraint.transferFunction._

/** Companion object of [[GlobalCapacityConstraint]] */
object GlobalCapacityConstraint {

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
    * @param vrs
    *   The object that represents the vehicle routing structure.
    * @param vehiclesCapacity
    *   The maximal capacity of each vehicle.
    * @param contentVariationAtNode
    *   The variation of the content reaching a specific node or leaving from depot.
    * @param contentVariationBackAtDepot
    *   The variation of the content getting back to depot.
    * @param withLogReduction
    *   If true the log reduction algorithm will be activated.
    * @param withExtremesPC
    *   If true classical pre-computation will be applied for each pair of node starting at
    *   vehicle's depot and ending in the vehicle's route. And also for each pair of node starting
    *   at the end of the route and ending in the vehicle's route. (Useless without using log
    *   reduction as well)
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    vrs: VRS,
    vehiclesCapacity: Array[Long],
    contentVariationAtNode: Array[Long],
    contentVariationBackAtDepot: Option[Array[Long]] = None,
    withLogReduction: Boolean = false,
    withExtremesPC: Boolean = false,
    name: String = "Global Capacity Constraint"
  ): GlobalCapacityConstraint = {
    val _contentVariationBackAtDepot = contentVariationBackAtDepot.getOrElse(Array.fill(vrs.v)(0L))
    val violationPerVehicle          = Array.fill(vrs.v)(IntVariable(vrs.store, 0))
    new GlobalCapacityConstraint(
      vrs,
      vehiclesCapacity,
      contentVariationAtNode,
      violationPerVehicle,
      _contentVariationBackAtDepot,
      withLogReduction,
      withExtremesPC,
      if (name == "") None else Some(name)
    )
  }
}

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
  * @param vrs
  *   The object that represents the vehicle routing structure.
  * @param vehiclesCapacity
  *   The maximal capacity of each vehicle.
  * @param contentVariationAtNode
  *   The variation of the content reaching a specific node or leaving from depot.
  * @param contentVariationBackAtDepot
  *   The variation of the content getting back to depot.
  * @param withLogReduction
  *   If true the log reduction algorithm will be activated.
  * @param withExtremesPC
  *   If true classical pre-computation will be applied for each pair of node starting at vehicle's
  *   depot and ending in the vehicle's route. And also for each pair of node starting at the end of
  *   the route and ending in the vehicle's route. (Useless without using log reduction as well)
  * @param name
  *   The (optional) name of the Invariant.
  */
class GlobalCapacityConstraint(
  vrs: VRS,
  vehiclesCapacity: Array[Long],
  contentVariationAtNode: Array[Long],
  violationPerVehicle: Array[IntVariable],
  contentVariationBackAtDepot: Array[Long],
  withLogReduction: Boolean,
  withExtremesPC: Boolean,
  name: Option[String]
) extends TransferFunctionBasedGlobalConstraint[Boolean](
      vrs,
      withLogReduction,
      withExtremesPC,
      name
    ) {

  require(!vehiclesCapacity.exists(x => x < 0), "Vehicle capacity can not be negative.")
  require(
    !contentVariationAtNode.take(vrs.v).exists(x => x < 0),
    "Vehicle can not start with a negative content."
  )

  violationPerVehicle.foreach(_.setDefiningInvariant(this))

  private val contentTFOfNode: Array[VehicleContentTF] =
    initTF(contentVariationAtNode, initVehicle = true)

  private val contentTFBackAtDepot: Array[VehicleContentTF] = initTF(contentVariationBackAtDepot)

  /** Returns the violation variable of each vehicle.
    *
    * @return
    *   the violation variable of each vehicle.
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

  /** Initialize the VehicleContentTF with delta as initial value.
    *
    * @param initValues
    *   An array containing the initial value of the expected VehicleContentTF.
    * @param initVehicle
    *   Whether we must set vehicle capacity for the first v VehicleContentTF.
    * @return
    *   An array of initialized VehicleContentTF.
    */
  private def initTF(
    initValues: Array[Long],
    initVehicle: Boolean = false
  ): Array[VehicleContentTF] = {
    Array.tabulate(initValues.length)(i => {
      val delta = initValues(i)
      val forward =
        UnidirectionalContentTF(forward = true, delta, delta, delta)
      val backward =
        UnidirectionalContentTF(forward = false, delta, delta, delta)
      val vehicleCapacity = if (initVehicle && i < vrs.v) Some(vehiclesCapacity(i)) else None
      VehicleContentTF(i, i, forward, backward, vehicleCapacity)
    })
  }

  override def nodeValue(node: Int): TransferFunction[Boolean] = contentTFOfNode(node)

  override def endNodeValue(vehicle: Int): TransferFunction[Boolean] = contentTFBackAtDepot(vehicle)

  override protected def assignVehicleValue(vehicle: Int, value: Boolean): Unit = {
    violationPerVehicle(vehicle) := (if (value) 1 else 0)
  }

  override def checkViolationStartingAtDepot(
    vehicle: Int,
    transferFunction: TransferFunction[Boolean]
  ): Boolean = transferFunction.apply()

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
          if (explorer.prev.value < vrs.v) return false // => empty route
          else continue = false
        case _ =>
          if (explorer.value < vrs.v) {
            if (explorer.prev.value < vrs.v) return false // => empty route
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
