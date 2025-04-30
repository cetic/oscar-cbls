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

package oscar.cbls.lib.invariant.routing.timeWindows

import oscar.cbls.VRS
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.lib.invariant.routing.abstractGenericConstraint.TransferFunctionBasedGlobalConstraint
import oscar.cbls.lib.invariant.routing.abstractGenericConstraint.transferFunction.{TransferFunction, UnidirectionalTransferFunction}

/** Companion object of the [[TimeWindowsConstraint]] class. */
object TimeWindowsConstraint {

  /** Returns an instance of a Time Windows constraint.
    *
    * @param vrs
    *   The object that represents the vehicle routing structure.
    * @param timeFunction
    *   Function that gives the travel time between two nodes.
    * @param singleNodeTimeWindows
    *   For each node, associates a [[TimeWindow]].
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
    timeFunction: Int => Int => Long,
    singleNodeTimeWindows: Array[TimeWindow],
    withLogReduction: Boolean = false,
    withExtremesPC: Boolean = false,
    name: String = "Time Windows Constraint"
  ): TimeWindowsConstraint = {
    val output = Array.fill(vrs.v)(IntVariable(vrs.store, 0L))

    new TimeWindowsConstraint(
      vrs,
      timeFunction,
      singleNodeTimeWindows,
      output,
      withLogReduction,
      withExtremesPC,
      if (name == "") None else Some(name)
    )
  }

  /** Returns an instance of a Time Windows constraint based an on a time matrix.
    *
    * @param vrs
    *   The object that represents the vehicle routing structure.
    * @param timeMatrix
    *   Matrix that gives the travel time between two nodes.
    * @param singleNodeTimeWindows
    *   For each node, associates a [[TimeWindow]].
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
  def fromMatrix(
    vrs: VRS,
    timeMatrix: Array[Array[Long]],
    singleNodeTimeWindows: Array[TimeWindow],
    withLogReduction: Boolean = false,
    withExtremesPC: Boolean = false,
    name: String = "Time Windows Constraint"
  ): TimeWindowsConstraint = {
    val timeFunction: Int => Int => Long = i => j => timeMatrix(i)(j)
    TimeWindowsConstraint(
      vrs,
      timeFunction,
      singleNodeTimeWindows,
      withLogReduction,
      withExtremesPC,
      name
    )
  }
}

/** Creates an invariant maintaining for each vehicle an [[IntVariable]] stating if there is a time
  * window violation at some point in the vehicle's route. `0` means no violation, `1` means that
  * there is a violation.<br>
  *
  * There is a violation if the vehicle visits a node after its last permit arrival time.
  *
  * @param vrs
  *   The object that represents the vehicle routing structure.
  * @param timeFunction
  *   Function that gives the travel time between two nodes.
  * @param singleNodeTimeWindows
  *   For each node, associates a [[TimeWindow]].
  * @param output
  *   For each vehicle, maintains if the time windows constraint is violated.
  * @param withLogReduction
  *   If true the log reduction algorithm will be activated.
  * @param withExtremesPC
  *   If true classical pre-computation will be applied for each pair of node starting at vehicle's
  *   depot and ending in the vehicle's route. And also for each pair of node starting at the end of
  *   the route and ending in the vehicle's route. (Useless without using log reduction as well)
  * @param name
  *   The (optional) name of the Invariant.
  */
class TimeWindowsConstraint(
  vrs: VRS,
  timeFunction: Int => Int => Long,
  singleNodeTimeWindows: Array[TimeWindow],
  output: Array[IntVariable],
  withLogReduction: Boolean,
  withExtremesPC: Boolean,
  name: Option[String]
) extends TransferFunctionBasedGlobalConstraint[Boolean](
      vrs,
      withLogReduction,
      withExtremesPC,
      name
    ) {

  output.foreach(_.setDefiningInvariant(this))

  private val timeWindowTFOfNode: Array[TimeWindowTF] = initTF()

  def apply(): Array[IntVariable] = output

  def apply(i: Int): IntVariable = output(i)

  /** Given an arrival time at some node, returns the leaving time.
    *
    * @param node
    *   The target node.
    * @param t
    *   The arrival time at node.
    * @param flipped
    *   If the segment containing the node is flipped or not.
    */
  def leavingTimeWhenArrivingAt(node: Int, t: Long, flipped: Boolean = false): Long = {
    timeWindowTFOfNode(node).leavingTimeWhenArrivingAt(t, flipped)
  }

  override def nodeValue(node: Int): TransferFunction[Boolean] = timeWindowTFOfNode(node)

  override def endNodeValue(vehicle: Int): TransferFunction[Boolean] = timeWindowTFOfNode(vehicle)

  override def checkViolationStartingAtDepot(
    vehicle: Int,
    transferFunction: TransferFunction[Boolean]
  ): Boolean = transferFunction.apply()

  override protected def assignVehicleValue(vehicle: Int, value: Boolean): Unit = {
    output(vehicle) := (if (value) 1L else 0L)
  }

  override protected def computeVehicleValueFromScratch(
    vehicle: Int,
    routes: IntSequence
  ): Boolean = {
    var from            = vehicle
    var fromTW          = singleNodeTimeWindows(from)
    var leaveTimeAtFrom = fromTW.earliestLeaving
    for (exp <- routes.explorerAtFirstOccurrence(vehicle).get.next.forward.until(_.value < vrs.v)) {
      val to              = exp.value
      val toTW            = singleNodeTimeWindows(to)
      val arrivalTimeAtTo = leaveTimeAtFrom + timeFunction(from)(to)
      val leavingTimeAtTo = (toTW.earliestArrival max arrivalTimeAtTo) + toTW.taskDuration

      // Checks violation
      if (leavingTimeAtTo > toTW.latestLeaving) return true

      from = to
      fromTW = toTW
      leaveTimeAtFrom = leavingTimeAtTo
    }
    val arrivalTimeAtDepot = leaveTimeAtFrom + timeFunction(from)(vehicle)
    arrivalTimeAtDepot > singleNodeTimeWindows(vehicle).latestLeaving
  }

  private def initTF(): Array[TimeWindowTF] = {
    Array.tabulate(vrs.n)(i => {
      val tw = singleNodeTimeWindows(i)
      val tf = new UnidirectionalTimeWindowTF(
        tw.earliestArrival,
        tw.latestArrival,
        tw.earliestLeaving,
        i,
        i
      )
      new TimeWindowTF(i, i, tf, tf)
    })
  }

  /** Defines a unidirectional time window transfer function over a segment.
    *
    * @param earliestArrival
    *   The earliest time the vehicle can access to the start node of the segment.
    * @param latestArrival
    *   The latest time the vehicle can access to the start node of the segment.
    * @param earliestLeaving
    *   The earliest time the vehicle can leave the end node of the segment.
    * @param from
    *   The node from which this transfer function is valid.
    * @param to
    *   The node to which this transfer function is valid.
    */
  private class UnidirectionalTimeWindowTF(
    val earliestArrival: Long,
    val latestArrival: Long,
    val earliestLeaving: Long,
    val from: Int,
    val to: Int
  ) extends UnidirectionalTransferFunction {

    /** The duration of the tasks to perform on the associated segment. */
    private val duration: Long = earliestLeaving - earliestArrival

    /** The last time we can leave `to` node to respect the constraint. */
    private val latestLeaving: Long = latestArrival + duration

    /** Whether the constraint is violated for a node in the associated segment. */
    def isEmpty: Boolean = false

    /** Given an arrival time at node `from`, returns the leaving time from node `to`. */
    def leavingTimeWhenArrivingAt(t: Long): Long = {
      if (t <= earliestArrival) earliestLeaving
      else if (t <= latestArrival) t + duration
      else -1L
    }

    override def toString: String =
      s"""
         |Earliest arrival time: $earliestArrival
         |Latest arrival time: $latestArrival
         |Earliest leaving time: $earliestLeaving
         |Latest leaving time: $latestLeaving
         |From: $from
         |To: $to
         |""".stripMargin

    override def compose(
      otherTF: UnidirectionalTransferFunction
    ): UnidirectionalTransferFunction = {
      otherTF match {
        case other: UnidirectionalTimeWindowTF =>
          if (this.isEmpty || other.isEmpty) return new EmptyTimeWindow(this.from, other.to)

          val travelTime: Long                 = timeFunction(this.to)(other.from)
          val earliestArrivalTimeAtOther: Long = this.earliestLeaving + travelTime
          val latestArrivalTimeAtOther: Long   = this.latestLeaving + travelTime

          val (composedEarliestArrival, composedLatestArrival, composedEarliestLeaving)
            : (Long, Long, Long) =
            (
              earliestArrivalTimeAtOther <= other.earliestArrival,
              earliestArrivalTimeAtOther <= other.latestArrival,
              latestArrivalTimeAtOther <= other.earliestArrival,
              latestArrivalTimeAtOther <= other.latestArrival
            ) match {
              case (true, true, true, true) =>
                (this.latestArrival, this.latestArrival, other.earliestLeaving)

              case (true, true, false, true) =>
                (
                  other.earliestArrival - this.duration - travelTime,
                  this.latestArrival,
                  other.earliestLeaving
                )

              case (true, true, false, false) =>
                (
                  other.earliestArrival - this.duration - travelTime,
                  other.latestArrival - this.duration - travelTime,
                  other.earliestLeaving
                )

              case (false, true, false, true) =>
                (
                  this.earliestArrival,
                  this.latestArrival,
                  this.earliestLeaving + travelTime + other.duration
                )

              case (false, true, false, false) =>
                (
                  this.earliestArrival,
                  other.latestArrival - this.duration - travelTime,
                  this.earliestLeaving + travelTime + other.duration
                )

              case (false, false, false, false) =>
                (1L, -1L, -1L)

              case x =>
                throw new Error(s"Unhandled case: $x")
            }

          if (composedEarliestArrival <= composedLatestArrival)
            new UnidirectionalTimeWindowTF(
              composedEarliestArrival,
              composedLatestArrival,
              composedEarliestLeaving,
              this.from,
              other.to
            )
          else new EmptyTimeWindow(this.from, other.to)
      }
    }
  }

  /** Special case of time windows transfer function. It is used as an absorbent element for the
    * composition of [[UnidirectionalTimeWindowTF]] when some of the time window constrains are
    * violated.
    *
    * @param from
    *   The node from which this transfer function is valid.
    * @param to
    *   The node to which this transfer function is valid.
    */
  private class EmptyTimeWindow(from: Int, to: Int)
      extends UnidirectionalTimeWindowTF(1L, -1L, -1L, from, to) {

    override def isEmpty: Boolean = true

    override def leavingTimeWhenArrivingAt(t: Long): Long = -1L
  }

  /** A time window transfer function.
    *
    * @param from
    *   The node from which this transfer function is valid.
    * @param to
    *   The node to which this transfer function is valid.
    * @param forwardTF
    *   A unidirectional TF starting at `from` and ending at `to`.
    * @param backwardTF
    *   A unidirectional TF starting at `to` and ending at `from`.
    */
  private class TimeWindowTF(
    override val from: Int,
    override val to: Int,
    forwardTF: UnidirectionalTimeWindowTF,
    backwardTF: UnidirectionalTimeWindowTF
  ) extends TransferFunction[Boolean](from, to, forwardTF, backwardTF) {

    override def apply(): Boolean = forwardTF.isEmpty

    override def compose(
      otherTF: TransferFunction[Boolean],
      otherFlipped: Boolean
    ): TransferFunction[Boolean] = {
      val (newForward, newBackward) = {
        if (otherFlipped) {
          (
            forwardTF.compose(otherTF.backward).asInstanceOf[UnidirectionalTimeWindowTF],
            otherTF.forward.compose(backwardTF).asInstanceOf[UnidirectionalTimeWindowTF]
          )
        } else {
          (
            forwardTF.compose(otherTF.forward).asInstanceOf[UnidirectionalTimeWindowTF],
            otherTF.backward.compose(backwardTF).asInstanceOf[UnidirectionalTimeWindowTF]
          )

        }
      }

      new TimeWindowTF(this.from, otherTF.to(otherFlipped), newForward, newBackward)
    }

    /** Given an arrival time at node `from`, returns the leaving time from node `to`.
      *
      * @param t
      *   The arrival time.
      * @param flipped
      *   If the current segment is flipped or not.
      */
    def leavingTimeWhenArrivingAt(t: Long, flipped: Boolean = false): Long = {
      if (flipped) backwardTF.leavingTimeWhenArrivingAt(t)
      else forwardTF.leavingTimeWhenArrivingAt(t)
    }

  }

}
