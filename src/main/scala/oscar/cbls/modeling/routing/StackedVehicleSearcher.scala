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

package oscar.cbls.modeling.routing

import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.seq.SeqVariable

/** Factory for [[StackedVehicleSearcher]]. */
object StackedVehicleSearcher {

  /** @param routes
    *   A sequence containing only the vehicles in increasing order.
    * @return
    *   An instance of StackedVehicleSearcher.
    */
  def apply(routes: SeqVariable): ConcreteSearcher =
    StackedVehicleSearcher(routes.value())

  /** @param initialSeq
    *   A sequence containing only the vehicles in increasing order.
    * @return
    *   An instance of StackedVehicleSearcher.
    */
  def apply(initialSeq: IntSequence): ConcreteSearcher =
    StackedVehicleSearcher(initialSeq, initialSeq.size)

  /** @param initialSeq
    *   A sequence modeling a route with `v` vehicles
    * @param v
    *   The number of vehicle in the route.
    * @return
    *   An instance of StackedVehicleSearcher.
    */
  def apply(initialSeq: IntSequence, v: Int): ConcreteSearcher = {
    val positionOfVehicle: Array[Int] = Array.tabulate(v)(initialSeq.positionOfAnyOccurrence(_).get)
    new ConcreteSearcher(positionOfVehicle)
  }

  /** @param v
    *   The number of considered vehicles.
    * @param nodeToPosition
    *   A function to compute the position of each vehicle in a reference route.
    * @return
    *   A ConcreteSearcher based on `nodeToPosition`.
    */
  def apply(v: Int, nodeToPosition: Int => Int): ConcreteSearcher =
    new ConcreteSearcher(Array.tabulate(v)(nodeToPosition))

  /** @param startPositionOfVehicles
    *   An array that gives for each vehicle its start position in a reference route
    * @return
    *   A ConcreteSearcher based on `startPositionIfVehicles`
    */
  def apply(startPositionOfVehicles: Array[Int]) =
    new ConcreteSearcher(startPositionOfVehicles)

}

/** Abstract structure which maintains the positions visited by a vehicle related to a
  * [[oscar.cbls.core.computation.seq.SeqVariable]].
  *
  * In a routing problem, the routes are modeled by a
  * [[oscar.cbls.core.computation.seq.SeqVariable]]. Each vehicle visits a set of position in this
  * sequence.
  *
  * During a search, the sequence is modified. This structure maintains the positions visited in the
  * sequence by each vehicle by keeping a stack of updates.
  *
  * @param v
  *   The number of vehicles.
  * @param level
  *   The level in the stack.
  * @param topCheckpoint
  *   An optional previous state in the stack to which we can easily roll back.
  */
abstract class StackedVehicleSearcher(
  val v: Int,
  val level: Int,
  protected var topCheckpoint: Option[SearcherAtCheckpoint]
) extends VehicleSearcher(v) {

  /** Returns the start position of a given vehicle in a [[oscar.cbls.algo.sequence.IntSequence]].
    */
  def startPosOfVehicle(vehicle: Int): Int

  /** Drop and applies all pending updates in the stack. */
  def regularize: StackedVehicleSearcher = {
    new ConcreteSearcher(Array.tabulate(v)(startPosOfVehicle), topCheckpoint)
  }

  /** Checks if all the vehicles are well positioned in the sequence `s`. */
  def checkOnSequence(s: IntSequence): Unit = {
    for (vehicle <- 0 until v) {
      require(
        s.positionOfAnyOccurrence(vehicle).get == startPosOfVehicle(vehicle),
        s"Vehicle $vehicle is not on the good position in sequence $s"
      )
    }
  }

  /** @param oldPosToNewPos
    *   A description of the update through a function that maps old position to new position for
    *   all values in the sequence.
    * @param maxStackLevel
    *   The maximal level of stack before a regularization automatically takes place. By default, it
    *   is set to `2 * v`.
    * @return
    *   A vehicle location reflecting the start position of vehicle after the update is performed.
    */
  def push(
    oldPosToNewPos: Int => Option[Int],
    maxStackLevel: Int = 2 * v
  ): StackedVehicleSearcher = {
    val tmp = new StackedSearcher(oldPosToNewPos, this, topCheckpoint)
    if (tmp.level > maxStackLevel) tmp.regularize
    else tmp
  }

  override def defineCheckpoint(
    newCheckpoint: Option[IntSequence] = None
  ): StackedVehicleSearcher = {
    new SearcherAtCheckpoint(this)
  }

  override def releaseTopCheckpoint(): StackedVehicleSearcher = {
    require(topCheckpoint.nonEmpty, s"Cannot release. No checkpoint has been defined")
    require(this == topCheckpoint.get, s"Rollback first before release top checkpoint")
    topCheckpoint.get.prev
  }

  /** Empty the stack by jumping to te top checkpoint. All the updates performed after the
    * checkpoint are deleted.
    */
  override def rollbackToTopCheckpoint(): StackedVehicleSearcher = {
    require(topCheckpoint.nonEmpty, s"Cannot rollback. No checkpoint has been defined")
    topCheckpoint.get
  }

  /** Finds the vehicle that reaches a given position. */
  def vehicleReachingPosition(pos: Int): Int = vehicleBinarySearch(pos, startPosOfVehicle)

}

/** Level 0 of the stack. No update has been performed.
  *
  * @param startPositionOfVehicle
  *   The initial starting position of the vehicles.
  * @param topCheckpoint
  *   An optional previous state in the stack to which we can easily roll back.
  */
class ConcreteSearcher(
  startPositionOfVehicle: Array[Int],
  topCheckpoint: Option[SearcherAtCheckpoint] = None
) extends StackedVehicleSearcher(startPositionOfVehicle.length, 0, topCheckpoint) {

  override def startPosOfVehicle(vehicle: Int): Int = startPositionOfVehicle(vehicle)

  override def regularize: StackedVehicleSearcher = this

  override def toString: String =
    s"ConcreteSearcher(${startPositionOfVehicle.mkString(",")})"
}

/** Structure for stacked updates.
  *
  * @param oldPosToNewPos
  *   How to compute the new positions in the sequences from the positions before the updates.
  * @param prev
  *   The vehicle searcher before the last update.
  * @param topCheckpoint
  *   An optional previous state in the stack to which we can easily roll back.
  */
class StackedSearcher(
  val oldPosToNewPos: Int => Option[Int],
  val prev: StackedVehicleSearcher,
  topCheckpoint: Option[SearcherAtCheckpoint] = None
) extends StackedVehicleSearcher(prev.v, prev.level + 1, topCheckpoint) {

  override def startPosOfVehicle(vehicle: Int): Int =
    oldPosToNewPos(prev.startPosOfVehicle(vehicle)).get

  override def toString: String =
    s"StackedSearcher([${(0 until v).map(startPosOfVehicle).mkString(",")}] " +
      s"depth: $level prev: $prev)"
}

/** Structure to manage checkpoints.
  *
  * @param prev
  *   The searcher used at checkpoint.
  */
class SearcherAtCheckpoint(val prev: StackedVehicleSearcher)
    extends StackedVehicleSearcher(prev.v, prev.level + 1, None) {

  topCheckpoint = Some(this)

  override def startPosOfVehicle(vehicle: Int): Int = prev.startPosOfVehicle(vehicle)

  override def toString: String = {
    s"Searcher at checkpoint:\t$prev"
  }
}
