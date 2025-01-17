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

/** Companion object of the [[CachedVehicleSearcher]] class. */
object CachedVehicleSearcher {

  /** @param routes
    *   A sequence containing only the vehicles in increasing order.
    * @return
    *   An instance of CachedVehicleSearcher.
    */
  def apply(routes: SeqVariable): CachedVehicleSearcher =
    CachedVehicleSearcher(routes.pendingValue)

  /** @param checkpoint
    *   A sequence containing only the vehicles in increasing order.
    * @return
    *   An instance of CachedVehicleSearcher.
    */
  def apply(checkpoint: IntSequence): CachedVehicleSearcher =
    new CachedVehicleSearcher(checkpoint, checkpoint.size)

  /** @param checkpoint
    *   A checkpoint related to a route.
    * @param v
    *   The number of vehicle in the route.
    * @return
    *   An instance of CachedVehicleSearcher.
    */
  def apply(checkpoint: IntSequence, v: Int): CachedVehicleSearcher =
    new CachedVehicleSearcher(checkpoint, v)

}

/** Class to find which vehicle reaches a node in a routes. This class use a cache to speed up the
  * search when the routes are at checkpoint.
  *
  * @param checkpoint
  *   The sequence defining the cache.
  * @param v
  *   The number of vehicle in the routes.
  */
class CachedVehicleSearcher(checkpoint: IntSequence, v: Int) extends VehicleSearcher(v) {

  private[this] val batch: Array[Int] =
    Array.tabulate(v)(vehicle => checkpoint.positionOfAnyOccurrence(vehicle).get)

  private var searcherAtPreviousCheckpoint = this

  /** Finds the vehicle that reaches a given position.
    *
    * @param position
    *   Target position of the searched vehicle.
    * @param seq
    *   Sequence on which to find the vehicle. If this sequence is the same that the checkpoint
    *   defining this searcher, the cache is used to perform the search. Otherwise, a binary search
    *   is performed on all the sequence
    * @return
    *   The vehicle in the sequence that reaches the given position.
    */
  def vehicleReachingPosition(position: Int, seq: IntSequence): Int = {
    vehicleBinarySearch(
      position,
      (v: Int) => startPosOfVehicle(v, seq, onCache = seq sameIdentity checkpoint)
    )
  }

  override def defineCheckpoint(newCheckpoint: Option[IntSequence]): CachedVehicleSearcher = {
    val cvs = CachedVehicleSearcher(newCheckpoint.get, this.v)
    cvs.searcherAtPreviousCheckpoint = this
    cvs
  }

  override def rollbackToTopCheckpoint(): CachedVehicleSearcher = this

  override def releaseTopCheckpoint(): CachedVehicleSearcher = searcherAtPreviousCheckpoint

  private[this] def startPosOfVehicle(vehicle: Int, seq: IntSequence, onCache: Boolean): Int = {
    if (onCache) batch(vehicle)
    else seq.positionOfAnyOccurrence(vehicle).get
  }

}
