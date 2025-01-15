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

package oscar.cbls.test.modeling.routing

import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.seq._
import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.modeling.routing.{CachedVehicleSearcher, StackedVehicleSearcher}

/** Test invariant which maintains a [[StackedVehicleSearcher]] and a [[CachedVehicleSearcher]]
  * updated.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param input
  *   Sequence which is supposed to contains vehicles in increasing order.
  */
private[routing] class UpdateVehicleSearchersInvariant(
  model: Store,
  input: SeqVariable,
  numVehicle: Int
) extends Invariant(model, None)
    with SeqNotificationTarget {

  input.registerStaticallyAndDynamicallyListeningElement(this)

  var stackedVehicleSearcher: StackedVehicleSearcher =
    StackedVehicleSearcher(input.value(), numVehicle)
  var cachedVehicleSearcher: CachedVehicleSearcher =
    CachedVehicleSearcher(input.value(), numVehicle)

  override def notifySeqChanges(
    seqVariable: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit = {
    digestUpdates(changes)
  }

  override def checkInternals(): Unit = {
    for (pos <- 0 until input.pendingValue.size) {
      val stackedSearcher = stackedVehicleSearcher.vehicleReachingPosition(pos)
      val cachedSearcher  = cachedVehicleSearcher.vehicleReachingPosition(pos, input.pendingValue)
      val expected        = findVehicleFromScratch(pos)
      require(
        stackedSearcher == expected,
        s"""Stacked Vehicle searcher don't return the expected result:
           |Position: $pos
           |VehicleSearcher: $stackedSearcher
           |Expected: $expected
           |""".stripMargin
      )

      require(
        cachedSearcher == expected,
        s"""Cached Vehicle searcher don't return the expected result:
           |Position: $pos
           |VehicleSearcher: $cachedSearcher
           |Expected: $expected
           |""".stripMargin
      )

    }
  }

  private def findVehicleFromScratch(pos: Int): Int = {
    var currentExplorer = input.pendingValue.explorerAtPosition(pos).get
    while (currentExplorer.position >= 0 && currentExplorer.value >= numVehicle) {
      currentExplorer = currentExplorer.prev
    }
    currentExplorer.value
  }

  private def digestUpdates(changes: SeqUpdate): Unit = {
    changes match {
      case SeqUpdateAssign(seq: IntSequence) =>
        require(
          seq.unorderedContentNoDuplicate.count(_ < numVehicle) == numVehicle,
          "The assigned sequence has not the expected number of vehicle"
        )
        stackedVehicleSearcher = StackedVehicleSearcher(seq, numVehicle)
        cachedVehicleSearcher = CachedVehicleSearcher(seq, numVehicle)

      case SeqUpdateDefineCheckpoint(prev: SeqUpdate, _: Int) =>
        digestUpdates(prev)
        stackedVehicleSearcher = stackedVehicleSearcher.defineCheckpoint()
        cachedVehicleSearcher = cachedVehicleSearcher.defineCheckpoint(Some(prev.newValue))

      case surbttc: SeqUpdateRollBackToTopCheckpoint =>
        digestUpdates(surbttc.prev)
        stackedVehicleSearcher = stackedVehicleSearcher.rollbackToTopCheckpoint()
        cachedVehicleSearcher = cachedVehicleSearcher.rollbackToTopCheckpoint()

      case surtc: SeqUpdateReleaseTopCheckpoint =>
        digestUpdates(surtc.prev)
        stackedVehicleSearcher = stackedVehicleSearcher.releaseTopCheckpoint()
        cachedVehicleSearcher = cachedVehicleSearcher.releaseTopCheckpoint()

      case x: SeqUpdateWithPrev =>
        digestUpdates(x.prev)
        stackedVehicleSearcher = stackedVehicleSearcher.push(x.oldPosToNewPos)

      case _: SeqUpdateLastNotified =>

      case x => require(requirement = false, s"Try unhandled update $x")
    }

  }
}
