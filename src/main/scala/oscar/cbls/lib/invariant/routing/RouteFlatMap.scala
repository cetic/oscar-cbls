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

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.Invariant
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.seq._
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.modeling.routing.VRP

import scala.collection.immutable.{HashMap, HashSet}

/** Companion object of the [[RouteFlatMap]] class. */
object RouteFlatMap {

  /** Creates a RouteFlatMap invariant, which maintains for a given routing problem, `{f(i, j) | `
    * `the edge i -> j is in the sequence}`. If `f(i, i)` is not an empty set, the invariant also
    * consider the self-edge `i -> i`.
    *
    * @param vrp
    *   The object that represents the Vehicle Routing Problem.
    * @param fun
    *   The function defining the mapping.
    * @param name
    *   The name of the Invariant.
    */
  def apply(vrp: VRP, fun: (Int, Int) => Set[Int], name: String = "RouteFlatMap"): RouteFlatMap = {
    val output        = SetVariable(vrp.model, Set.empty)
    val numDuplicated = IntVariable(vrp.model, 0L)
    new RouteFlatMap(vrp, fun, output, numDuplicated, name)
  }
}

/** An invariant which maintains for a given routing problem, `{f(i, j) | the edge i -> j is in`
  * `the sequence}`. If `f(i, i)` is not an empty set, the invariant also consider the self-edge `i`
  * `-> i`.
  *
  * @param vrp
  *   The object that represents the Vehicle Routing Problem.
  * @param fun
  *   The function defining the mapping.
  * @param output
  *   The output of the invariant.
  * @param numDuplicates
  *   The sum of the values with an occurrence > 1.
  * @param name
  *   The name of the Invariant.
  */
class RouteFlatMap(
  vrp: VRP,
  fun: (Int, Int) => Set[Int],
  val output: SetVariable,
  val numDuplicates: IntVariable,
  name: String
) extends Invariant(vrp.model, Some(name))
    with SeqNotificationTarget {

  for (i <- 0 until vrp.n) {
    for (j <- i until vrp.n) {
      require(fun(i, j) == fun(j, i), "The input function must be symmetrical")
    }
  }

  /** Internal class used to stock the value of the output, the number of times an output value has
    * been the result of `fun` according to the value of the route and how many values are the
    * result of `fun` many times.
    *
    * '''Note: ''' `outputCount(x)` can never be 0 (meaning that no edge has `x` as a result).
    */
  private[this] case class FlatMapInternalState(
    value: HashSet[Int] = HashSet.empty,
    outputCount: HashMap[Int, Int] = HashMap.empty,
    duplicates: Int = 0
  ) {

    /** Updates the internal state when a new segment is inserted in the sequence. */
    def insertSegment(from: Int, to: Int): FlatMapInternalState = {
      val mappedSegment: Set[Int]     = fun(from, to)
      var newValue: HashSet[Int]      = value
      var newCount: HashMap[Int, Int] = outputCount
      var newDuplicated               = duplicates
      for (ms <- mappedSegment) {
        val count: Int = newCount.getOrElse(ms, 0)
        if (count == 0) newValue += ms
        else if (count >= 1) newDuplicated += 1
        newCount += (ms -> (count + 1))
      }
      FlatMapInternalState(newValue, newCount, newDuplicated)
    }

    /** Updates the internal state when a segment is removed from the sequence. */
    def removeSegment(from: Int, to: Int): FlatMapInternalState = {
      val mappedSegment: Set[Int]     = fun(from, to)
      var newValue: HashSet[Int]      = value
      var newCount: HashMap[Int, Int] = outputCount
      var newDuplicated: Int          = duplicates
      for (ms <- mappedSegment) {
        val count: Int = newCount(ms)
        assert(count != 0, s"No input segment has mapped value $ms")
        if (count == 1) {
          newValue -= ms
          newCount -= ms
        } else {
          newDuplicated -= 1
          newCount += (ms -> (count - 1))
        }
      }
      FlatMapInternalState(newValue, newCount, newDuplicated)
    }
  }

  private[this] val routes: SeqVariable                = vrp.routes
  private[this] var currentState: FlatMapInternalState = computeFlatMapFromScratch(routes.value())
  // Used to manage checkpoints
  private[this] val stackedCheckpoints: SeqValueStack[FlatMapInternalState] =
    new SeqValueStack[FlatMapInternalState]()

  routes.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)
  numDuplicates.setDefiningInvariant(this)

  output        := currentState.value
  numDuplicates := currentState.duplicates

  /** Returns the number of `f(i, j)` such that `i -> j` is an edge of the route and `x in f(i, j)`.
    */
  def nbOccurrence(x: Int): Int = currentState.outputCount.getOrElse(x, 0)

  override def notifySeqChanges(
    seqVariable: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit = {
    currentState = digestUpdate(changes)
    output        := currentState.value
    numDuplicates := currentState.duplicates
  }

  override def checkInternals(): Unit = {
    val expected: HashSet[Int] = computeFlatMapFromScratch(routes.pendingValue).value

    val vehicleTrips: Iterable[List[Int]] =
      vrp.mapVehicleToRoute.map({ case (vehicle, route) => route.appended(vehicle) })

    def detailedTrip(trip: List[Int]): String = {
      val fromTo: List[(Int, Int)] = trip.tail.tail
        .foldLeft(List((trip.head, trip.tail.head)))((pairs, to) => {
          val from = pairs.last._2
          pairs.appended((from, to))
        })

      // add node to itself
      val toReturn: List[(Int, Int)] =
        if (trip.length == 2) fromTo
        else trip.tail.foldLeft(fromTo)((t, node) => t.appended((node, node)))

      toReturn
        .map({ case (from, to) => s"$from -> $to: ${fun(from, to)}" })
        .mkString("\n")
    }

    val routesString: String = vehicleTrips.map(trip => detailedTrip(trip)).mkString("\n\n")

    require(
      output.pendingValue == expected,
      s"""checkInternal fails in invariant ${name()}
         |Got: ${output.pendingValue}
         |Expected: $expected
         |Current seq: ${routes.pendingValue}
         |Current routes maps:
         |$routesString
         |""".stripMargin
    )

    val expectedDuplicates = currentState.outputCount
      .filter({ case (_, v) => v > 1 })
      .foldLeft(0)({ case (acc, (_, v)) => acc + v - 1 })
    require(
      numDuplicates.pendingValue == expectedDuplicates,
      s"""checkInternals fails in invariant ${name()} while checking the number of duplicated values
         |Got: ${numDuplicates.value()}
         |Expected: $expectedDuplicates
         |""".stripMargin
    )

  }

  private[this] def computeFlatMapFromScratch(seq: IntSequence): FlatMapInternalState = {
    var state: FlatMapInternalState = FlatMapInternalState()
    var prevExplorer                = seq.explorerAtPosition(0).get
    var explorer                    = prevExplorer.next

    while (explorer.position < seq.size) {
      state =
        if (explorer.value < vrp.v) // Next node is a new vehicle
          state.insertSegment(prevExplorer.value, explorer.value - 1)
        else
          state.insertSegment(prevExplorer.value, explorer.value)

      // add map values from node to itself
      state = state.insertSegment(prevExplorer.value, prevExplorer.value)

      prevExplorer = explorer
      explorer = explorer.next
    }

    // add map values from last node to itself
    state = state.insertSegment(prevExplorer.value, prevExplorer.value)

    // Add the segment from the last routed node to its depot
    if (prevExplorer.value >= vrp.v) // Else the segment is already inserted
      state = state.insertSegment(prevExplorer.value, vrp.v - 1)

    state
  }

  private[this] def digestUpdate(changes: SeqUpdate): FlatMapInternalState = {
    changes match {
      case SeqUpdateInsert(insertedVal: Int, afterPosExp: IntSequenceExplorer, prev: SeqUpdate) =>
        var state           = digestUpdate(prev)
        val nodeBefore: Int = afterPosExp.value
        val nodeAfter: Int  = vrp.nextNodeInRouting(afterPosExp)
        if (nodeBefore != nodeAfter) state = state.removeSegment(nodeBefore, nodeAfter)
        state = state.insertSegment(nodeBefore, insertedVal)
        state = state.insertSegment(insertedVal, nodeAfter)
        state = state.insertSegment(insertedVal, insertedVal)
        state

      case SeqUpdateRemove(removedExp: IntSequenceExplorer, prev: SeqUpdate) =>
        var state           = digestUpdate(prev)
        val nodeBefore: Int = removedExp.prev.value
        val nodeAfter: Int  = vrp.nextNodeInRouting(removedExp)
        state = state.removeSegment(nodeBefore, removedExp.value)
        state = state.removeSegment(removedExp.value, nodeAfter)
        state = state.removeSegment(removedExp.value, removedExp.value)
        if (nodeBefore != nodeAfter) state = state.insertSegment(nodeBefore, nodeAfter)
        state

      case SeqUpdateMove(
            fromExp: IntSequenceExplorer,
            toExp: IntSequenceExplorer,
            afterPosExp: IntSequenceExplorer,
            flip: Boolean,
            prev: SeqUpdate
          ) =>
        var state                 = digestUpdate(prev)
        val nodeBeforeSource: Int = fromExp.prev.value
        val nodeAfterSource: Int  = vrp.nextNodeInRouting(toExp)
        val nodeBeforeDest: Int   = afterPosExp.value
        val nodeAfterDest: Int =
          if (nodeBeforeSource == afterPosExp.value) nodeAfterSource
          else vrp.nextNodeInRouting(afterPosExp)
        val (startSeg: Int, endSeg: Int) =
          if (flip) (toExp.value, fromExp.value) else (fromExp.value, toExp.value)

        state = state.removeSegment(nodeBeforeSource, fromExp.value)
        state = state.removeSegment(toExp.value, nodeAfterSource)
        if (nodeBeforeSource != nodeAfterSource)
          state = state.insertSegment(nodeBeforeSource, nodeAfterSource)
        if (nodeBeforeDest != nodeAfterDest)
          state = state.removeSegment(nodeBeforeDest, nodeAfterDest)
        state = state.insertSegment(nodeBeforeDest, startSeg)
        state = state.insertSegment(endSeg, nodeAfterDest)
        state

      case SeqUpdateAssign(newSeq: IntSequence) =>
        computeFlatMapFromScratch(newSeq)

      case defCheckpoint @ SeqUpdateDefineCheckpoint(prev: SeqUpdate, level: Int) =>
        val state = digestUpdate(prev)
        stackedCheckpoints.pushToLevel(defCheckpoint.newValue, level, state)
        state

      case release: SeqUpdateReleaseTopCheckpoint =>
        val state = digestUpdate(release.prev)
        stackedCheckpoints.pop()
        state

      case SeqUpdateRollBackToTopCheckpoint(
            checkpoint: IntSequence,
            howToRollback: SeqUpdate,
            level: Int,
            _
          ) =>
        rollbackUpdates(howToRollback)
        assert(
          stackedCheckpoints.stackLevel == level,
          s"Checkpoint levels are not coherent (sequence level: $level != this invariant level: ${stackedCheckpoints.stackLevel})"
        )
        stackedCheckpoints.popUntilLevel(checkpoint, level)
      case _: SeqUpdateLastNotified => currentState
    }
  }

  // When rolling back, we only need to digest the "release top checkpoint" to maintains
  // a good stack level and the other rollbacks. Other updates can be ignored.
  private[this] def rollbackUpdates(howToRollback: SeqUpdate): Unit = {
    howToRollback match {
      case release: SeqUpdateReleaseTopCheckpoint =>
        rollbackUpdates(release.prev)
        stackedCheckpoints.pop()

      case SeqUpdateRollBackToTopCheckpoint(
            checkpoint: IntSequence,
            howToRollback: SeqUpdate,
            level: Int,
            _
          ) =>
        rollbackUpdates(howToRollback)
        assert(
          stackedCheckpoints.stackLevel == level,
          s"Checkpoint levels are not coherent (sequence level: $level != this invariant level: ${stackedCheckpoints.stackLevel})"
        )
        stackedCheckpoints.popUntilLevel(checkpoint, level)
      case _: SeqUpdateLastNotified =>
      case x: SeqUpdateWithPrev     => rollbackUpdates(x.prev)
      case x: SeqUpdate => throw new IllegalArgumentException(s"Unexpected rollback: $x")
    }
  }

}
