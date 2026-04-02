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

import oscar.cbls.algo.pairs.Pairs
import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.Invariant
import oscar.cbls.core.computation.seq._
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.modeling.routing.VRS

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Companion object of the [[RouteFlatMap]] class. */
object RouteFlatMap {

  /** Creates a RouteFlatMap invariant, which maintains for a given routing problem, `{f(i, j) | `
    * `the edge i -> j is in the sequence}`. If `f(i, i)` is not an empty set, the invariant also
    * consider the self-edge `i -> i`.
    *
    * @param vrs
    *   The object that represents the vehicle routing structure.
    * @param fun
    *   The function defining the mapping.
    * @param isSymmetric
    *   Whether the input function is symmetric. If `true`, it allows some speed-up for updates.
    * @param name
    *   The name of the Invariant.
    */
  def apply(
    vrs: VRS,
    fun: (Int, Int) => Iterable[Int],
    isSymmetric: Boolean = true,
    name: String = "RouteFlatMap"
  ): RouteFlatMap = {
    val output = SetVariable(vrs.store, Set.empty)
    new RouteFlatMap(vrs, fun, output, isSymmetric, name)
  }
}

/** An invariant which maintains for a given routing problem, `{f(i, j) | the edge i -> j is in`
  * `the sequence}`. If `f(i, i)` is not an empty set, the invariant also consider the self-edge `i`
  * `-> i`.
  *
  * @param vrs
  *   The object that represents the vehicle routing structure.
  * @param fun
  *   The function defining the mapping.
  * @param output
  *   The output of the invariant.
  * @param isSymmetric
  *   Whether the input function is symmetric. If `true`, it allows some speed-up for updates.
  * @param name
  *   The name of the Invariant.
  */
class RouteFlatMap(
  vrs: VRS,
  fun: (Int, Int) => Iterable[Int],
  val output: SetVariable,
  isSymmetric: Boolean,
  name: String
) extends Invariant(vrs.store, Some(name))
    with SeqNotificationTarget {

  vrs.routes.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  private[this] var outputCount: mutable.HashMap[Int, Int] = mutable.HashMap.empty

  resetOutputs(vrs.routes.value())

  /** Returns the number of `f(i, j)` such that `i -> j` is an edge of the route and `x in f(i, j)`.
    */
  def nbOccurrence(x: Int): Int = outputCount.getOrElse(x, 0)

  override def notifySeqChanges(
    seqVariable: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit = digestUpdate(changes)

  override def checkInternals(): Unit = {
    val (expected, expectedDuplicates) = computeFromScratch(vrs.routes.pendingValue)

    var routesString = ""
    for ((from, to) <- Pairs.pairsOfAdjacentInRoute(vrs.routes.pendingValue, vrs.v)) {
      routesString += s"$from -> $from: ${fun(from, from)}\n"
      if (from != to) routesString += s"$from -> $to: ${fun(from, to)}\n"
      if (to < vrs.v) routesString += "\n"
    }

    require(
      output.pendingValue == expected,
      s"""checkInternal fails in invariant ${name()}.
         |Got: ${output.pendingValue}
         |Expected: $expected
         |Current seq: ${vrs.routes.pendingValue}
         |Current routes maps:
         |$routesString
         |""".stripMargin
    )

  }

  /** Computes from scratch the expected flat map and the total number of duplicated values.
    */
  private[this] def computeFromScratch(seq: IntSequence): (HashSet[Int], Int) = {
    val allElement: ListBuffer[Int] = ListBuffer.empty
    for ((from, to) <- Pairs.pairsOfAdjacentInRoute(seq, vrs.v)) {
      if (from != to) allElement ++= fun(from, to)
      allElement ++= fun(from, from)
    }
    val flatten = HashSet.from(allElement)
    val duplicates = flatten.foldLeft(0)((acc, x) => {
      val count = allElement.count(_ == x)
      if (count > 1) acc + count - 1
      else acc
    })

    (flatten, duplicates)
  }

  /** Resets the outputs. */
  private[this] def resetOutputs(route: IntSequence): Unit = {
    outputCount = mutable.HashMap.empty
    output := HashSet.empty
    for ((from, to) <- Pairs.pairsOfAdjacentInRoute(route, vrs.v)) {
      if (from != to) updateAfterInsert(from, to)
      updateAfterInsert(from, from)
    }

  }

  /** Updates the output values after inserting the `from -> to` edge. */
  private[this] def updateAfterInsert(from: Int, to: Int): Unit = {
    val mappedValue: Iterable[Int] = fun(from, to)
    for (value <- mappedValue) {
      val count: Int = outputCount.getOrElse(value, 0)
      if (count == 0) output :+= value
      outputCount(value) = count + 1
    }
  }

  /** Updates the output values after removing the `from -> to` edge. */
  private[this] def updateAfterRemove(from: Int, to: Int): Unit = {
    val mappedValue: Iterable[Int] = fun(from, to)
    for (value <- mappedValue) {
      val count: Int = outputCount(value)
      require(count != 0, s"No input has $value as mapped value.")
      if (count == 1) output :-= value
      outputCount(value) -= 1
    }
  }

  private[this] def digestUpdate(changes: SeqUpdate): Unit = {
    changes match {
      case SeqUpdateInsert(insertedVal: Int, afterPosExp: IntSequenceExplorer, prev: SeqUpdate) =>
        digestUpdate(prev)
        val nodeBefore: Int = afterPosExp.value
        val nodeAfter: Int  = vrs.nextNodeInRouting(afterPosExp)
        if (nodeBefore != nodeAfter) updateAfterRemove(nodeBefore, nodeAfter)
        updateAfterInsert(nodeBefore, insertedVal)
        updateAfterInsert(insertedVal, nodeAfter)
        updateAfterInsert(insertedVal, insertedVal)

      case SeqUpdateRemove(removedExp: IntSequenceExplorer, prev: SeqUpdate) =>
        digestUpdate(prev)
        val nodeBefore: Int = removedExp.prev.value
        val nodeAfter: Int  = vrs.nextNodeInRouting(removedExp)
        updateAfterRemove(nodeBefore, removedExp.value)
        updateAfterRemove(removedExp.value, nodeAfter)
        updateAfterRemove(removedExp.value, removedExp.value)
        if (nodeBefore != nodeAfter) updateAfterInsert(nodeBefore, nodeAfter)

      case sum @ SeqUpdateMove(
            fromExp: IntSequenceExplorer,
            toExp: IntSequenceExplorer,
            afterPosExp: IntSequenceExplorer,
            flip: Boolean,
            prev: SeqUpdate
          ) =>
        digestUpdate(prev)
        val nodeBeforeSource: Int = fromExp.prev.value
        val nodeAfterSource: Int  = vrs.nextNodeInRouting(toExp)
        val nodeBeforeDest: Int   = afterPosExp.value
        val nodeAfterDest: Int =
          if (nodeBeforeSource == afterPosExp.value) nodeAfterSource
          else vrs.nextNodeInRouting(afterPosExp)
        val (startSeg: Int, endSeg: Int) =
          if (flip) (toExp.value, fromExp.value) else (fromExp.value, toExp.value)

        updateAfterRemove(nodeBeforeSource, fromExp.value)
        updateAfterRemove(toExp.value, nodeAfterSource)
        if (nodeBeforeSource != nodeAfterSource)
          updateAfterInsert(nodeBeforeSource, nodeAfterSource)
        if (nodeBeforeDest != nodeAfterDest)
          updateAfterRemove(nodeBeforeDest, nodeAfterDest)
        updateAfterInsert(nodeBeforeDest, startSeg)
        updateAfterInsert(endSeg, nodeAfterDest)

        if (!isSymmetric && flip) {
          for ((from, to) <- Pairs.pairsOfAdjacent(sum.movedValues)) {
            updateAfterRemove(from, to)
            updateAfterInsert(to, from)
          }
        }

      case SeqUpdateAssign(newSeq: IntSequence) =>
        resetOutputs(newSeq)

      case defCheckpoint: SeqUpdateDefineCheckpoint =>
        digestUpdate(defCheckpoint.prev)

      case release: SeqUpdateReleaseTopCheckpoint =>
        digestUpdate(release.prev)

      case rollback: SeqUpdateRollBackToTopCheckpoint =>
        digestUpdate(rollback.howToRollBack)

      case _: SeqUpdateLastNotified => ;
      case x: SeqUpdate             => throw new IllegalArgumentException(s"Unexpected update $x")
    }
  }

}
