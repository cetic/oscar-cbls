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

package oscar.cbls.algo.sequence.affineFunction

import oscar.cbls.algo.rb.{RedBlackTreeMap, RedBlackTreeMapExplorer}
import oscar.cbls.algo.sequence.IntSequence

import scala.annotation.tailrec
import scala.language.implicitConversions

/** Companion object of [[PiecewiseUnitaryAffineFunction]] */
object PiecewiseUnitaryAffineFunction {

  /** Returns an empty [[PiecewiseUnitaryAffineFunction]] */
  def identity = new PiecewiseUnitaryAffineFunction()

  /** Creates a [[PiecewiseUnitaryAffineFunction]] based on a existing list of [[Pivot]] */
  def createFromPivots(pivots: Iterable[Pivot]): PiecewiseUnitaryAffineFunction = {
    var acc     = RedBlackTreeMap.empty[Pivot]
    val pivotIt = pivots.iterator
    while (pivotIt.hasNext) {
      val currentPivot = pivotIt.next()
      acc = acc.insert(currentPivot.fromValue, currentPivot)
    }
    new PiecewiseUnitaryAffineFunction(acc)
  }

  @tailrec
  def computeInvertedPivots(
    remainingPivots: List[Pivot],
    newPivots: List[Pivot] = List.empty
  ): List[Pivot] = {
    remainingPivots match {
      case Nil => newPivots
      case p1 :: p2 :: tail =>
        val fun      = p1.f
        val invert   = fun.invert
        val newPivot = new Pivot(fun(if (fun.flip) p2.fromValue - 1 else p1.fromValue), invert)
        computeInvertedPivots(p2 :: tail, List(newPivot) ::: newPivots)
      case p1 :: _ =>
        val fun = p1.f
        require(!fun.flip)
        val invert = fun.invert
        List(new Pivot(fun(p1.fromValue), invert)) ::: newPivots
    }
  }

  implicit def toIterable(f: PiecewiseUnitaryAffineFunction): Iterable[Pivot] =
    f.transformation.values
}

/** A piecewise unitary affine function matching the external and the internal position of each
  * element of a [[IntSequence]].
  *
  * To avoid expensive modification by moving values around upon insertion/deletion/movements in the
  * [[IntSequence]], we use a PiecewiseSequenceShiftingBijection. It's composed of a sorted list of
  * [[Pivot]] (stored in a [[oscar.cbls.algo.rb.RedBlackTreeMap]]) which represents the changes made
  * to the [[IntSequence]]. Each [[Pivot]] starts at a position and applies a bijection to the start
  * position and each position after. That way we know the new position of the element at position
  * x. Those are created rapidly and can be merged (See [[UnitaryAffineFunction]] for more
  * information).
  *
  * Once in a while we create a brand new [[IntSequence]] to avoid having too many Pivots.
  *
  * @param transformation
  *   a RedBlackTree keeping the [[Pivot]] sorted by their [[Pivot.fromValue]]
  */
class PiecewiseUnitaryAffineFunction(
  private[sequence] val transformation: RedBlackTreeMap[Pivot] = RedBlackTreeMap.empty
) {

  /** No recorded pivot */
  def isIdentity: Boolean = transformation.isEmpty

  /** The backward [[PiecewiseUnitaryAffineFunction]] such that backward(this(x)) = x */
  lazy val backward: PiecewiseUnitaryAffineFunction = {
    PiecewiseUnitaryAffineFunction.createFromPivots(
      PiecewiseUnitaryAffineFunction.computeInvertedPivots(pivots)
    )
  }

  /** Returns the new position of the old position specified as value.
    *
    * If there is no [[Pivot]] starting before value, it means that there was no move affecting this
    * position, hence returning the old position itself. Otherwise returns the new position using
    * the [[UnitaryAffineFunction]] of the [[Pivot]]
    *
    * @param oldPos
    *   The old position
    * @return
    *   The new position of the old position.
    */
  def apply(oldPos: Int): Int = {
    transformation.biggestLowerOrEqual(oldPos) match {
      case None             => oldPos
      case Some((_, pivot)) => pivot.f(oldPos)
    }
  }

  /** Returns the old position of the new position specified as value.
    *
    * It uses the backward version of the this class.
    *
    * @param newPosition
    *   The new position
    * @return
    *   The old position of the new position.
    */
  def unApply(newPosition: Int): Int = backward(newPosition)

  ///////////////////
  // Pivots access //
  ///////////////////

  /** Returns the [[scala.List]] of all [[Pivot]] */
  def pivots: List[Pivot] = transformation.values

  /** Returns the number of [[Pivot]] */
  def nbPivot: Int = transformation.size

  /** Optionally returns a [[oscar.cbls.algo.rb.RedBlackTreeMapExplorer]] of the first pivot of the sequence.
    *
    * @return
    *   The [[oscar.cbls.algo.rb.RedBlackTreeMapExplorer]] or [[scala.None]]
    */
  def firstPivotAndPosition: Option[RedBlackTreeMapExplorer[Pivot]] = {
    transformation.smallest match {
      case None                        => None
      case Some((fromValueOfPivot, _)) => positionOfValue(fromValueOfPivot)
    }
  }

  /** Optionally returns a [[oscar.cbls.algo.rb.RedBlackTreeMapExplorer]] of the pivot applying at the given position.
    *
    * @param position
    *   The position on which the pivot must apply
    * @return
    *   The [[oscar.cbls.algo.rb.RedBlackTreeMapExplorer]] or [[scala.None]]
    */
  def pivotWithPositionApplyingTo(position: Int): Option[RedBlackTreeMapExplorer[Pivot]] = {
    transformation.biggestLowerOrEqual(position) match {
      case None                        => None
      case Some((fromValueOfPivot, _)) => positionOfValue(fromValueOfPivot)
    }
  }

  /** Optionally returns a [[oscar.cbls.algo.rb.RedBlackTreeMapExplorer]] at the corresponding key
    */
  def positionOfValue(value: Int): Option[RedBlackTreeMapExplorer[Pivot]] =
    transformation.positionOf(value)

  //////////////////////////
  // Pivots manipulations //
  //////////////////////////

  /** Applies a list of updates one by one.
    *
    * For each update :
    *   - Removes [[Pivot]] in the update's interval
    *   - Applies the update before applying the previous pivot
    *   - Deletes unnecessary pivots that may have appear in the process
    *
    * @param updates
    *   the list of updates
    * @param updatedTransform
    *   the recursively updated transform
    * @return
    *   The [[PiecewiseUnitaryAffineFunction]] with all the updates
    */
  @tailrec
  final def updatesForCompositionBefore(
    updates: List[(Int, Int, UnitaryAffineFunction)],
    updatedTransform: RedBlackTreeMap[Pivot] = transformation
  ): PiecewiseUnitaryAffineFunction = {
    updates match {
      case Nil => new PiecewiseUnitaryAffineFunction(updatedTransform)
      case (fromIncluded, toIncluded, update) :: tail =>
        updatesForCompositionBefore(
          tail,
          deleteUnnecessaryPivotStartingJustAfter(
            toIncluded,
            myUpdateForCompositionBefore(
              fromIncluded,
              toIncluded,
              update,
              removePivotsBetween(fromIncluded, toIncluded, updatedTransform)
            )
          )
        )
    }
  }

  /** Applies the additional [[UnitaryAffineFunction]] in the defined interval before existing
    * bijection.
    *
    * @param fromIncluded
    *   Starting position of the interval
    * @param toIncluded
    *   Ending position of the interval
    * @param additionalBijectionAppliedBefore
    *   The additional [[UnitaryAffineFunction]] to apply before
    * @return
    *   An updated [[PiecewiseUnitaryAffineFunction]]
    */
  def updateForCompositionBefore(
    fromIncluded: Int,
    toIncluded: Int,
    additionalBijectionAppliedBefore: UnitaryAffineFunction
  ): PiecewiseUnitaryAffineFunction = {

    // Removes all pivots between fromIncluded and toIncluded since we must apply the additional bijection before.
    val cleanedTransformation = removePivotsBetween(fromIncluded, toIncluded, transformation)
    val updatedTransform = myUpdateForCompositionBefore(
      fromIncluded,
      toIncluded,
      additionalBijectionAppliedBefore,
      cleanedTransformation
    )
    val updatedTransformDeletedExtraPivot =
      deleteUnnecessaryPivotStartingJustAfter(toIncluded, updatedTransform)
    new PiecewiseUnitaryAffineFunction(updatedTransformDeletedExtraPivot)
  }

  /** Makes the composition of a [[UnitaryAffineFunction]] (BEFORE) and existing bijection in the
    * defined interval.
    *
    * The idea is to create to composition of the existing bijections (aka THIS) and the specified
    * bijection (aka BEFORE). The implemented composition formula is THIS°BEFORE <=>
    * THIS(BEFORE(x)). To do that we need several things :
    *   - The defined interval were we want to do the composition
    *   - The "before" bijection
    *   - The "this" bijections (they are in the class variable transform)
    *   - A cleaned version of the current transform variable
    *
    * The algorithm is driven by the before bijection (flipping or not flipping) and the "this"
    * bijections.
    *
    * @param fromIncluded
    *   Starting position of the interval
    * @param toIncluded
    *   Ending position of the interval
    * @param additionalBijectionAppliedBefore
    *   The [[UnitaryAffineFunction]] to apply before
    * @param cleanedTransformation
    *   A RBTree were all pivot between fromIncluded and toIncluded were removed
    * @return
    *   An updated [[PiecewiseUnitaryAffineFunction]]
    */
  private def myUpdateForCompositionBefore(
    fromIncluded: Int,
    toIncluded: Int,
    additionalBijectionAppliedBefore: UnitaryAffineFunction,
    cleanedTransformation: RedBlackTreeMap[Pivot]
  ): RedBlackTreeMap[Pivot] = {
    // If true, BEFORE(fromIncluded) > BEFORE(toIncluded) &rarr; "we go backward"
    val isAdditionalBijectionNegativeSlope = additionalBijectionAppliedBefore.flip

    var currentFromIncluded   = fromIncluded
    var currentTransformation = cleanedTransformation
    // BEFORE(currentFromIncluded) &rarr; this position can be used in THIS(...) since it's THIS(BEFORE(x))
    var currentIncludedFromAfterAdditionalF = additionalBijectionAppliedBefore(currentFromIncluded)
    // Checking if one Pivot applies on BEFORE(currentFromIncluded)
    var positionOfPivotApplyingOnCurrentIncludedFromAfterAdditionalF =
      transformation.biggestLowerOrEqual(currentIncludedFromAfterAdditionalF) match {
        // No pivot applying at this position, considers identity function.
        case None => None
        // Pivot applying at this position
        case Some((key, _)) => positionOfValue(key)
      }

    while (currentFromIncluded <= toIncluded) {
      positionOfPivotApplyingOnCurrentIncludedFromAfterAdditionalF match {
        // No pivot at this point => (THIS(BEFORE(x)) == BEFORE(x)), but until when ?
        case None =>
          val (nextCurrentIncludedFrom, nextPivotExplorer) = {
            // Going backward &rarr; no bijection until toIncluded &rarr; DONE
            if (isAdditionalBijectionNegativeSlope) {
              (toIncluded + 1, None)
            } else {
              // Going forward &rarr; Get first bijection of transformation, maybe it's within interval
              val nextPivotExplorer: Option[RedBlackTreeMapExplorer[Pivot]] =
                transformation.smallestPosition
              (
                nextPivotExplorer match {
                  // No bijection &rarr; DONE
                  case None => toIncluded + 1
                  // The position x such that BEFORE(x) is explorer.value.fromValue
                  // to keep the THIS(BEFORE(x)) logic
                  case Some(explorer) =>
                    additionalBijectionAppliedBefore.unApply(explorer.value.fromValue)
                },
                nextPivotExplorer
              )
            }
          }

          // Since there is no pivot applying at this point just need to add BEFORE()
          val newBijection = additionalBijectionAppliedBefore

          // Adds the pivot (only if different bijections of course)
          val previousPivotBijectionIsDifferentFromNewBijection =
            currentTransformation.biggestLowerOrEqual(currentFromIncluded) match {
              case None                     => !newBijection.isIdentity
              case Some((_, existingPivot)) => !existingPivot.f.equals(newBijection)
            }
          if (previousPivotBijectionIsDifferentFromNewBijection) {
            currentTransformation = currentTransformation.insert(
              currentFromIncluded,
              new Pivot(currentFromIncluded, newBijection)
            )
          }

          currentFromIncluded = nextCurrentIncludedFrom
          currentIncludedFromAfterAdditionalF = additionalBijectionAppliedBefore(
            currentFromIncluded
          )
          positionOfPivotApplyingOnCurrentIncludedFromAfterAdditionalF = nextPivotExplorer
        case Some(pivotExplorer) =>
          // There is a pivot applying at this position, but until when ?
          val (nextCurrentIncludedFrom, nextPivotPosition) = {
            // Going backward
            if (isAdditionalBijectionNegativeSlope) {
              (
                // Moving backward &rarr; nextIncludedFrom = BEFORE.unApply(THIS.fromValue-1)
                // note : BEFORE.unApply(THIS.fromValue-1) is still greater than
                // 		BEFORE.unApply(currentIncludedFrom since it's flipping)
                additionalBijectionAppliedBefore.unApply(pivotExplorer.value.fromValue - 1),
                pivotExplorer.prev
              )
            } else {
              val nextPivotPosition = pivotExplorer.next
              (
                nextPivotPosition match {
                  case None => toIncluded + 1
                  case Some(position) =>
                    additionalBijectionAppliedBefore.unApply(position.value.fromValue)
                },
                nextPivotPosition
              )
            }
          }

          // THIS(BEFORE(x))
          val newBijection = pivotExplorer.value.f(additionalBijectionAppliedBefore)
          val previousPivotBijectionIsDifferentFromNewBijection =
            currentTransformation.biggestLowerOrEqual(currentFromIncluded) match {
              case None                     => !newBijection.isIdentity
              case Some((_, existingPivot)) => !existingPivot.f.equals(newBijection)
            }
          // Adds the pivot, if different from THIS
          if (previousPivotBijectionIsDifferentFromNewBijection) {
            currentTransformation = currentTransformation.insert(
              currentFromIncluded,
              new Pivot(currentFromIncluded, newBijection)
            )
          }

          currentFromIncluded = nextCurrentIncludedFrom
          currentIncludedFromAfterAdditionalF = additionalBijectionAppliedBefore(
            currentFromIncluded
          )
          positionOfPivotApplyingOnCurrentIncludedFromAfterAdditionalF = nextPivotPosition
      }
    }
    currentTransformation
  }

  /** Swaps two adjacent zones without flipping using the fastest order.
    *
    * @param startZone1Included
    *   Start position of the first zone
    * @param endZone1Included
    *   End position of the first zone
    * @param endZone2Included
    *   End position of the second zone
    * @return
    *   A new [[PiecewiseUnitaryAffineFunction]] with the two zones swapped
    */
  def swapAdjacentZonesShiftBest(
    startZone1Included: Int,
    endZone1Included: Int,
    endZone2Included: Int
  ): PiecewiseUnitaryAffineFunction = {
    val widthZone1 = endZone1Included - startZone1Included + 1
    val widthZone2 = endZone2Included - endZone1Included
    // TODO: the choice is based on the number of positions, it should be based on the number of segments instead (but this is probably the same very often)
    if (widthZone1 > widthZone2) {
      swapAdjacentZonesShiftFirst(
        startZone1Included,
        endZone1Included,
        endZone2Included,
        flipZone2 = false
      )
    } else {
      swapAdjacentZonesShiftSecond(
        startZone1Included,
        endZone1Included,
        endZone2Included,
        flipZone1 = false
      )
    }
  }

  /** Swaps two adjacent zones by shifting the first one after the second one.
    *
    * The zones are define like this :
    *   - first one : startZone1Included to endZone1Included
    *   - second one : endZone1Included + 1 to endZone2Included
    *
    * It is done in 5 steps see comments in code for more information.
    *
    * @param startZone1Included
    *   Starting position of the first zone
    * @param endZone1Included
    *   Ending position of the first zone
    * @param endZone2Included
    *   Ending position of the second zone
    * @param flipZone2
    *   If the second zone needs to be flipped
    * @return
    *   The updated [[PiecewiseUnitaryAffineFunction]]
    */
  def swapAdjacentZonesShiftFirst(
    startZone1Included: Int,
    endZone1Included: Int,
    endZone2Included: Int,
    flipZone2: Boolean
  ): PiecewiseUnitaryAffineFunction = {
    val widthZone2 = endZone2Included - endZone1Included
    val widthZone1 = endZone1Included - startZone1Included + 1

    /*
     1° Removes pivots for destination of zone1
     We need to do so otherwise we can't ensure hypothesis of [[RedBlackTreeMap.updateDelta]].
     We cannot change the keys (deltaKey) of the RBTree such that their new value become greater/lower
     than existing unchanged keys.
     */
    val transformWithTargetZone2Cleaned =
      removePivotsBetween(endZone1Included + 1, endZone2Included, transformation)

    // 2° Adds a pivot at zone1 start copying the previous one
    val transformReadyForShiftOfZone1 =
      addRedundantPivotAt(startZone1Included, transformWithTargetZone2Cleaned)

    // Meant to be composed with an existing UnitaryAffineFunction. (f2 or f3)°f
    val f2 = UnitaryAffineFunction(-widthZone2, flip = false)
    val f3 = UnitaryAffineFunction(widthZone2, flip = false)

    /*
     * 3° Shifts the first zone after the second one
     * Moving the pivots by zone2's width and composing the pivots of the first zone with f2 or f3.
     */
    val transformWithZone1Shifted =
      transformReadyForShiftOfZone1.updateDelta(
        startZone1Included,
        endZone1Included,
        widthZone2,
        (p: Pivot) => new Pivot(p.fromValue + widthZone2, if (p.f.flip) f3(p.f) else f2(p.f))
      )

    /*
     * 4° Next adds a new pivot (shifting backward) for the second zone which is now starting at startZone1
     * before applying pivots that were removed at step 1
     */
    val transformationWithUpdate2Done = myUpdateForCompositionBefore(
      startZone1Included,
      startZone1Included + widthZone2 - 1,
      UnitaryAffineFunction(
        if (flipZone2) endZone2Included + startZone1Included else widthZone1,
        flipZone2
      ),
      transformWithZone1Shifted
    )

    // 5° finally, cleans the potentially redundant pivots
    new PiecewiseUnitaryAffineFunction(
      deleteUnnecessaryPivotStartingJustAfter(
        startZone1Included - 1,
        deleteUnnecessaryPivotStartingJustAfter(
          startZone1Included + widthZone2 - 1,
          deleteUnnecessaryPivotStartingJustAfter(endZone2Included, transformationWithUpdate2Done)
        )
      )
    )
  }

  /** Swaps two adjacent zones by shifting the second one before the first one.
    *
    * The zones are define like this :
    *   - first one : startZone1Included to endZone1Included
    *   - second one : endZone1Included + 1 to endZone2Included
    *
    * It is done in 5 steps see comments in code for more information.
    *
    * @param startZone1Included
    *   Starting position of the first zone
    * @param endZone1Included
    *   Ending position of the first zone
    * @param endZone2Included
    *   Ending position of the second zone
    * @param flipZone1
    *   If the first zone needs to be flipped
    * @return
    *   The updated [[PiecewiseUnitaryAffineFunction]]
    */
  def swapAdjacentZonesShiftSecond(
    startZone1Included: Int,
    endZone1Included: Int,
    endZone2Included: Int,
    flipZone1: Boolean
  ): PiecewiseUnitaryAffineFunction = {

    val widthZone2 = endZone2Included - endZone1Included
    val widthZone1 = endZone1Included - startZone1Included + 1

    /*
         1° Removes pivots for destination of zone2
         We need to do so otherwise we can't ensure hypothesis of [[RedBlackTreeMap.updateDelta]].
         We cannot change the keys (deltaKey) of the RBTree such that their new value become greater/lower
         than existing unchanged keys.
     */
    val transformWithTargetZone1Cleaned =
      removePivotsBetween(startZone1Included, endZone1Included, transformation)

    // 2° Adds a pivot after zone 2 end copying the previous one to be able to "cut" the zone 2
    val transformReadyForShiftOfZone2 =
      addRedundantPivotAt(endZone2Included + 1, transformWithTargetZone1Cleaned)

    // Meant to be composed with an existing UnitaryAffineFunction. (f2 or f3)°f
    val f2 = UnitaryAffineFunction(widthZone1, flip = false)
    val f3 = UnitaryAffineFunction(-widthZone1, flip = false)

    /*
     * 3° Shifts the second zone before the first one
     * Moving the pivots by minus zone1's width and composing the pivots of the second zone with f2 or f3.
     */
    val transformWithZone2Shifted =
      transformReadyForShiftOfZone2.updateDelta(
        endZone1Included + 1,
        endZone2Included,
        -widthZone1,
        (p: Pivot) => new Pivot(p.fromValue - widthZone1, if (p.f.flip) f3(p.f) else f2(p.f))
      )

    /*
     *	4° Next adds a new pivot for the first zone which is now starting at startZone2 before
     *	applying the pivots that were removed at step 1
     */
    val transformationWithUpdate1Done = myUpdateForCompositionBefore(
      startZone1Included + widthZone2,
      endZone2Included,
      UnitaryAffineFunction(
        if (flipZone1) endZone1Included + startZone1Included + widthZone2 else -widthZone2,
        flipZone1
      ),
      transformWithZone2Shifted
    )

    // 5° finally, cleans the potentially redundant pivots
    new PiecewiseUnitaryAffineFunction(
      deleteUnnecessaryPivotStartingJustAfter(
        startZone1Included - 1,
        deleteUnnecessaryPivotStartingJustAfter(
          startZone1Included + widthZone2 - 1,
          deleteUnnecessaryPivotStartingJustAfter(endZone2Included, transformationWithUpdate1Done)
        )
      )
    )
  }

  /** Flips the pivots within interval defined by the two parameters.
    *
    * In term of bijection its an "in place flip".
    *
    * ex :
    *   - 0,1,2,3,4,5,6,7,8,9
    *   - swap adjacent zone shift first (0,2,5,true)
    *   - [5,4,3],[0,1,2],6,7,8,9
    *   - flipPivotsInInterval (3,6)
    *   - 5,4,3,[6,2,1,0],7,8,9
    *
    * @param startZoneIncluded
    *   Starting position of the interval
    * @param endZoneIncluded
    *   Ending position of the interval
    * @return
    *   An updated [[PiecewiseUnitaryAffineFunction]]
    */
  def flipPivotsInInterval(
    startZoneIncluded: Int,
    endZoneIncluded: Int
  ): PiecewiseUnitaryAffineFunction = {
    // 1° If needed, defines a redundant pivot at start to keep information of previous pivot
    val transformReadyForFlipOnLeft = addRedundantPivotAt(startZoneIncluded, this.transformation)
    // 2° If needed, defines a redundant pivot at end+1 to forward information of last pivot of the interval
    val transformReadyForFlip =
      addRedundantPivotAt(endZoneIncluded + 1, transformReadyForFlipOnLeft)

    // 3° Collects pivots within the interval
    val collectedPivotsForwardOrder: List[Pivot] =
      pivotsBetween(startZoneIncluded, endZoneIncluded, transformReadyForFlip)

    // 4° Flips the collected pivots
    val flippedPivots =
      flipListOfPivots(collectedPivotsForwardOrder, endZoneIncluded, endZoneIncluded)

    val flippedPivotsIterator = flippedPivots.iterator

    // 5° Update the ready for flip RBTree with the flipped pivots
    val updatedForwardFct = transformReadyForFlip.update(
      startZoneIncluded,
      transformReadyForFlip.biggestLowerOrEqual(endZoneIncluded).get._1,
      (_, _) => {
        val newPivot = flippedPivotsIterator.next()
        (newPivot.fromValue, newPivot)
      }
    )

    new PiecewiseUnitaryAffineFunction(
      deleteUnnecessaryPivotStartingJustAfter(
        startZoneIncluded - 1,
        deleteUnnecessaryPivotStartingJustAfter(endZoneIncluded, updatedForwardFct)
      )
    )
  }

  /** Flips a list of pivots.
    *
    * List(p1, p2, p3) &rarr; List(p3', p2', p1') The pivots keep their length but :
    *   - Their start position are shifted accordingly (ex : p3'.start = p1.start, p1'.start =
    *     p1.start+p3.length+p2.length)
    *   - Their [[UnitaryAffineFunction]] are mirrored and shifted (see mirrorPivot)
    *
    * Recursive process starting with flipping and moving the p1, then p2 ... pn
    *
    * @param pivotList
    *   The list of pivots to flip
    * @param newEndPositionOfNextPivotToFlip
    *   End position of the next pivot to flip
    * @param endOfLastPivotUntouched
    *   Old end position of last pivot to flip (used to compute width)
    * @param acc
    *   List of flipped pivots
    * @return
    */
  @tailrec
  private def flipListOfPivots(
    pivotList: List[Pivot],
    newEndPositionOfNextPivotToFlip: Int,
    endOfLastPivotUntouched: Int,
    acc: List[Pivot] = List.empty
  ): List[Pivot] = {
    pivotList match {
      case Nil => acc
      case p1 :: tail1 =>
        tail1 match {
          case Nil =>
            val width = endOfLastPivotUntouched - p1.fromValue + 1
            mirrorPivot(p1, width, newEndPositionOfNextPivotToFlip) :: acc
          case p2 :: _ =>
            val width = p2.fromValue - p1.fromValue
            flipListOfPivots(
              tail1,
              newEndPositionOfNextPivotToFlip - width,
              endOfLastPivotUntouched,
              mirrorPivot(p1, width, newEndPositionOfNextPivotToFlip) :: acc
            )
        }
    }
  }

  /** Creates the mirror [[Pivot]] of p, based on it's width and it's new endPosition.
    *
    * The pivot may have moved within the sequence. Therefore the new end and width are needed to
    * compute the new start of the pivot and it's new offset.
    * @param p
    *   the pivot to mirror
    * @param width
    *   the pivot's width
    * @param newEnd
    *   the pivot's new end
    * @return
    */
  private def mirrorPivot(p: Pivot, width: Int, newEnd: Int): Pivot = {
    val newFromValue = newEnd - width + 1
    val newFlip      = !p.f.flip
    val newOffset    = p.f(p.fromValue + newEnd)
    val newPivot     = new Pivot(newFromValue, new UnitaryAffineFunction(newOffset, newFlip))

    assert(p.f(p.fromValue) == newPivot.f(newEnd))
    assert(p.f(p.fromValue + width - 1) == newPivot.f(newPivot.fromValue))

    newPivot
  }

  /** Collects the pivots in the defined zone.
    *
    * Keeps the pivots order.
    * @param startPositionIncluded
    *   Start position of the zone.
    * @param endPositionIncluded
    *   End position of the zone.
    * @param transform
    *   The [[RedBlackTreeMap]] to consider.
    * @return
    */
  private def pivotsBetween(
    startPositionIncluded: Int,
    endPositionIncluded: Int,
    transform: RedBlackTreeMap[Pivot]
  ): List[Pivot] = {
    val lastPivotBeforeEndZonePosition = transform.biggestLowerOrEqual(endPositionIncluded)

    @tailrec
    def collectAllPivots(
      optExplorer: Option[RedBlackTreeMapExplorer[Pivot]],
      pivots: List[Pivot] = List.empty
    ): List[Pivot] = {
      optExplorer match {
        case Some(explorer) =>
          if (startPositionIncluded > explorer.key) pivots
          else collectAllPivots(explorer.prev, explorer.value :: pivots)
        case None => pivots
      }
    }

    val optExplorer =
      if (lastPivotBeforeEndZonePosition.nonEmpty)
        transform.positionOf(lastPivotBeforeEndZonePosition.get._1)
      else None
    collectAllPivots(optExplorer)
  }

  /** Removes unnecessary [[Pivot]] starting after the given position.
    *
    * There are two cases :
    *   - There is no [[Pivot]] starting before position and the pivot starting right after is the
    *     identity.
    *   - There is a [[Pivot]] starting before position and it's equal to the pivot starting right
    *     after the position.
    *
    * @param position
    *   The position after which we are checking the [[Pivot]]
    * @param updatedTransform
    *   The current [[RedBlackTreeMap]] holding the pivots
    * @return
    *   An updated [[RedBlackTreeMap]]
    */
  private def deleteUnnecessaryPivotStartingJustAfter(
    position: Int,
    updatedTransform: RedBlackTreeMap[Pivot]
  ): RedBlackTreeMap[Pivot] = {
    updatedTransform.get(position + 1) match {
      case None => updatedTransform
      case Some(pivotStartingJustAfterToIncluded) =>
        updatedTransform.biggestLowerOrEqual(position) match {
          case None =>
            if (pivotStartingJustAfterToIncluded.f.isIdentity)
              updatedTransform.remove(position + 1)
            else updatedTransform
          case Some((_, pivotApplyingAtToIncluded)) =>
            if (pivotStartingJustAfterToIncluded.f equals pivotApplyingAtToIncluded.f)
              updatedTransform.remove(position + 1)
            else updatedTransform
        }
    }
  }

  /** Tries to add a copy of the previous pivot at the given position.
    *
    * If no [[Pivot]] exists before atPosition, adds an identity [[Pivot]] Else if a [[Pivot]]
    * already exists at this position nothing is done. Else we copy the previous [[Pivot]]
    *
    * @param atPosition
    *   The position were the redundant [[Pivot]] is added
    * @param updatedTransform
    *   The current [[RedBlackTreeMap]] holding the pivots
    * @return
    *   An updated [[RedBlackTreeMap]]
    */
  private def addRedundantPivotAt(
    atPosition: Int,
    updatedTransform: RedBlackTreeMap[Pivot]
  ): RedBlackTreeMap[Pivot] = {
    updatedTransform.biggestLowerOrEqual(atPosition) match {
      case Some((key, pivot)) =>
        if (key == atPosition)
          updatedTransform // we do not add a redundant pivot because there is already a pivot here
        else updatedTransform.insert(atPosition, new Pivot(atPosition, pivot.f))
      case _ =>
        updatedTransform.insert(atPosition, new Pivot(atPosition, UnitaryAffineFunction.identity))
    }
  }

  /** Removes all [[Pivot]] between fromIncluded and toIncluded.
    *
    * if necessary adds a pivot at toIncluded+1 to ensure that values starting at toIncluded+1
    * onwards are not impacted
    * @param fromIncludedPosition
    *   Removes from this position
    * @param toIncludedPosition
    *   Removes to this position
    * @param transformToClean
    *   The [[RedBlackTreeMap]] to update.
    * @return
    *   The updated [[RedBlackTreeMap]]
    */
  private def removePivotsBetween(
    fromIncludedPosition: Int,
    toIncludedPosition: Int,
    transformToClean: RedBlackTreeMap[Pivot]
  ): RedBlackTreeMap[Pivot] = {

    val transformWithAddedPivot =
      addRedundantPivotAt(toIncludedPosition + 1, transformToClean)

    /*
     Here we could iterate using explorer instead but :
     - getting the biggestLowerOrEqual &rarr; O(Log(k))
     - getting the explorer after &rarr; O(Log(k))
     So except if we have more than 2 pivot to remove, it's better to use biggestLowerOrEqual each time.
     */
    @tailrec
    def removePivotRec(currentCleanedTransform: RedBlackTreeMap[Pivot]): RedBlackTreeMap[Pivot] = {
      currentCleanedTransform.biggestLowerOrEqual(toIncludedPosition) match {
        case Some((key, _)) if key >= fromIncludedPosition =>
          removePivotRec(currentCleanedTransform.remove(key))
        case _ => currentCleanedTransform
      }
    }
    removePivotRec(transformWithAddedPivot)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: PiecewiseUnitaryAffineFunction =>
        if (this.nbPivot != that.nbPivot) {
          return false
        }
        for ((thisPivot, thatPivot) <- this.pivots.zip(that.pivots)) {
          if (thisPivot.fromValue != thatPivot.fromValue) return false
          if (!thisPivot.f.equals(thatPivot.f)) return false
        }
        true
      case _ => false
    }
  }

  override def toString: String = {
    s"PiecewiseLinearFun(nbSegments:${transformation.size}, ${if (transformation.isEmpty) "identity"
      else s"segments:${transformation.values.mkString(",")}"})"
  }
}
