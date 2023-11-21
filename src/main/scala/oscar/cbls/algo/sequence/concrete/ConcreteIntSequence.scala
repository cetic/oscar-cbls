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

package oscar.cbls.algo.sequence.concrete

import oscar.cbls.algo.rb.{RedBlackTreeMap, RedBlackTreeMapExplorer}
import oscar.cbls.algo.sequence.affineFunction.{
  PiecewiseUnitaryAffineFunction,
  Pivot,
  UnitaryAffineFunction
}
import oscar.cbls.algo.sequence.stackedUpdate.{
  InsertedIntSequence,
  MovedIntSequence,
  RemovedIntSequence
}
import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer, Token}

import scala.annotation.tailrec

/** A concrete representation of an [[IntSequence]]
  *
  * The creation of a brand new Array of Int after each modification is very costly so the decision
  * has been made to create a more complex structure offering better performances. There are four
  * main concepts around the IntSequence :
  *   - The value : an [[Int]] value contained in the IntSequence
  *   - The internal position : The position of a specific value without considering any pivot
  *   - The external position : The position of a specific value considering every pivot
  *   - A pivot : A [[Pivot]] is an affine function used to link the external to the internal
  *     position
  *
  * So each time the route is modified, a new [[Pivot]] is created to adjust the new position of
  * each element. The internal positions don't change. Once in a while we commit / apply all pivots
  * and update all the internal position.
  *
  * @param internalPositionToValue
  *   A [[RedBlackTreeMap]] of [[Int]] associating an internal position to its value
  * @param valueToInternalPositions
  *   A [[RedBlackTreeMap]] of [[RedBlackTreeMap]] of [[Int]] associating a value to its internal
  *   position. The second RedBlackTree([Int]) is used as a Set[Int].
  * @param externalToInternalPosition
  *   A [[PiecewiseUnitaryAffineFunction]] containing all the necessary [[Pivot]] used to link an
  *   external position to its internal position
  * @param startFreeRangeForInternalPosition
  *   The next free internal insertion position.
  * @param token
  *   A small object used to id the current instance
  */
// TODO : HashSet[Int] is faster than RBT with big amount of values. We should maybe make a
// 	dedicated IntSequence for one value to one position problems and use HashSet here.
class ConcreteIntSequence(
  private[sequence] val internalPositionToValue: RedBlackTreeMap[Int],
  private[sequence] val valueToInternalPositions: RedBlackTreeMap[RedBlackTreeMap[Int]],
  private[sequence] val externalToInternalPosition: PiecewiseUnitaryAffineFunction,
  private[sequence] val startFreeRangeForInternalPosition: Int,
  token: Token = Token()
) extends IntSequence(token, 0) {

  /** During exploration we often use the getExplorerAt(...) method. This call is expensive (Log(n))
    * Furthermore, the same explorer may be called several times in a row. The idea here is to add a
    * tiny cache (this class could be created thousands of time during the search) to kee track of
    * the last used explorer and returning it as fast as possible. We use an sorted array where the
    * explorer at the end are those most recently used. Initiated with None values
    */
  private val cacheSize           = 10
  private var cacheFirstEmptySlot = cacheSize - 1
  private val intSequenceExplorerCache: Array[Option[IntSequenceExplorer]] =
    Array.fill(cacheSize)(None)

  // TODO: replace internalPositionToValue by an immutable Array, or an immutable array + a small RBTree + size

  override def descriptorString: String =
    "[" + this.iterator.toList.mkString(",") + "]_impl:concrete"

  override def check(): Unit = {
    // externalToInternalPosition.checkBijection()
    require(
      internalPositionToValue.content.sortBy(_._1) equals valueToInternalPositions.content
        .flatMap({ case (a, b) => b.keys.map(x => (x, a)) })
        .sortBy(_._1),
      "internalPositionToValue:" + internalPositionToValue.content.sortBy(
        _._1
      ) + " valueToInternalPositions:" + valueToInternalPositions.content
        .flatMap({ case (a, b) => b.keys.map(x => (x, a)) })
        .sortBy(_._1)
    )
  }

  override val size: Int = internalPositionToValue.size

  override def isEmpty: Boolean = internalPositionToValue.isEmpty

  override def nbOccurrence(value: Int): Int = valueToInternalPositions.get(value) match {
    case None    => 0
    case Some(p) => p.size
  }

  // Returns the largest value of the sequence
  def largestValue: Option[Int] = valueToInternalPositions.biggest match {
    case None         => None
    case Some((k, _)) => Some(k)
  }

  // Returns the smallest value of the sequence
  def smallestValue: Option[Int] = valueToInternalPositions.smallest match {
    case None         => None
    case Some((k, _)) => Some(k)
  }

  override def contains(value: Int): Boolean = valueToInternalPositions.contains(value)

  override def valueAtPosition(position: Int): Option[Int] = {
    val internalPosition: Int = externalToInternalPosition(position)
    internalPositionToValue.get(internalPosition)
  }

  override def positionsOfValue(value: Int): List[Int] = {
    valueToInternalPositions.get(value) match {
      case None => List.empty
      case Some(internalPositions) =>
        internalPositions.values.map(internalPos =>
          externalToInternalPosition.backward(internalPos)
        )
    }
  }

  override def explorerAtPosition(position: Int): Option[IntSequenceExplorer] = {

    // Moving the explorer at position to the end of the related array
    def putUsedExplorerAtBack(usedExplorerIndex: Int): Unit = {
      val explorer = intSequenceExplorerCache(usedExplorerIndex)
      recursiveMoveDown(usedExplorerIndex)
      intSequenceExplorerCache(cacheSize - 1) = explorer
    }

    // Recursively moves the value at index+1 to index and move to next index
    @tailrec
    def recursiveMoveDown(index: Int): Unit = {
      if (index < cacheSize - 1) {
        intSequenceExplorerCache(index) = intSequenceExplorerCache(index + 1)
        recursiveMoveDown(index + 1)
      }
    }

    // Shifts every explorer to the left and adds the new one at the end of the array
    def insertExplorerAtEnd(explorer: Option[IntSequenceExplorer]): Unit = {
      recursiveMoveDown(0)
      intSequenceExplorerCache(cacheSize - 1) = explorer
    }

    // Puts the explorer at the first free space (starting at the end).
    def insertExplorerAtFreeSpace(explorer: Option[IntSequenceExplorer]): Unit = {
      intSequenceExplorerCache(cacheFirstEmptySlot) = explorer
      cacheFirstEmptySlot -= 1
    }

    for (index <- cacheFirstEmptySlot + 1 until cacheSize) {
      intSequenceExplorerCache(index) match {
        case Some(explorer) if explorer.position == position =>
          putUsedExplorerAtBack(index)
          return intSequenceExplorerCache(cacheSize - 1)
        case _ =>
      }
    }

    val optExplorer = computeExplorerAtPosition(position)
    (optExplorer, cacheFirstEmptySlot < 0) match {
      case (None, _) => // do nothing
      case (Some(_), true) =>
        insertExplorerAtEnd(optExplorer) // The cache is full, we need to make space
      case (Some(_), false) =>
        insertExplorerAtFreeSpace(optExplorer) // The cache is not full, saving at free space
    }

    optExplorer
  }

  private def computeExplorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
    if (position >= this.size) None
    else if (position < 0) None
    else {
      val currentPivotPosition = externalToInternalPosition.pivotWithPositionApplyingTo(position)
      val (pivotAbovePosition: Option[RedBlackTreeMapExplorer[Pivot]], internalPosition) = {
        currentPivotPosition match {
          case None    => (externalToInternalPosition.firstPivotAndPosition, position)
          case Some(p) => (p.next, p.value.f(position))
        }
      }

      Some(
        new ConcreteIntSequenceExplorer(
          this,
          position,
          internalPositionToValue.positionOf(internalPosition).get,
          currentPivotPosition,
          pivotAbovePosition
        )()
      )
    }
  }

  override def explorerAtAnyOccurrence(value: Int): Option[IntSequenceExplorer] = {
    for (index <- cacheFirstEmptySlot + 1 until cacheSize) {
      intSequenceExplorerCache(index) match {
        case Some(explorer) if explorer.value == value => return intSequenceExplorerCache(index)
        case _                                         =>
      }
    }
    super.explorerAtAnyOccurrence(value)
  }

  override def positionOfAnyOccurrence(value: Int): Option[Int] = {
    for (index <- cacheFirstEmptySlot + 1 until cacheSize) {
      intSequenceExplorerCache(index) match {
        case Some(explorer) if explorer.value == value => return Some(explorer.position)
        case _                                         =>
      }
    }
    super.positionOfAnyOccurrence(value)
  }

  /** Adds the new internal position of the specified value
    * @param value
    *   The value
    * @param internalPosition
    *   The new internal position
    * @param valueToInternalPositions
    *   The [[RedBlackTreeMap]] mapping each value to it's internal position.
    * @return
    *   The [[RedBlackTreeMap]] with the new internal position of the value.
    */
  private def internalInsertToValueToInternalPositions(
    value: Int,
    internalPosition: Int,
    valueToInternalPositions: RedBlackTreeMap[RedBlackTreeMap[Int]]
  ): RedBlackTreeMap[RedBlackTreeMap[Int]] = {
    valueToInternalPositions.get(value) match {
      // No positions already registered, need to create a new RBT
      case None =>
        valueToInternalPositions.insert(
          value,
          RedBlackTreeMap(List((internalPosition, internalPosition)))
        )
      // Positions are already registered, just add the new position in the RBT
      case Some(l) =>
        valueToInternalPositions.insert(value, l.insert(internalPosition, internalPosition))
    }
  }

  /** Removes the internal position of the specified value's positions
    *
    * @param value
    *   The value
    * @param internalPosition
    *   The internal position to remove
    * @param valueToInternalPositions
    *   The [[RedBlackTreeMap]] mapping each value to it's internal position.
    * @return
    *   The [[RedBlackTreeMap]] without the internal position of the value.
    */
  private def internalRemoveFromValueToInternalPositions(
    value: Int,
    internalPosition: Int,
    valueToInternalPositions: RedBlackTreeMap[RedBlackTreeMap[Int]]
  ): RedBlackTreeMap[RedBlackTreeMap[Int]] = {
    valueToInternalPositions.get(value) match {
      // No positions registered for this value, nothing to do
      case None => valueToInternalPositions
      // Positions registered for this value, remove the specified position
      case Some(l) =>
        assert(l.contains(internalPosition))
        val newSet = l.remove(internalPosition)
        if (newSet.isEmpty) valueToInternalPositions.remove(value)
        else valueToInternalPositions.insert(value, newSet)
    }
  }

  override def insertAfterPosition(value: Int, insertAfterPositionExpl: Option[IntSequenceExplorer], fast: Boolean): IntSequence = {
    val insertAfterPos = IntSequenceExplorer.getPosOrElse(insertAfterPositionExpl,-1)
    require(
      insertAfterPos+1 <= size,
      "inserting past the end of the sequence (size:" + size + " inserting after pos:" + insertAfterPos + ")"
    )

    /** 1° Inserts the value at startFreeRangeForInternalPosition 2° Adds necessary pivot to match
      * it's position within the external position ex : Insertion of x at pos 8 and the sequence is
      * of size 13. First free internal space is 13 (the current size) Insert at 13 and add a pivot
      * : from 13 to 13 moving x 5 position earlier
      */

    if (fast) return new InsertedIntSequence(this, value, insertAfterPositionExpl, 1)

    // insert into red blacks
    val newInternalPositionToValue =
      internalPositionToValue.insert(startFreeRangeForInternalPosition, value)
    val newValueToInternalPosition = internalInsertToValueToInternalPositions(
      value,
      startFreeRangeForInternalPosition,
      valueToInternalPositions
    )

    // If pos == size, we add it at the end of the sequence resulting on an identity pivot => no need
    val newExternalToInternalPosition = if (insertAfterPos+1 != size) {
      // inserting somewhere within the sequence, need to shift upper part
      val tmp = externalToInternalPosition.swapAdjacentZonesShiftFirst(insertAfterPos+1, size - 1, size, false)

      assert(
        tmp equals externalToInternalPosition.updatesForCompositionBefore(
          List(
            (insertAfterPos + 2, size, UnitaryAffineFunction(-1, false)),
            (insertAfterPos+1, insertAfterPos+1, UnitaryAffineFunction(startFreeRangeForInternalPosition - (insertAfterPos+1), false))
          )
        )
      )
      tmp
    } else { externalToInternalPosition }

    new ConcreteIntSequence(
      newInternalPositionToValue,
      newValueToInternalPosition,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition + 1
    )
  }

  override def delete(removePosAsExplorer: IntSequenceExplorer, fast: Boolean): IntSequence = {
    val pos = removePosAsExplorer.position
    require(
      pos >= 0 && pos < size,
      s"Remove position must be in [0, sizeOfSequence=$size [ got $pos"
    )

    if (fast) return new RemovedIntSequence(this, removePosAsExplorer, 1)

    // 1° Gets the corresponding values + some contextual information
    val internalPosition        = externalToInternalPosition(pos)
    val value                   = internalPositionToValue.get(internalPosition).get
    val largestInternalPosition = startFreeRangeForInternalPosition - 1
    val valueAtLargestInternalPosition: Int =
      internalPositionToValue.get(largestInternalPosition).get
    val deleteIsAtLargestInternalPosition = internalPosition == largestInternalPosition

    // 2° Removes internalPos to value mapping by pushing the value at the end of the sequence.
    val newInternalPositionToValue = if (deleteIsAtLargestInternalPosition) {
      internalPositionToValue.remove(largestInternalPosition)
    } else {
      internalPositionToValue
        .insert(internalPosition, valueAtLargestInternalPosition)
        .remove(largestInternalPosition)
    }

    // 3° Removes position from know internal positions for this value
    val newValueToInternalPositions = if (deleteIsAtLargestInternalPosition) {
      internalRemoveFromValueToInternalPositions(value, internalPosition, valueToInternalPositions)
    } else {
      internalInsertToValueToInternalPositions(
        valueAtLargestInternalPosition,
        internalPosition,
        internalRemoveFromValueToInternalPositions(
          valueAtLargestInternalPosition,
          largestInternalPosition,
          internalRemoveFromValueToInternalPositions(
            value,
            internalPosition,
            valueToInternalPositions
          )
        )
      )
    }

    // external position of the internal position that has been removed
    val externalPositionAssociatedToLargestInternalPosition =
      externalToInternalPosition.backward(largestInternalPosition)

    // 4° Updates the unitary affine function knowing the move and remove
    // TODO: this is overly complex and probably very slow
    val newExternalToInternalPosition = externalToInternalPosition
      .updatesForCompositionBefore(
        List(
          (
            externalPositionAssociatedToLargestInternalPosition,
            externalPositionAssociatedToLargestInternalPosition,
            UnitaryAffineFunction(pos - externalPositionAssociatedToLargestInternalPosition, false)
          ),
          (
            pos,
            pos,
            UnitaryAffineFunction(externalPositionAssociatedToLargestInternalPosition - pos, false)
          )
        )
      )
      .updatesForCompositionBefore(
        List(
          (pos, size - 2, UnitaryAffineFunction(1, false)),
          (size - 1, size - 1, UnitaryAffineFunction(pos - size + 1, false))
        )
      )

    /** TODO : Check this when the tests are added :
      *
      * externalToInternalPosition.swapAdjacentZonesShiftFirst(pos, pos, size-1, false).
      * updateForCompositionBefore(size-1,size-1,UnitaryAffineFunction.identity)
      *
      * basically switching the removed node position, with the zone after it (until end of
      * sequence) and then applying identity to last node
      */

    new ConcreteIntSequence(
      newInternalPositionToValue,
      newValueToInternalPositions,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition - 1
    )
  }

  override def moveAfter(
                          fromIncludedExpl: IntSequenceExplorer,
                          toIncludedExpl: IntSequenceExplorer,
                          moveAfterExpl: Option[IntSequenceExplorer],
                          flip: Boolean,
                          fast: Boolean
  ): IntSequence = {
    val fromIncludedPos = fromIncludedExpl.position
    val toIncludedPos = toIncludedExpl.position
    val moveAfterPos = if(moveAfterExpl.nonEmpty) moveAfterExpl.get.position else -1
    require(
      fromIncludedPos >= 0 && fromIncludedPos < size,
      s"StartPositionIncluded should be in [0,sizeOfSequence=$size[ got $fromIncludedPos"
    )
    require(
      toIncludedPos >= 0 && toIncludedPos < size,
      s"EndPositionIncluded should be in [0,sizeOfSequence=$size[ got $toIncludedPos"
    )
    require(
      moveAfterPos >= -1 && moveAfterPos < size,
      s"MoveAfterPosition should be in [-1,sizeOfSequence=$size[ got $moveAfterPos"
    )

    require(
      moveAfterPos < fromIncludedPos || moveAfterPos > toIncludedPos,
      s"MoveAfterPosition cannot be between startPositionIncluded and endPositionIncluded. " +
        s"Got $moveAfterPos (move), $fromIncludedPos (start) $toIncludedPos (end)"
    )
    require(
      fromIncludedPos <= toIncludedPos,
      s"StartPositionIncluded must be <= endPositionIncluded. Got $fromIncludedPos <= $toIncludedPos"
    )

    if (fast)
      return new MovedIntSequence(
        this,
        fromIncludedExpl,
        toIncludedExpl,
        moveAfterExpl,
        flip,
        1
      )

    val newExternalToInternalPosition =
      if (moveAfterPos + 1 == fromIncludedPos) {
        if (flip) {
          // The segment is flipping on itself
          externalToInternalPosition.flipPivotsInInterval(
            fromIncludedPos,
            toIncludedPos
          )

        } else {
          // The segment is not moving nor flipping
          externalToInternalPosition
        }
      } else {
        if (moveAfterPos > fromIncludedPos) {
          val tempNewExternalToInternalPosition = if (!flip) {
            // The segment is moving upward without flipping
            externalToInternalPosition.swapAdjacentZonesShiftBest(
              fromIncludedPos,
              toIncludedPos,
              moveAfterPos
            )
          } else {
            // The segment is moving upward and flipping
            externalToInternalPosition.swapAdjacentZonesShiftSecond(
              fromIncludedPos,
              toIncludedPos,
              moveAfterPos,
              true
            )
          }

          // TODO : Could we remove these ?
          assert(
            tempNewExternalToInternalPosition equals externalToInternalPosition
              .updatesForCompositionBefore(
                List(
                  (
                    fromIncludedPos,
                    moveAfterPos + fromIncludedPos - toIncludedPos - 1,
                    UnitaryAffineFunction(toIncludedPos + 1 - fromIncludedPos, false)
                  ),
                  (
                    fromIncludedPos + moveAfterPos - toIncludedPos,
                    moveAfterPos,
                    UnitaryAffineFunction(
                      if (flip) fromIncludedPos + moveAfterPos
                      else toIncludedPos - moveAfterPos,
                      flip
                    )
                  )
                )
              )
          )
          tempNewExternalToInternalPosition
        } else {
          // The segment is moving downward without flipping
          val tempNewExternalToInternalPosition = if (!flip) {
            externalToInternalPosition.swapAdjacentZonesShiftBest(
              moveAfterPos + 1,
              fromIncludedPos - 1,
              toIncludedPos
            )
          } else {
            // The segment is moving downward without and flipping
            externalToInternalPosition.swapAdjacentZonesShiftFirst(
              moveAfterPos + 1,
              fromIncludedPos - 1,
              toIncludedPos,
              true
            )
          }

          assert(
            externalToInternalPosition.updatesForCompositionBefore(
              List(
                (
                  moveAfterPos + 1,
                  moveAfterPos + toIncludedPos - fromIncludedPos + 1,
                  UnitaryAffineFunction(
                    if (flip) toIncludedPos + moveAfterPos + 1
                    else fromIncludedPos - moveAfterPos - 1,
                    flip
                  )
                ),
                (
                  moveAfterPos + toIncludedPos - fromIncludedPos + 2,
                  toIncludedPos,
                  UnitaryAffineFunction(fromIncludedPos - toIncludedPos - 1, false)
                )
              )
            ) equals tempNewExternalToInternalPosition
          )
          tempNewExternalToInternalPosition
        }
      }

    new ConcreteIntSequence(
      internalPositionToValue,
      valueToInternalPositions,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition
    )
  }

  override def regularizeToMaxPivot(
    maxPivotPerValuePercent: Int,
    targetToken: Token = this.token
  ): ConcreteIntSequence = {
    if (this.externalToInternalPosition.nbPivot * 100 > maxPivotPerValuePercent * this.size) {
      regularize()
    } else {
      if (targetToken != this.token) {
        new ConcreteIntSequence(
          internalPositionToValue,
          valueToInternalPositions,
          externalToInternalPosition,
          size,
          targetToken
        )
      } else this
    }
  }

  override def regularize(targetToken: Token = this.token): ConcreteIntSequence = {
    var explorerOpt                                    = this.explorerAtPosition(0)
    val newInternalPositionToValues: Array[(Int, Int)] = Array.ofDim[(Int, Int)](this.size)
    val oldInternalPosToNewInternalPos: Array[Int]     = Array.ofDim[Int](this.size)

    // TODO : Seems way too complicated
    // 	I think that we could remove the oldInternalPosToNewInternalPos value
    // 	Should check that when testing is available
    while (
      explorerOpt match {
        case None => false
        case Some(explorer) =>
          newInternalPositionToValues(explorer.position) = (explorer.position, explorer.value)
          oldInternalPosToNewInternalPos(
            explorer.asInstanceOf[ConcreteIntSequenceExplorer].internalPos
          ) = explorer.position
          explorerOpt = explorer.next
          true
      }
    ) {}

    new ConcreteIntSequence(
      RedBlackTreeMap.makeFromSortedArray(newInternalPositionToValues),
      valueToInternalPositions.updateAll(
        0,
        oldInternalPositions => {
          val newPositions = oldInternalPositions.keys.map(oldInt => {
            val newInternalPosition: Int = oldInternalPosToNewInternalPos(oldInt)
            (newInternalPosition: Int, newInternalPosition)
          })
          RedBlackTreeMap[Int](newPositions)
        }
      ),
      PiecewiseUnitaryAffineFunction.identity,
      newInternalPositionToValues.length,
      targetToken
    )
  }

  override def commitPendingMoves: IntSequence = this

  override def unorderedContentNoDuplicate: List[Int] = valueToInternalPositions.keys

  override def unorderedContentNoDuplicateWithNBOccurrences: List[(Int, Int)] =
    valueToInternalPositions.content.map({ case ((value, positions)) => ((value, positions.size)) })
}
