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

class ConcreteIntSequence(
  private[sequence] val internalPositionToValue: RedBlackTreeMap[Int],
  private[sequence] val valueToInternalPositions: RedBlackTreeMap[RedBlackTreeMap[Int]],
  private[sequence] val externalToInternalPosition: PiecewiseUnitaryAffineFunction,
  private[sequence] val startFreeRangeForInternalPosition: Int,
  token: Token = Token()
) extends IntSequence(token, 0) {

  private val cacheSize                                            = 10
  private var noneExplorerPosition: Int                            = Int.MinValue
  private val intSequenceExplorerCache: Array[IntSequenceExplorer] = Array.ofDim(cacheSize)

  // TODO: replace internalPositionToValue by an immutable Array, or an immutable array + a small RBTree + size

  def bij = externalToInternalPosition
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

  def largestValue: Option[Int] = valueToInternalPositions.biggest match {
    case None         => None
    case Some((k, _)) => Some(k)
  }

  def smallestValue: Option[Int] = valueToInternalPositions.smallest match {
    case None         => None
    case Some((k, _)) => Some(k)
  }

  def contains(value: Int): Boolean = valueToInternalPositions.contains(value)

  def valueAtPosition(position: Int): Option[Int] = {
    val internalPosition: Int = externalToInternalPosition(position)
    internalPositionToValue.get(internalPosition)
  }

  override def positionsOfValue(value: Int): List[Int] = {
    valueToInternalPositions.get(value) match {
      case None => null
      case Some(internalPositions) =>
        var toReturn: List[Int] = null
        var toDigest: List[Int] = internalPositions.values
        while (toDigest.nonEmpty) {
          toReturn = List(externalToInternalPosition.backward(toDigest.head)) ::: toReturn
          toDigest = toDigest.tail
        }
        toReturn
    }
  }

  def explorerAtPosition(position: Int): Option[IntSequenceExplorer] = {

    def putUsedExplorerAtBack(usedExplorerIndex: Int): Unit = {
      // Moving the explorer and position to the end of the related array
      var i        = usedExplorerIndex
      val explorer = intSequenceExplorerCache(i)
      while (i < cacheSize - 1) {
        intSequenceExplorerCache(i) = intSequenceExplorerCache(i + 1)
        i += 1
      }
      intSequenceExplorerCache(cacheSize - 1) = explorer
    }

    def insertExplorerAtEnd(explorer: IntSequenceExplorer): Unit = {
      var i = 0
      while (i < cacheSize - 1) {
        intSequenceExplorerCache(i) = intSequenceExplorerCache(i + 1)
        i += 1
      }
      intSequenceExplorerCache(i) = explorer
    }

    def insertExplorerAtFreeSpace(explorer: IntSequenceExplorer): Unit = {
      var i = cacheSize - 1
      while (i > 0 && intSequenceExplorerCache(i) != null)
        i -= 1
      require(
        intSequenceExplorerCache(i) == null,
        "That position should be empty got " + intSequenceExplorerCache(i) +
          "\nCurrent value : " + intSequenceExplorerCache.toList
      )
      intSequenceExplorerCache(i) = explorer
    }

    if (noneExplorerPosition == position) return None
    var index = 0
    while (index < cacheSize) {
      if (
        intSequenceExplorerCache(index) != null &&
        intSequenceExplorerCache(index).position == position
      ) {
        putUsedExplorerAtBack(index)
        return Some(intSequenceExplorerCache(cacheSize - 1))
      }
      index += 1
    }

    val optExplorer = computeExplorerAtPosition(position)
    optExplorer match {
      case None =>
        noneExplorerPosition = position

      case Some(explorer) =>
        if (intSequenceExplorerCache(0) != null)
          insertExplorerAtEnd(explorer) // The cache is full, we need to make space
        else
          insertExplorerAtFreeSpace(explorer) // The cache is not full, saving at free space
    }

    optExplorer
  }

  private def computeExplorerAtPosition(position: Int): Option[IntSequenceExplorer] = {
    if (position >= this.size) None
    else {
      val currentPivotPosition = externalToInternalPosition.pivotWithPositionApplyingTo(position)
      val (pivotAbovePosition: Option[RedBlackTreeMapExplorer[Pivot]], internalPosition) =
        currentPivotPosition match {
          case None    => (externalToInternalPosition.firstPivotAndPosition, position)
          case Some(p) => (p.next, p.value.f(position))
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
    var index = 0
    while (index < cacheSize) {
      if (
        intSequenceExplorerCache(index) != null &&
        intSequenceExplorerCache(index).value == value
      )
        return Some(intSequenceExplorerCache(index))
      else
        index += 1
    }
    super.explorerAtAnyOccurrence(value)
  }

  override def positionOfAnyOccurrence(value: Int): Option[Int] = {
    var index = 0
    while (index < cacheSize) {
      if (
        intSequenceExplorerCache(index) != null &&
        intSequenceExplorerCache(index).value == value
      )
        return Some(intSequenceExplorerCache(index).position)
      else
        index += 1
    }
    super.positionOfAnyOccurrence(value)
  }

  private def internalInsertToValueToInternalPositions(
    value: Int,
    internalPosition: Int,
    valueToInternalPositions: RedBlackTreeMap[RedBlackTreeMap[Int]]
  ): RedBlackTreeMap[RedBlackTreeMap[Int]] = {
    valueToInternalPositions.get(value) match {
      case None =>
        valueToInternalPositions.insert(
          value,
          RedBlackTreeMap(List((internalPosition, internalPosition)))
        )
      case Some(l) =>
        valueToInternalPositions.insert(value, l.insert(internalPosition, internalPosition))
    }
  }

  private def internalRemoveFromValueToInternalPositions(
    value: Int,
    internalPosition: Int,
    valueToInternalPositions: RedBlackTreeMap[RedBlackTreeMap[Int]]
  ): RedBlackTreeMap[RedBlackTreeMap[Int]] = {
    valueToInternalPositions.get(value) match {
      case None => valueToInternalPositions
      case Some(l) =>
        assert(l.contains(internalPosition))
        val newSet = l.remove(internalPosition)
        if (newSet.isEmpty) valueToInternalPositions.remove(value)
        else valueToInternalPositions.insert(value, newSet)
    }
  }

  def insertAtPosition(value: Int, pos: Int, fast: Boolean): IntSequence = {

    // println(this + ".insertAtPosition(value:" + value + " pos:" + pos + ")")
    require(
      pos <= size,
      "inserting past the end of the sequence (size:" + size + " pos:" + pos + ")"
    )

    if (fast) return new InsertedIntSequence(this, value, pos, 1)

    // insert into red blacks
    val newInternalPositionToValue =
      internalPositionToValue.insert(startFreeRangeForInternalPosition, value)
    val newValueToInternalPosition = internalInsertToValueToInternalPositions(
      value,
      startFreeRangeForInternalPosition,
      valueToInternalPositions
    )

    // move sequence after position, one upward
    // move inserted point at its position
    val oldExternalPosRelatedToFreeInternalPos =
      externalToInternalPosition.backward(startFreeRangeForInternalPosition)

    val newExternalToInternalPosition = if (pos == size) {
      // inserting at end of the sequence
      externalToInternalPosition.updateForCompositionBefore(
        size,
        size,
        UnitaryAffineFunction(oldExternalPosRelatedToFreeInternalPos - pos, false)
      )
      // TODO: this might be always identity, actually, so useless!
    } else {
      // inserting somewhere within the sequence, need to shift upper part

      val tmp = externalToInternalPosition.swapAdjacentZonesShiftFirst(pos, size - 1, size, false)

      assert(
        tmp equals externalToInternalPosition.updatesForCompositionBefore(
          List(
            (pos + 1, size, UnitaryAffineFunction(-1, false)),
            (pos, pos, UnitaryAffineFunction(oldExternalPosRelatedToFreeInternalPos - pos, false))
          )
        )
      )
      tmp
    }

    new ConcreteIntSequence(
      newInternalPositionToValue,
      newValueToInternalPosition,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition + 1
    )
  }

  def delete(pos: Int, fast: Boolean): IntSequence = {
    // println(this + ".delete(pos:" + pos + ")")
    require(pos < size, s"deleting past the end of the sequence (size:$size pos:$pos)")
    require(pos >= 0, s"deleting at negative pos:$pos")

    if (fast) return new RemovedIntSequence(this, pos, 1)

    val internalPosition        = externalToInternalPosition(pos)
    val value                   = internalPositionToValue.get(internalPosition).head
    val largestInternalPosition = startFreeRangeForInternalPosition - 1

    val valueAtLargestInternalPosition: Int =
      internalPositionToValue.get(largestInternalPosition).head

    val deleteIsAtLargestInternalPosition = internalPosition == largestInternalPosition

    val newInternalPositionToValue = if (deleteIsAtLargestInternalPosition) {
      internalPositionToValue.remove(largestInternalPosition)
    } else {
      internalPositionToValue
        .insert(internalPosition, valueAtLargestInternalPosition)
        .remove(largestInternalPosition)
    }

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

    // now, update the fct knowing the move and remove
    val externalPositionAssociatedToLargestInternalPosition =
      externalToInternalPosition.backward(largestInternalPosition)

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

    new ConcreteIntSequence(
      newInternalPositionToValue,
      newValueToInternalPositions,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition - 1
    )
  }

  def moveAfter(
    startPositionIncluded: Int,
    endPositionIncluded: Int,
    moveAfterPosition: Int,
    flip: Boolean,
    fast: Boolean
  ): IntSequence = {
    // println(this + ".moveAfter(startPositionIncluded:" + startPositionIncluded + " endPositionIncluded:" + endPositionIncluded + " moveAfterPosition:" + moveAfterPosition + " flip:" + flip + ")")
    require(
      startPositionIncluded >= 0 && startPositionIncluded < size,
      "startPositionIncluded should be in [0,size[ in UniqueIntSequence.moveAfter"
    )
    require(
      endPositionIncluded >= 0 && endPositionIncluded < size,
      s"endPositionIncluded(=$endPositionIncluded) should be in [0,size(=$size)[ in UniqueIntSequence.moveAfter"
    )
    require(
      moveAfterPosition >= -1 && moveAfterPosition < size,
      s"moveAfterPosition=$moveAfterPosition should be in [-1,size=$size[ in UniqueIntSequence.moveAfter"
    )

    require(
      moveAfterPosition < startPositionIncluded || moveAfterPosition > endPositionIncluded,
      s"moveAfterPosition=$moveAfterPosition cannot be between startPositionIncluded=$startPositionIncluded and endPositionIncluded=$endPositionIncluded"
    )
    require(
      startPositionIncluded <= endPositionIncluded,
      s"startPositionIncluded=$startPositionIncluded should be <= endPositionIncluded=$endPositionIncluded"
    )

    if (fast)
      return new MovedIntSequence(
        this,
        startPositionIncluded,
        endPositionIncluded,
        moveAfterPosition,
        flip,
        1
      )

    if (moveAfterPosition + 1 == startPositionIncluded) {
      // not moving
      if (flip) {
        // just flipping
        val newExternalToInternalPosition = externalToInternalPosition.flipPivotsInInterval(
          startPositionIncluded,
          endPositionIncluded
        )

        new ConcreteIntSequence(
          internalPositionToValue,
          valueToInternalPositions,
          newExternalToInternalPosition,
          startFreeRangeForInternalPosition
        )

      } else {
        this // nop
      }
    } else {
      if (moveAfterPosition > startPositionIncluded) {
        // move upwards
        val newExternalToInternalPosition = if (!flip) {
          externalToInternalPosition.swapAdjacentZonesShiftBest(
            startPositionIncluded,
            endPositionIncluded,
            moveAfterPosition
          )

        } else {

          val tmp = externalToInternalPosition.swapAdjacentZonesShiftSecond(
            startPositionIncluded,
            endPositionIncluded,
            moveAfterPosition,
            true
          )

          assert(
            tmp equals externalToInternalPosition.updatesForCompositionBefore(
              List(
                (
                  startPositionIncluded,
                  moveAfterPosition + startPositionIncluded - endPositionIncluded - 1,
                  UnitaryAffineFunction(endPositionIncluded + 1 - startPositionIncluded, false)
                ),
                (
                  startPositionIncluded + moveAfterPosition - endPositionIncluded,
                  moveAfterPosition,
                  UnitaryAffineFunction(
                    if (flip) startPositionIncluded + moveAfterPosition
                    else endPositionIncluded - moveAfterPosition,
                    flip
                  )
                )
              )
            )
          )

          tmp
        }

        assert(
          newExternalToInternalPosition equals externalToInternalPosition
            .updatesForCompositionBefore(
              List(
                (
                  startPositionIncluded,
                  moveAfterPosition + startPositionIncluded - endPositionIncluded - 1,
                  UnitaryAffineFunction(endPositionIncluded + 1 - startPositionIncluded, false)
                ),
                (
                  startPositionIncluded + moveAfterPosition - endPositionIncluded,
                  moveAfterPosition,
                  UnitaryAffineFunction(
                    if (flip) startPositionIncluded + moveAfterPosition
                    else endPositionIncluded - moveAfterPosition,
                    flip
                  )
                )
              )
            )
        )

        new ConcreteIntSequence(
          internalPositionToValue,
          valueToInternalPositions,
          newExternalToInternalPosition,
          startFreeRangeForInternalPosition
        )

      } else {
        // move downwards
        val newExternalToInternalPosition = if (!flip) {
          externalToInternalPosition.swapAdjacentZonesShiftBest(
            moveAfterPosition + 1,
            startPositionIncluded - 1,
            endPositionIncluded
          )
        } else {
          externalToInternalPosition.swapAdjacentZonesShiftFirst(
            moveAfterPosition + 1,
            startPositionIncluded - 1,
            endPositionIncluded,
            true
          )
        }

        assert(
          externalToInternalPosition.updatesForCompositionBefore(
            List(
              (
                moveAfterPosition + 1,
                moveAfterPosition + endPositionIncluded - startPositionIncluded + 1,
                UnitaryAffineFunction(
                  if (flip) endPositionIncluded + moveAfterPosition + 1
                  else startPositionIncluded - moveAfterPosition - 1,
                  flip
                )
              ),
              (
                moveAfterPosition + endPositionIncluded - startPositionIncluded + 2,
                endPositionIncluded,
                UnitaryAffineFunction(startPositionIncluded - endPositionIncluded - 1, false)
              )
            )
          ) equals newExternalToInternalPosition
        )

        new ConcreteIntSequence(
          internalPositionToValue,
          valueToInternalPositions,
          newExternalToInternalPosition,
          startFreeRangeForInternalPosition
        )
      }
    }
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

  def regularize(targetToken: Token = this.token): ConcreteIntSequence = {
    var explorerOpt                                    = this.explorerAtPosition(0)
    val newInternalPositionToValues: Array[(Int, Int)] = Array.ofDim[(Int, Int)](this.size)
    val oldInternalPosToNewInternalPos: Array[Int]     = Array.ofDim[Int](this.size)

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
