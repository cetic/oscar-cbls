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

package oscar.cbls.algo.sequence

import oscar.cbls.algo.rb.{RedBlackTreeMap, RedBlackTreeMapExplorer}
import oscar.cbls.algo.sequence.affineFunction.PiecewiseUnitaryAffineFunction
import oscar.cbls.algo.sequence.concrete.ConcreteIntSequence
import oscar.cbls.algo.sequence.stackedUpdate._

import scala.collection.immutable.SortedSet
import scala.language.implicitConversions

// Companion object of [[IntSequence]]
object IntSequence {

  /** Creates an [[ConcreteIntSequence]] from a sorted list of integers.
    *
    * @param values
    *   The sorted integers as a [[Iterable]] of [[Int]]
    * @return
    *   A [[ConcreteIntSequence]] with the sorted integers
    */
  def apply(values: Iterable[Int]): IntSequence = {
    val valuesArray     = values.toArray
    val forwardRedBlack = RedBlackTreeMap.makeFromSortedContinuousArray(values.toArray)
    val backwardRedBlack: RedBlackTreeMap[RedBlackTreeMap[Int]] =
      aggregatePosOnValToInternalPosFrom(valuesArray)

    new ConcreteIntSequence(
      forwardRedBlack,
      backwardRedBlack,
      PiecewiseUnitaryAffineFunction.identity,
      valuesArray.length
    )
  }

  /** Creates an IntSequence from a sorted list of integers.
    *
    * @param values
    *   The sorted integers as a [[Iterable]] of [[Int]]
    * @return
    *   An IntSequence with the sorted integers
    */
  private def aggregatePosOnValToInternalPosFrom(
    values: Array[Int]
  ): RedBlackTreeMap[RedBlackTreeMap[Int]] = {
    var valToPoses = RedBlackTreeMap.empty[RedBlackTreeMap[Int]]
    for (pos <- values.indices) {
      val value       = values(pos)
      val existingPos = valToPoses.getOrElse(value, RedBlackTreeMap.empty[Int])
      valToPoses = valToPoses.insert(value, existingPos.insert(pos, pos))
    }
    valToPoses
  }

  def empty(): IntSequence = new ConcreteIntSequence(
    RedBlackTreeMap.empty[Int],
    RedBlackTreeMap.empty[RedBlackTreeMap[Int]],
    PiecewiseUnitaryAffineFunction.identity,
    0
  )

  implicit def toIterable(seq: IntSequence): IterableIntSequence = new IterableIntSequence(seq)
}

// TODO try to remove this, looks like an unnecessary quick fix solution
class Token()

object Token {
  def apply(): Token = new Token()
}

abstract class IntSequence(protected[cbls] val token: Token = Token(), val depth: Int) {

  def size: Int

  def isEmpty: Boolean = size == 0

  def nonEmpty: Boolean = !isEmpty

  def iterator: Iterator[Int] = new IntSequenceIterator(this.explorerAtPosition(0))

  def iterateFromAnyOccurrenceOfValue(value: Int): Iterator[Int] = new IntSequenceIterator(
    this.explorerAtAnyOccurrence(value)
  )

  def iterable: Iterable[Int] = new IterableIntSequence(this)

  // Returns the number of occurrence of the specified value
  def nbOccurrence(value: Int): Int

  // Returns the content of the sequence without duplicates
  def unorderedContentNoDuplicate: List[Int]

  // Returns the content of the sequence without duplicates associated with their number of occurrence
  def unorderedContentNoDuplicateWithNBOccurrences: List[(Int, Int)]

  /** Returns the optional value at the specified position
    *
    * @param position
    *   The position of the value
    * @return
    *   [[None]] if the position is out of the [[IntSequence]] size. Else [[Some]] value as [[Int]]
    */
  def valueAtPosition(position: Int): Option[Int]

  // Returns all the positions of the specified value
  def positionsOfValue(value: Int): List[Int]

  // Checks whether or not the IntSequence contains the specified value
  def contains(value: Int): Boolean

  /** Returns an optional [[IntSequenceExplorer]] at the specified position
    *
    * @param position
    *   The position of the value
    * @return
    *   [[None]] if the position is out of the [[IntSequence]] size. Else [[Some]]
    *   [[IntSequenceExplorer]]
    */
  def explorerAtPosition(position: Int): Option[IntSequenceExplorer]

  /** Applies a function to each element of the sequence
    *
    * @param fun
    *   The [[Int]] to [[Int]] function that will be applied to each element.
    * @return
    *   A new [[IntSequence]] with modified values.
    */
  def map(fun: Int => Int): IntSequence = {
    val l: List[Int] = this.iterator.toList
    val l2           = l.map(fun)
    IntSequence.apply(l2)
  }

  /** Returns the values between the specified positions (included)
    *
    * @param fromPositionIncluded
    *   Starting position (included) as an [[Int]]
    * @param toPositionIncluded
    *   Ending position (included) as an [[Int]]
    * @return
    *   The values between the specified positions as a [[List]] of [[Int]]
    */
  def valuesBetweenPositions(fromPositionIncluded: Int, toPositionIncluded: Int): List[Int] = {
    var toReturn: List[Int] = List.empty
    var e                   = explorerAtPosition(fromPositionIncluded)
    while (
      e match {
        case None => false
        case Some(explorer) =>
          if (explorer.position <= toPositionIncluded) {
            toReturn = List(explorer.value) ::: toReturn
            e = explorer.next
            true
          } else false
      }
    ) {}
    toReturn
  }

  /** Returns an [[IntSequenceExplorer]] at the first occurrence of the specified value
    *
    * @param value
    *   The value
    * @return
    *   An [[IntSequenceExplorer]]
    */
  def explorerAtFirstOccurrence(value: Int): Option[IntSequenceExplorer] = {
    positionOfFirstOccurrence(value: Int) match {
      case None    => None
      case Some(x) => explorerAtPosition(x)
    }
  }

  /** Returns an [[IntSequenceExplorer]] at any occurrence of the specified value
    *
    * @param value
    *   The value
    * @return
    *   An [[IntSequenceExplorer]]
    */
  def explorerAtAnyOccurrence(value: Int): Option[IntSequenceExplorer] = {
    positionOfAnyOccurrence(value) match {
      case None    => None
      case Some(x) => explorerAtPosition(x)
    }
  }

  /** Returns an [[IntSequenceExplorer]] at the last occurrence of the specified value
    *
    * @param value
    *   The value
    * @return
    *   An [[IntSequenceExplorer]]
    */
  def explorerAtLastOccurrence(value: Int): Option[IntSequenceExplorer] = {
    positionOfLastOccurrence(value: Int) match {
      case None    => None
      case Some(x) => explorerAtPosition(x)
    }
  }

  // Returns the position of the first occurrence of the specified value
  def positionOfFirstOccurrence(value: Int): Option[Int] = {
    positionsOfValue(value) match {
      case null           => None
      case x if x.isEmpty => None
      case x              => Some(x.min)
    }
  }

  // Returns the position of any occurrence of the specified value
  def positionOfAnyOccurrence(value: Int): Option[Int] = {
    positionsOfValue(value) match {
      case null           => None
      case x if x.isEmpty => None
      case x              => Some(x.head)
    }
  }

  // Returns the position of the last occurrence of the specified value
  def positionOfLastOccurrence(value: Int): Option[Int] = {
    positionsOfValue(value) match {
      case null           => None
      case x if x.isEmpty => None
      case x              => Some(x.max)
    }
  }

  /** Inserts a the new value at the specified position.
    *
    * There is two ways to insert a new value, a fast and a normal one. If fast, it returns a
    * [[StackedUpdateIntSequence]]. If normal, it computes a brand new [[ConcreteIntSequence]].
    *
    * @param value
    *   The value to insert as [[Int]]
    * @param pos
    *   The position where to insert the value as [[Int]]
    * @param fast
    *   Fast flag as [[Boolean]] for more detail see description.
    * @param autoRework
    *   //TODO : Remove it , not used, not documented
    * @return
    *   An [[IntSequence]] with the new value
    */
  def insertAtPosition(
    value: Int,
    pos: Int,
    fast: Boolean = false,
    autoRework: Boolean = true
  ): IntSequence

  /** Removes the value at the specified position.
    *
    * There is two ways to delete a value, a fast and a normal one. If fast, it returns a
    * [[StackedUpdateIntSequence]]. If normal, it computes a brand new [[ConcreteIntSequence]].
    *
    * @param pos
    *   The position where to remove the value as [[Int]]
    * @param fast
    *   Fast flag as [[Boolean]] for more detail see description.
    * @param autoRework
    *   //TODO : Remove it , not used, not documented
    * @return
    *   An [[IntSequence]] without the value at the specified position
    */
  def delete(pos: Int, fast: Boolean = false, autoRework: Boolean = false): IntSequence

  /** Moves nodes after a position and optionally flip them.
    *
    * There is two ways to move values, a fast and a normal one. If fast, it returns a
    * [[StackedUpdateIntSequence]]. If normal, it computes a brand new [[ConcreteIntSequence]].
    *
    * @param startPositionIncluded
    *   Starting position of the nodes to move (included) as [[Int]]
    * @param endPositionIncluded
    *   Ending position of the nodes to move (included) as [[Int]]
    * @param moveAfterPosition
    *   The position after which to move the nodes as [[Int]]
    * @param flip
    *   If true, flip the nodes before moving them
    * @param fast
    *   Fast flag as [[Boolean]] for more detail see description.
    * @param autoRework
    *   //TODO : Remove it , not used, not documented
    * @return
    *   An [[IntSequence]] where the nodes have been moved accordingly
    */
  def moveAfter(
    startPositionIncluded: Int,
    endPositionIncluded: Int,
    moveAfterPosition: Int,
    flip: Boolean,
    fast: Boolean = false,
    autoRework: Boolean = true
  ): IntSequence

  /** Flips the [[IntSequence]]
    *
    * @param fast
    *   If true uses [[StackedUpdateIntSequence]] else [[ConcreteIntSequence]]
    * @param autoRework
    *   TODO : remove it
    * @return
    *   A flipped [[IntSequence]]
    */
  def flip(fast: Boolean = false, autoRework: Boolean = true): IntSequence =
    if (this.isEmpty) this
    else moveAfter(0, this.size - 1, -1, flip = true, fast, autoRework)

  /** Applies all [[oscar.cbls.algo.sequence.affineFunction.Pivot]] if the max number of Pivot is
    * reached.
    *
    * The max number of pivot is defined by a percentage of the sequence size.
    *
    * @param maxPivotPerValuePercent
    *   The maximum percent of pivot allowed
    * @param targetToken
    *   TODO : remove this option, target token is always default value (this.token)
    * @return
    *   A [[ConcreteIntSequence]] with or without pivot
    */
  def regularizeToMaxPivot(
    maxPivotPerValuePercent: Int,
    targetToken: Token = this.token
  ): ConcreteIntSequence

  /** Applies all [[oscar.cbls.algo.sequence.affineFunction.Pivot]]
    *
    * @param targetToken
    *   TODO : remove this option, target token is always default value (this.token)
    * @return
    *   A [[ConcreteIntSequence]] with no Pivot
    */
  def regularize(targetToken: Token = this.token): ConcreteIntSequence

  /** Commit all [[StackedUpdateIntSequence]] moves to form a [[ConcreteIntSequence]]
    *
    * @return
    *   An [[IntSequence]] (concrete class is a [[ConcreteIntSequence]])
    */
  def commitPendingMoves: IntSequence

  // Checks if the sequence is coherent
  def check(): Unit = {}

  // Checks if two IntSequence shares the same Token
  def quickEquals(that: IntSequence): Boolean = that != null && this.token == that.token

  // Checks if two IntSequence shares the same Token or have the same elements
  def equals(that: IntSequence): Boolean = {
    quickEquals(that) || (that != null && (this.toList equals that.toList))
  }

  override def toString: String = {
    s"(length:$size)[${this.iterator.toList.mkString(",")}]"
  }

  def descriptorString: String

  // TODO : Not sure about the interest of keeping those two following methods.
  // 		You could just use valueAtPosition(position-1). Otherwise, why not predecessorPredecessorPos2Val ?
  /** Returns the value of the predecessor of the node at the specified position.
    *
    * @param position
    *   The position of the node we want to know the predecessor as an [[Int]]
    * @return
    *   The value of the predecessor as an [[Int]]
    */
  def predecessorPos2Val(position: Int): Option[Int] = {
    valueAtPosition(position - 1)
  }

  /** Returns the value of the successor of the node at the specified position.
    *
    * @param position
    *   The position of the node we want to know the successor as an [[Int]]
    * @return
    *   The value of the successor as an [[Int]]
    */
  def successorPos2Val(position: Int): Option[Int] = {
    valueAtPosition(position + 1)
  }
}
