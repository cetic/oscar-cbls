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

import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.sequence.affineFunction.PiecewiseUnitaryAffineFunction
import oscar.cbls.algo.sequence.concrete._
import oscar.cbls.algo.sequence.stackedUpdate._

import scala.annotation.tailrec
import scala.language.implicitConversions

/** Companion object of [[IntSequence]] */
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

  /** Creates an [[IntSequence]] from a sorted list of [[Int]].
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

  /** Creates an empty [[IntSequence]] * */
  def empty(): IntSequence = new ConcreteIntSequence(
    RedBlackTreeMap.empty[Int],
    RedBlackTreeMap.empty[RedBlackTreeMap[Int]],
    PiecewiseUnitaryAffineFunction.identity,
    0
  )

  implicit def toIterable(seq: IntSequence): IterableIntSequence = new IterableIntSequence(seq)
}

/** Companion object of [[Token]] */
object Token {
  def apply(): Token = new Token()
}

/** The identity of an [[IntSequence]].
  *
  * The idea behind this Token is to be able to quickly check if two IntSequence are the same. It
  * allows us to say that a [[StackedUpdateIntSequence]] is the same as a [[ConcreteIntSequence]] if
  * they share the same [[Token]]. Which is mandatory since the regularization mechanism can be
  * trigger any time.
  *
  * By default a new Token is created each time a new [[IntSequence]] is created. The only exception
  * is during regularization where the [[Token]] is copied into the new [[ConcreteIntSequence]]
  */
class Token()

/** Representation of an updatable Sequence of Integer.
  *
  * This abstract class exposes all the methods used to interact with an IntSequence. Such as :
  *   - Basic collections methods (size, contains, map...)
  *   - Iterator / explorer
  *   - Modifications methods : insert/move/remove
  *
  * @param token
  *   A small object used to id the current instance
  * @param depth
  *   The current number of stacked updates
  */
abstract class IntSequence(protected[cbls] val token: Token = Token(), val depth: Int) {

  def size: Int

  def isEmpty: Boolean = size == 0

  def nonEmpty: Boolean = !isEmpty

  def iterator: Iterator[Int] = new IntSequenceIterator(this.explorerAtPosition(0))

  def iterateFromAnyOccurrenceOfValue(value: Int): Iterator[Int] = new IntSequenceIterator(
    this.explorerAtAnyOccurrence(value)
  )

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
    require(
      fromPositionIncluded >= 0 && fromPositionIncluded < size,
      s"fromPositionIncluded must be between 0 and sequence size $size. Got $fromPositionIncluded"
    )
    require(
      toPositionIncluded >= fromPositionIncluded && toPositionIncluded < size,
      s"toPositionIncluded must be between fromPositionIncluded $fromPositionIncluded and sequence size $size. Got $toPositionIncluded"
    )
    val explorer = explorerAtPosition(fromPositionIncluded).get
    tailRecValuesBetweenPositions(explorer, toPositionIncluded)
  }

  def positionsAndValuesBetweenPositions(
    fromPositionIncluded: Int,
    toPositionIncluded: Int
  ): List[(Int, Int)] = {
    require(
      fromPositionIncluded >= 0 && fromPositionIncluded < size,
      s"fromPositionIncluded must be between 0 and sequence size $size. Got $fromPositionIncluded"
    )
    require(
      toPositionIncluded >= fromPositionIncluded && toPositionIncluded < size,
      s"toPositionIncluded must be between fromPositionIncluded $fromPositionIncluded and sequence size $size. Got $toPositionIncluded"
    )
    val explorer = explorerAtPosition(fromPositionIncluded)
    tailRecPositionAndValuesBetweenPositions(explorer.get, toPositionIncluded)
  }

  @tailrec
  private def tailRecValuesBetweenPositions(
    explorer: IntSequenceExplorer,
    toPositionIncluded: Int,
    values: List[Int] = List.empty
  ): List[Int] = {
    if (explorer.position == toPositionIncluded) values ::: List(explorer.value)
    else
      tailRecValuesBetweenPositions(
        explorer.next.get,
        toPositionIncluded,
        values ::: List(explorer.value)
      )
  }

  @tailrec
  private def tailRecPositionAndValuesBetweenPositions(
    explorer: IntSequenceExplorer,
    toPositionIncluded: Int,
    positionsAndValues: List[(Int, Int)] = List.empty
  ): List[(Int, Int)] = {
    if (explorer.position == toPositionIncluded)
      positionsAndValues ::: List((explorer.position, explorer.value))
    else
      tailRecPositionAndValuesBetweenPositions(
        explorer.next.get,
        toPositionIncluded,
        positionsAndValues ::: List((explorer.position, explorer.value))
      )
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
      case Nil => None
      case x   => Some(x.min)
    }
  }

  // Returns the position of any occurrence of the specified value
  def positionOfAnyOccurrence(value: Int): Option[Int] = {
    positionsOfValue(value) match {
      case Nil => None
      case x   => Some(x.head)
    }
  }

  // Returns the position of the last occurrence of the specified value
  def positionOfLastOccurrence(value: Int): Option[Int] = {
    positionsOfValue(value) match {
      case Nil => None
      case x   => Some(x.max)
    }
  }

  /** Inserts a the new value at the specified position.
    *
    * There is two ways to insert a new value, a fast and a normal one. If fast, it returns a
    * [[StackedUpdateIntSequence]]. If normal, it computes a brand new [[ConcreteIntSequence]].
    *
    * @param value
    *   The value to insert as [[Int]]
    * @param insertAfterPositionExpl
    *   The position where to insert the value as [[Int]]
    * @param fast
    *   Fast flag as [[Boolean]] for more detail see description.
    * @return
    *   An [[IntSequence]] with the new value
    */
  def insertAfterPosition(value: Int, insertAfterPositionExpl: Option[IntSequenceExplorer], fast: Boolean = false): IntSequence

  /** Removes the value at the specified position.
    *
    * There is two ways to delete a value, a fast and a normal one. If fast, it returns a
    * [[StackedUpdateIntSequence]]. If normal, it computes a brand new [[ConcreteIntSequence]].
    *
    * @param pos
    *   The position where to remove the value as [[Int]]
    * @param fast
    *   Fast flag as [[Boolean]] for more detail see description.
    * @return
    *   An [[IntSequence]] without the value at the specified position
    */
  def delete(removePosAsExplorer: IntSequenceExplorer, fast: Boolean = false): IntSequence

  /** Moves nodes after a position and optionally flip them.
    *
    * There is two ways to move values, a fast and a normal one. If fast, it returns a
    * [[StackedUpdateIntSequence]]. If normal, it computes a brand new [[ConcreteIntSequence]].
    *
    * @param fromIncludedExpl
    *   Starting position of the nodes to move (included) as [[Int]]
    * @param toIncludedExpl
    *   Ending position of the nodes to move (included) as [[Int]]
    * @param moveAfterExpl
    *   The position after which to move the nodes as [[Int]]
    * @param flip
    *   If true, flip the nodes before moving them
    * @param fast
    *   Fast flag as [[Boolean]] for more detail see description.
    * @return
    *   An [[IntSequence]] where the nodes have been moved accordingly
    */
  def moveAfter(
                 fromIncludedExpl: IntSequenceExplorer,
                 toIncludedExpl: IntSequenceExplorer,
                 moveAfterExpl: Option[IntSequenceExplorer],
                 flip: Boolean,
                 fast: Boolean = false
  ): IntSequence

  /** Flips the [[IntSequence]]
    *
    * @param fast
    *   If true uses [[StackedUpdateIntSequence]] else [[ConcreteIntSequence]]
    * @return
    *   A flipped [[IntSequence]]
    */
  def flip(fast: Boolean = false): IntSequence =
    if (this.isEmpty) this
    else moveAfter(this.explorerAtPosition(0).get, this.explorerAtPosition(size - 1).get, None, flip = true, fast)

  /** Regularizes the current [[IntSequence]] if the max number of pivot is reached
    *
    * 3 steps are involved in the process :
    *   - Commits all stacked updates (see [[StackedUpdateIntSequence]])
    *   - Applies all pivots IF MAX PIVOTS IS REACHED (see [[ConcreteIntSequence]])
    *   - Set the resulting [[ConcreteIntSequence]] token with this.token
    *
    * @param maxPivotPerValuePercent
    *   The maximum percent of pivot allowed
    * @param targetToken
    *   The identity of the resulting [[ConcreteIntSequence]]
    * @return
    * A [[ConcreteIntSequence]] or and [[EmptyIntSequence]] with or without pivot
    */
  def regularizeToMaxPivot(
    maxPivotPerValuePercent: Int,
    targetToken: Token = this.token
  ): IntSequence

  /** Regularizes the current [[IntSequence]]
    *
    * 3 steps are involved in the process :
    *   - Commits all stacked updates (see [[StackedUpdateIntSequence]])
    *   - Applies all pivots (see [[ConcreteIntSequence]])
    *   - Set the resulting [[ConcreteIntSequence]] token with this.token
    *
    * @param targetToken
    *   The identity of the resulting [[ConcreteIntSequence]]
    * @return
    *   A [[ConcreteIntSequence]] with no Pivot
    */
  def regularize(targetToken: Token = this.token): IntSequence

  /** Commit all [[StackedUpdateIntSequence]] moves to form a [[ConcreteIntSequence]]
    *
    * @return
    *   An [[IntSequence]] (concrete class is a [[ConcreteIntSequence]])
    */
  def commitPendingMoves: IntSequence

  // Checks if the sequence is coherent
  def check(): Unit = {}

  // Checks if two IntSequence shares the same Token namely the same identity
  def quickEquals(that: IntSequence): Boolean = that != null && this.token == that.token

  // Checks if two IntSequence shares the same Token or have the same elements
  def equals(that: IntSequence): Boolean = {
    quickEquals(that) || (that != null && (this.toList equals that.toList))
  }

  override def toString: String = {
    s"(length:$size)[${this.iterator.toList.mkString(",")}]"
  }

  def descriptorString: String
}
