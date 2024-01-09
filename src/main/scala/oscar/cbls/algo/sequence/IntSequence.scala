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

  /** Creates a ConcreteIntSequence from an [[scala.Iterable]] of [[scala.Int]]
    *
    * Beware: in [[scala.Iterable]], the order do not always matter. But in IntSequence, the order
    * does matter. If you give this constructor a collection where the order do not matter, an
    * arbitrary order will be decided once and for all.
    *
    * @param values
    *   The Int values used to initiate the sequence
    * @return
    *   A ConcreteIntSequence with the integers (ordered)
    */
  def apply(values: Iterable[Int]): IntSequence = {
    val valuesArray             = values.toArray
    val internalPositionToValue = RedBlackTreeMap.makeFromSortedContinuousArray(valuesArray)
    val valueToInternalPositions: RedBlackTreeMap[RedBlackTreeMap[Int]] =
      aggregatePosOnValToInternalPosFrom(valuesArray)

    new ConcreteIntSequence(
      internalPositionToValue,
      valueToInternalPositions,
      PiecewiseUnitaryAffineFunction.identity,
      valuesArray.length
    )
  }

  /** Creates the structure that'll hold the positions of each value in the sequence.
    *
    * It's a two level RedBclackTree structure. The first level holds the value of the sequence, the
    * second one the positions of those values.
    *
    * @param values
    *   The ordered values of the sequence
    * @return
    *   The two level RedBlackTree holding the positons of each value
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
  def empty(): IntSequence = EmptyIntSequence()

  implicit def toIterable(seq: IntSequence): IterableIntSequence = new IterableIntSequence(seq)
}

/** Representation of an updatable Sequence of Integer.
  *
  * This abstract class exposes all the methods used to interact with an IntSequence. Such as :
  *   - Basic collections methods (size, contains, map...)
  *   - Iterator / explorer
  *   - Modifications methods : insert/move/remove
  *
  * @param token
  *   An object used to id the current instance
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

  /** Returns the number of occurrence of the specified value */
  def nbOccurrence(value: Int): Int

  /** Returns the content of the sequence without duplicates */
  def unorderedContentNoDuplicate: List[Int]

  /** Returns the content of the sequence without duplicates associated with their number of
    * occurrence
    */
  def unorderedContentNoDuplicateWithNBOccurrences: List[(Int, Int)]

  /** Returns the optional value at the specified position
    *
    * @param position
    *   The position where to search for the value
    * @return
    *   None if the position is not in the IntSequence size. Else Some value
    */
  def valueAtPosition(position: Int): Option[Int]

  /** Returns all the positions of the specified value */
  def positionsOfValue(value: Int): List[Int]

  /** Checks whether or not the IntSequence contains the specified value */
  def contains(value: Int): Boolean

  /** Returns an optional [[IntSequenceExplorer]] at the specified position
    *
    * If the required position is -1, it'll return a [[concrete.RootIntSequenceExplorer]] so that the user is
    * able to insert at the front of the sequence. After position -1.
    *
    * @param position
    *   The position where to search for the explorer
    * @return
    *   None if the position is not in the IntSequence size nor equal to -1. Else Some
    *   IntSequenceExplorer
    */
  def explorerAtPosition(position: Int): Option[IntSequenceExplorer]

  /** Applies a function to each element of the sequence
    *
    * @param fun
    *   The function that will be applied to each element.
    * @return
    *   A new IntSequence with modified values.
    */
  def map(fun: Int => Int): IntSequence = {
    val l: List[Int] = this.iterator.toList
    val l2           = l.map(fun)
    IntSequence.apply(l2)
  }

  /** Returns the values between the specified positions (included)
    *
    * @param fromPositionIncluded
    *   Starting position (included)
    * @param toPositionIncluded
    *   Ending position (included)
    * @return
    *   The values between the included specified positions
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

  /** Returns the values and their positions between the specified positions (included)
    *
    * @param fromPositionIncluded
    *   Starting position (included)
    * @param toPositionIncluded
    *   Ending position (included)
    * @return
    *   The values and their positions between the included specified positions
    */
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

  /** Returns [[scala.Some]] [[IntSequenceExplorer]] at the first occurrence of the specified value
    * or [[scala.None]] if the value is not in the sequence.
    *
    * @param value
    *   The value we are looking for
    * @return
    *   Some IntSequenceExplorer or None if not found
    */
  def explorerAtFirstOccurrence(value: Int): Option[IntSequenceExplorer] = {
    positionOfFirstOccurrence(value: Int) match {
      case None    => None
      case Some(x) => explorerAtPosition(x)
    }
  }

  /** Returns [[scala.Some]] [[IntSequenceExplorer]] at any occurrence of the specified value or
    * [[scala.None]] if the value is not in the sequence.
    *
    * @param value
    *   The value we are looking for
    * @return
    *   Some IntSequenceExplorer or None if not found
    */
  def explorerAtAnyOccurrence(value: Int): Option[IntSequenceExplorer] = {
    positionOfAnyOccurrence(value) match {
      case None    => None
      case Some(x) => explorerAtPosition(x)
    }
  }

  /** Returns [[scala.Some]] [[IntSequenceExplorer]] at the last occurrence of the specified value
    * or [[scala.None]] if the value is not in the sequence.
    *
    * @param value
    *   The value we are looking for
    * @return
    *   Some IntSequenceExplorer or None if not found
    */
  def explorerAtLastOccurrence(value: Int): Option[IntSequenceExplorer] = {
    positionOfLastOccurrence(value: Int) match {
      case None    => None
      case Some(x) => explorerAtPosition(x)
    }
  }

  /** Returns the position of the first occurrence of the specified value or [[scala.None]] if not
    * found
    */
  def positionOfFirstOccurrence(value: Int): Option[Int] = {
    positionsOfValue(value) match {
      case Nil => None
      case x   => Some(x.min)
    }
  }

  /** Returns the position of any occurrence of the specified value or [[scala.None]] if not found
    */
  def positionOfAnyOccurrence(value: Int): Option[Int] = {
    positionsOfValue(value) match {
      case Nil => None
      case x   => Some(x.head)
    }
  }

  /** Returns the position of the last occurrence of the specified value or [[scala.None]] if not
    * found
    */
  def positionOfLastOccurrence(value: Int): Option[Int] = {
    positionsOfValue(value) match {
      case Nil => None
      case x   => Some(x.max)
    }
  }

  /** Inserts a new value after the specified position.
    *
    * There is two ways to insert a new value, a fast and a normal one. If fast, it returns a
    * [[oscar.cbls.algo.sequence.stackedUpdate.StackedUpdateIntSequence]]. If normal, it computes a
    * brand new [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]].
    *
    * @param value
    *   The value to insert
    * @param insertAfterPositionExplorer
    *   The position after which to insert the value
    * @param fast
    *   Fast flag (for more detail see description)
    * @return
    *   An IntSequence with the new value
    */
  def insertAfterPosition(
    value: Int,
    insertAfterPositionExplorer: IntSequenceExplorer,
    fast: Boolean = false
  ): IntSequence

  /** Removes the value at the specified position given by an explorer
    *
    * There is two ways of removing a value, a fast and a normal one. If fast, it returns a
    * [[oscar.cbls.algo.sequence.stackedUpdate.StackedUpdateIntSequence]]. If normal, it computes a
    * brand new [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]].
    *
    * @param removePosAsExplorer
    *   The explorer at the position where to remove the value
    * @param fast
    *   Fast flag (for more detail see description)
    * @return
    *   An [[IntSequence]] without the value at the specified position
    */
  def remove(removePosAsExplorer: IntSequenceExplorer, fast: Boolean = false): IntSequence

  /** Moves a subsequence of values after a position and optionally flip it. The subsequence of
    * values is identified by the starting and ending position.
    *
    * There is two ways to move values, a fast and a normal one. If fast, it returns a
    * [[oscar.cbls.algo.sequence.stackedUpdate.StackedUpdateIntSequence]]. If normal, it computes a
    * brand new [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]].
    *
    * @param fromIncludedExplorer
    *   Starting position of the subsequence to move (included)
    * @param toIncludedExplorer
    *   Ending position of the subsequence to move (included)
    * @param moveAfterExplorer
    *   The position after which to move the subsequence
    * @param flip
    *   If true, flips the subsequence before moving it
    * @param fast
    *   Fast flag (for more detail see description)
    * @return
    *   An IntSequence where the subsequence have been moved accordingly
    */
  def moveAfter(
    fromIncludedExplorer: IntSequenceExplorer,
    toIncludedExplorer: IntSequenceExplorer,
    moveAfterExplorer: IntSequenceExplorer,
    flip: Boolean,
    fast: Boolean = false
  ): IntSequence

  /** Flips the [[IntSequence]]
    *
    * There is two ways to move values, a fast and a normal one. If fast, it returns a
    * [[oscar.cbls.algo.sequence.stackedUpdate.StackedUpdateIntSequence]]. If normal, it computes a
    * brand new [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]].
    *
    * @param fast
    *   Fast flag (for more detail see description)
    * @return
    *   A flipped [[IntSequence]]
    */
  def flip(fast: Boolean = false): IntSequence =
    if (this.isEmpty) this
    else
      moveAfter(
        this.explorerAtPosition(0).get,
        this.explorerAtPosition(size - 1).get,
        new RootIntSequenceExplorer(this),
        flip = true,
        fast
      )

  /** Regularizes the current [[IntSequence]] if the max number of pivot is reached
    *
    * 3 steps are involved in the process :
    *   - Commits all stacked updates (see
    *     [[oscar.cbls.algo.sequence.stackedUpdate.StackedUpdateIntSequence]])
    *   - Applies all pivots IF MAX PIVOTS IS REACHED (see
    *     [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]])
    *   - Set the resulting [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]] token with
    *     this.token
    *
    * @param maxPivotPerValuePercent
    *   The maximum percent of pivot allowed
    * @param targetToken
    *   The identity of the resulting [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]]
    * @return
    *   A [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]] or and
    *   [[oscar.cbls.algo.sequence.concrete.EmptyIntSequence]] with or without pivot
    */
  def regularizeToMaxPivot(
    maxPivotPerValuePercent: Int,
    targetToken: Token = this.token
  ): IntSequence

  /** Regularizes the current [[IntSequence]]
    *
    * 3 steps are involved in the process :
    *   - Commits all stacked updates (see StackedUpdateIntSequence)
    *   - Applies all pivots (see [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]])
    *   - Set the resulting [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]] token with
    *     this.token
    *
    * @param targetToken
    *   The identity token to give at the resulting
    *   [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]]
    * @return
    *   A [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]] with no Pivot
    */
  def regularize(targetToken: Token = this.token): IntSequence

  /** Commit all [[oscar.cbls.algo.sequence.stackedUpdate.StackedUpdateIntSequence]] moves to form a
    * [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]]
    *
    * @return
    *   An [[IntSequence]] (concrete class is a
    *   [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]])
    */
  def commitPendingMoves: IntSequence

  /** Checks if the sequence is coherent */
  def check(): Unit = {}

  /** Checks if two IntSequence shares the same Token namely the same identity */
  def quickEquals(that: IntSequence): Boolean = that != null && this.token == that.token

  /** Checks if two IntSequence shares the same Token or have the same elements */
  def equals(that: IntSequence): Boolean = {
    quickEquals(that) || (that != null && (this.toList equals that.toList))
  }

  override def toString: String = {
    s"(length:$size)[${this.iterator.toList.mkString(",")}]"
  }

  /** Special string used to recursively describe the whole IntSequence with all stacked updated */
  def descriptorString: String
}

/** The identity of an [[IntSequence]].
  *
  * The idea behind this Token is to be able to quickly check if two IntSequence are the same. It
  * allows us to say that a [[oscar.cbls.algo.sequence.stackedUpdate.StackedUpdateIntSequence]] is
  * the same as a [[oscar.cbls.algo.sequence.concrete.ConcreteIntSequence]] if they share the same
  * [[Token]]. Which is mandatory since the regularization mechanism can be trigger any time.
  *
  * By default a new Token is created each time a new IntSequence is created. The only exception is
  * during regularization where the Token is copied into the new ConcreteIntSequence
  */
case class Token()
