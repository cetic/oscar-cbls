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

import scala.collection.immutable.SortedSet
import scala.language.implicitConversions

// Companion object of [[IntSequence]]
object IntSequence {
  /** Creates an [[ConcreteIntSequence]] from a sorted list of integers.
   *
   * @param values The sorted integers as a [[Iterable]] of [[Int]]
   * @return A [[ConcreteIntSequence]] with the sorted integers
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
   * @param values The sorted integers as a [[Iterable]] of [[Int]]
   * @return An IntSequence with the sorted integers
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

class IterableIntSequence(sequence: IntSequence) extends Iterable[Int] {
  override def iterator: Iterator[Int] = sequence.iterator

  override def head: Int = sequence.valueAtPosition(0).head

  override def headOption: Option[Int] = sequence.valueAtPosition(0)

  override def last: Int = sequence.valueAtPosition(sequence.size - 1).head

  override def lastOption: Option[Int] = sequence.valueAtPosition(sequence.size - 1)
}

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

  def nbOccurrence(value: Int): Int

  def unorderedContentNoDuplicate: List[Int]

  def unorderedContentNoDuplicateWithNBOccurences: List[(Int, Int)]

  def valueAtPosition(position: Int): Option[Int]

  final def positionsOfValue(value: Int): Iterable[Int] = {
    positionsOfValueQ(value).toIterable
  }

  final def positionsOfValueSet(value: Int): SortedSet[Int] = {
    SortedSet.empty[Int] ++ positionsOfValue(value)
  }

  def positionsOfValueQ(value: Int): List[Int]

  def contains(value: Int): Boolean

  def explorerAtPosition(position: Int): Option[IntSequenceExplorer]

  def map(fun: Int => Int): IntSequence = {
    val l: List[Int] = this.iterator.toList
    val l2           = l.map(fun)
    IntSequence.apply(l2)
  }

  def valuesBetweenPositionsSet(
    fromPositionIncluded: Int,
    toPositionIncluded: Int
  ): SortedSet[Int] = {
    var toReturn = SortedSet.empty[Int]
    var e        = explorerAtPosition(fromPositionIncluded)
    while (
      e match {
        case None => false
        case Some(explorer) =>
          if (explorer.position <= toPositionIncluded) {
            toReturn = toReturn + explorer.value
            e = explorer.next
            true
          } else false
      }
    ) {}
    toReturn
  }

  def valuesBetweenPositionsList(fromPositionIncluded: Int, toPositionIncluded: Int): List[Int] = {
    var toReturn: List[Int] = null
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

  // List[(position,value)]
  def positionsBetweenFromToAndTheirValues(
    fromPositionIncluded: Int,
    toPositionIncluded: Int
  ): List[(Int, Int)] = {
    var toReturn: List[(Int, Int)] = null
    var e                          = explorerAtPosition(fromPositionIncluded)
    while (true) { // TODO: maybe this approach has less overhead than the "while" above?
      e match {
        case None => return toReturn
        case Some(explorer) =>
          if (explorer.position > toPositionIncluded) {
            return toReturn
          }
          toReturn = List((explorer.position, explorer.value)) ::: toReturn
          e = explorer.next
      }
    }
    null
  }

  def explorerAtFirstOccurrence(value: Int): Option[IntSequenceExplorer] = {
    positionOfFirstOccurrence(value: Int) match {
      case None    => None
      case Some(x) => explorerAtPosition(x)
    }
  }

  def explorerAtLastOccurrence(value: Int): Option[IntSequenceExplorer] = {
    positionOfLastOccurrence(value: Int) match {
      case None    => None
      case Some(x) => explorerAtPosition(x)
    }
  }

  def explorerAtAnyOccurrence(value: Int): Option[IntSequenceExplorer] = {
    positionOfAnyOccurrence(value) match {
      case None    => None
      case Some(x) => explorerAtPosition(x)
    }
  }

  def positionOfFirstOccurrence(value: Int): Option[Int] = {
    positionsOfValue(value) match {
      case null           => None
      case x if x.isEmpty => None
      case x              => Some(x.min)
    }
  }

  def positionOfLastOccurrence(value: Int): Option[Int] = {
    positionsOfValue(value) match {
      case null           => None
      case x if x.isEmpty => None
      case x              => Some(x.max)
    }
  }

  def positionOfAnyOccurrence(value: Int): Option[Int] = {
    positionsOfValue(value) match {
      case null           => None
      case x if x.isEmpty => None
      case x              => Some(x.head)
    }
  }

  def insertAtPosition(
    value: Int,
    pos: Int,
    fast: Boolean = false,
    autoRework: Boolean = true
  ): IntSequence
  def delete(pos: Int, fast: Boolean = false, autoRework: Boolean = false): IntSequence
  def moveAfter(
    startPositionIncluded: Int,
    endPositionIncluded: Int,
    moveAfterPosition: Int,
    flip: Boolean,
    fast: Boolean = false,
    autoRework: Boolean = true
  ): IntSequence

  def flip(fast: Boolean = false, autoRework: Boolean = true): IntSequence =
    if (this.isEmpty) this
    else moveAfter(0, this.size - 1, -1, flip = true, fast, autoRework)

  def regularizeToMaxPivot(
    maxPivotPerValuePercent: Int,
    targetToken: Token = this.token
  ): ConcreteIntSequence

  def regularize(targetToken: Token = this.token): ConcreteIntSequence
  def commitPendingMoves: IntSequence

  def check(): Unit = {}

  def quickEquals(that: IntSequence): Boolean = that != null && this.token == that.token
  def equals(that: IntSequence): Boolean = {
    quickEquals(that) || (that != null && (this.toList equals that.toList))
  }

  override def toString: String = {
    s"(length:$size)[${this.iterator.toList.mkString(",")}]"
  }

  def descriptorString: String

  def predecessorPos2Val(position: Int): Option[Int] = {
    valueAtPosition(position - 1)
  }

  def successorPos2Val(position: Int): Option[Int] = {
    valueAtPosition(position + 1)
  }
}

class IntSequenceIterator(var crawler: Option[IntSequenceExplorer]) extends Iterator[Int] {

  override def hasNext: Boolean =
    crawler match {
      case None    => false
      case Some(_) => true
    }

  override def next(): Int = {
    val position = crawler.head
    crawler = position.next
    position.value
  }
}

abstract class IntSequenceExplorer {
  val value: Int
  def position: Int
  def next: Option[IntSequenceExplorer]
  def prev: Option[IntSequenceExplorer]
}








