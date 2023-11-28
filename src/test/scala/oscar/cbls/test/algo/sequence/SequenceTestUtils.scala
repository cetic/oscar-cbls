package oscar.cbls.test.algo.sequence

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.algo.sequence.concrete.ConcreteIntSequence

import scala.util.Random

object SequenceTestUtils {

  private val myTestUtils = new SequenceTestUtils()

  /** Implements manually the moveAfter transformation (to compare with IntSequence)
    * @param list
    *   The original list to swap
    * @param indexFrom
    *   The starting index of the subsequence to flip (inclusive)
    * @param indexTo
    *   The ending index of the subsequence to flip (inclusive)
    * @param destination
    *   The index where the subsequence must be re-inserted (equivalent to moveAfter parameter)
    * @return
    *   A new sequence with the proper transformation
    */
  def flipListManually(
    list: List[Int],
    indexFrom: Int,
    indexTo: Int,
    destination: Int
  ): List[Int] = {

    var flippedList = List[Int]()
    val flip        = list.slice(indexFrom, indexTo + 1).reverse
    val start       = list.take(indexFrom)
    val end = if (indexTo < list.size - 1) list.takeRight(list.size - indexTo - 1) else List()

    if (destination == -1) {
      flippedList = flip ::: start ::: end
    } else {
      if (destination < indexFrom) {
        // Insert the flip at the left
        val part1 = start.take(destination + 1)
        val part2 = start.takeRight(indexFrom - destination - 1)
        flippedList = part1 ::: flip ::: part2 ::: end
      } else {
        // Insert the flip at the right
        val part1 = end.take(destination - indexTo)
        val part2 = end.takeRight(list.size - destination - 1)
        flippedList = start ::: part1 ::: flip ::: part2
      }
    }
    flippedList
  }

  def getRandomParametersForMoveAfter(list: List[Int]): (Int, Int, Int) = {
    val indexFrom   = Random.nextInt(list.size - 1)
    val indexTo     = indexFrom + Random.nextInt(list.size - indexFrom)
    var destination = -1
    if (Random.nextBoolean()) {
      // Insert before indexFrom
      destination = if (indexFrom == 0) -1 else Random.nextInt(indexFrom)
    } else {
      // Insert after indexTo (if indexTo is not the very last index)
      destination =
        if (indexTo < list.size - 1)
          indexTo + Random.nextInt(list.size - indexTo - 1) + 1
        else
          -1
    }

    (indexFrom, indexTo, destination)
  }

  def getRandomParametersForInsert(list: List[Int]): (Int, Int) = {
    val value    = Random.nextInt(1000)
    val position = Random.nextInt(list.size)
    (value, position)
  }

  def compareAllAttributes(intSeq: IntSequence, list: List[Int]): Unit =
    myTestUtils.compare(intSeq, list)
}

class SequenceTestUtils extends AnyFunSuite with Matchers {

  /** Exhaustively compares the IntSequence with a reference list, supposed to be identical
    * @param intSeq
    *   the integer sequence
    * @param list
    *   the reference list
    */
  def compare(intSeq: IntSequence, list: List[Int]): Unit = {
    intSeq.size should be(list.size)
    intSeq.isEmpty should be(list.isEmpty)
    intSeq.nonEmpty should be(list.nonEmpty)
    intSeq.iterator.toList should be(list.iterator.toList)
    intSeq match {
      case sequence: ConcreteIntSequence if list.nonEmpty =>
        sequence.largestValue.get should be(list.max)
        sequence.smallestValue.get should be(list.min)
      case _ =>
    }

    intSeq.unorderedContentNoDuplicate.sorted should be(list.sorted.distinct)
    intSeq.unorderedContentNoDuplicateWithNBOccurrences.sorted should be(
      list.sorted.distinct.map(e => (e, list.count(_ == e)))
    )

    // This will intentionally search for items that are not in the list
    if (list.nonEmpty) {
      for (i <- list.min until list.max) {
        intSeq.nbOccurrence(i) should be(list.count(_ == i))
        intSeq.contains(i) should be(list.contains(i))
        if (intSeq.contains(i)) {
          list(intSeq.positionOfAnyOccurrence(i).get) should be(i)
          intSeq.positionOfFirstOccurrence(i).get should be(list.indexOf(i))
          intSeq.positionOfLastOccurrence(i).get should be(list.lastIndexOf(i))
        } else {
          intSeq.positionOfAnyOccurrence(i) should be(None)
          intSeq.positionOfFirstOccurrence(i) should be(None)
          intSeq.positionOfLastOccurrence(i) should be(None)
        }
      }
    }

    for (i <- list.indices) {
      val positionsOfValue = list.zipWithIndex.filter(_._1 == list(i)).map(_._2).sorted
      intSeq.positionsOfValue(list(i)).sorted should be(positionsOfValue)
      intSeq.valueAtPosition(i).get should be(list(i))

      // Didn't find a matcher for that ...
      list containsSlice intSeq.iterateFromAnyOccurrenceOfValue(list(i)).toList should be(true)
    }

    // It is way too expensive to test all pair of index values exhaustively
    // Instead, test with indexFrom fixed at 0 and exhaustive indexTo, then backwards
    for (j <- 1 until list.size - 1) {
      val slice = list.zipWithIndex.slice(0, j + 1).map { case (e, i) => (i, e) }.sorted
      intSeq.valuesBetweenPositions(0, j).sorted should be(slice.map(_._2).sorted)
      intSeq.positionsAndValuesBetweenPositions(0, j).sorted should be(slice)
    }

    for (j <- 1 until list.size) {
      val slice = list.zipWithIndex.slice(j, list.size).map { case (e, i) => (i, e) }.sorted
      intSeq.valuesBetweenPositions(j, list.size - 1).sorted should be(slice.map(_._2).sorted)
      intSeq.positionsAndValuesBetweenPositions(j, list.size - 1).sorted should be(slice)
    }
  }
}
