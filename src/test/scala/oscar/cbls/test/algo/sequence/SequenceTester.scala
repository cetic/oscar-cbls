package oscar.cbls.test.algo.sequence

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.algo.sequence.{ConcreteIntSequence, IntSequence}

object SequenceTester extends AnyFunSuite with Matchers {

  /** Exhaustively compares the IntSequence with a reference list, supposed to be identical
    * @param intSeq
    *   the integer sequence
    * @param list
    *   the reference list
    */
  def compare(intSeq: IntSequence, list: List[Int]): Unit = {
    (intSeq.iterator.map(_.value).toList == list) should be(true)
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
      list containsSlice intSeq.iterateFromAnyOccurrenceOfValue(list(i)).map(_.value).toList should be(true)
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
