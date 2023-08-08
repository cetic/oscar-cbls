package oscar.cbls.test.algo.sequence

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import oscar.cbls.algo.sequence.PiecewiseSequenceShiftingBijection

class PiecewiseSequenceShiftingBijectionUnitTestSuite extends AnyFunSuite {

  test("PiecewiseSequenceShiftingBijection : identity creation build the identity object") {
    val piecewiseSequenceShiftingBijection: PiecewiseSequenceShiftingBijection =
      PiecewiseSequenceShiftingBijection.identity
    piecewiseSequenceShiftingBijection.isIdentity should be(true)
    for (i <- 0 until 20) piecewiseSequenceShiftingBijection(i) should be(i)
  }

  test("PiecewiseSequenceShiftingBijection : pivots creation works as expected (no pivot)") {
    val pssb: PiecewiseSequenceShiftingBijection = PiecewiseSequenceShiftingBijection.identity
    val pivots                                   = pssb.pivots
    val pssb2 = PiecewiseSequenceShiftingBijection.createFromPivots(pivots)
    pssb2.pivots.sortBy(_.fromValue) should be(pivots.sortBy(_.fromValue))
  }

  test("PiecewiseSequenceShiftingBijection : pivots creation works as expected") {
    var pssb: PiecewiseSequenceShiftingBijection =
      PiecewiseSequenceShiftingBijection.identity
    pssb = pssb.swapAdjacentZonesShiftFirst(5, 9, 14, true)
    pssb = pssb.swapAdjacentZonesShiftFirst(3, 21, 34, true)
    pssb = pssb.swapAdjacentZonesShiftFirst(12, 15, 22, true)
    pssb = pssb.swapAdjacentZonesShiftFirst(21, 24, 35, true)
    val pivots = pssb.pivots
    val pssb2  = PiecewiseSequenceShiftingBijection.createFromPivots(pivots)
    pssb2.pivots.sortBy(_.fromValue) should be(pivots.sortBy(_.fromValue))
  }

  test("PiecewiseSequenceShiftingBijection : backward works as expected (identity)") {
    val pssb: PiecewiseSequenceShiftingBijection = PiecewiseSequenceShiftingBijection.identity
    val backward                                 = pssb.backward
    for (i <- 0 until 100) backward(pssb(i)) should be(i)
  }

  test("PiecewiseSequenceShiftingBijection : backward works as expected") {
    var pssb: PiecewiseSequenceShiftingBijection =
      PiecewiseSequenceShiftingBijection.identity
    pssb = pssb.swapAdjacentZonesShiftFirst(5, 9, 14, true)
    pssb = pssb.swapAdjacentZonesShiftFirst(3, 21, 34, true)
    pssb = pssb.swapAdjacentZonesShiftFirst(12, 15, 22, true)
    pssb = pssb.swapAdjacentZonesShiftFirst(21, 24, 35, true)
    val backward = pssb.backward
    for (i <- 0 until 100) backward(pssb(i)) should be(i)
  }

  test("PiecewiseSequenceShiftingBijection : pivots access work as expected") {
    var pssb: PiecewiseSequenceShiftingBijection = PiecewiseSequenceShiftingBijection.identity
    pssb.firstPivotAndPosition should be(None)
    pssb.pivotWithPositionApplyingTo(10) should be(None)
    pssb.pivots.size should be(0)
    pssb = pssb.swapAdjacentZonesShiftFirst(0, 4, 9, true)
    pssb = pssb.swapAdjacentZonesShiftFirst(10, 14, 19, true)
    pssb.firstPivotAndPosition.nonEmpty should be(true)
    pssb.firstPivotAndPosition.get.key should be(0)
    pssb.pivots.size should be(5) // two swaps + identity bijection
    pssb.pivotWithPositionApplyingTo(11).nonEmpty should be(true)
    pssb.pivotWithPositionApplyingTo(11).get.key should be(10)
    pssb.positionOfValue(0).nonEmpty should be(true)
    pssb.positionOfValue(10).nonEmpty should be(true)
    pssb.positionOfValue(12).nonEmpty should be(false)
  }

}
