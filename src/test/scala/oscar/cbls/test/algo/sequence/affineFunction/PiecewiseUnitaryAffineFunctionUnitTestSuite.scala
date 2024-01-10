package oscar.cbls.test.algo.sequence.affineFunction

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import oscar.cbls.algo.sequence.affineFunction.PiecewiseUnitaryAffineFunction

class PiecewiseUnitaryAffineFunctionUnitTestSuite extends AnyFunSuite {

  test("PiecewiseSequenceShiftingBijection : identity creation build the identity object") {
    val piecewiseSequenceShiftingBijection: PiecewiseUnitaryAffineFunction =
      PiecewiseUnitaryAffineFunction.identity
    piecewiseSequenceShiftingBijection.isIdentity should be(true)
    for (i <- 0 until 20) piecewiseSequenceShiftingBijection(i) should be(i)
  }

  test("PiecewiseSequenceShiftingBijection : pivots creation works as expected (no pivot)") {
    val pssb: PiecewiseUnitaryAffineFunction = PiecewiseUnitaryAffineFunction.identity
    val pivots                                   = pssb.pivots
    val pssb2 = PiecewiseUnitaryAffineFunction.createFromPivots(pivots)
    pssb2.pivots.sortBy(_.fromValue) should be(pivots.sortBy(_.fromValue))
  }

  test("PiecewiseSequenceShiftingBijection : pivots creation works as expected") {
    var pssb: PiecewiseUnitaryAffineFunction =
      PiecewiseUnitaryAffineFunction.identity
    pssb = pssb.swapAdjacentZonesShiftFirst(5, 9, 14, flipZone2 = true)
    pssb = pssb.swapAdjacentZonesShiftFirst(3, 21, 34, flipZone2 = true)
    pssb = pssb.swapAdjacentZonesShiftFirst(12, 15, 22, flipZone2 = true)
    pssb = pssb.swapAdjacentZonesShiftFirst(21, 24, 35, flipZone2 = true)
    val pivots = pssb.pivots
    val pssb2  = PiecewiseUnitaryAffineFunction.createFromPivots(pivots)
    pssb2.pivots.sortBy(_.fromValue) should be(pivots.sortBy(_.fromValue))
  }

  test("PiecewiseSequenceShiftingBijection : backward works as expected") {
    var pssb: PiecewiseUnitaryAffineFunction =
      PiecewiseUnitaryAffineFunction.identity
    val backwardIdentity = pssb.backward
    for (i <- 0 until 100) backwardIdentity(pssb(i)) should be(i)

    pssb = pssb.swapAdjacentZonesShiftFirst(5, 9, 14, flipZone2 = true)
    pssb = pssb.swapAdjacentZonesShiftFirst(3, 21, 34, flipZone2 = true)
    pssb = pssb.swapAdjacentZonesShiftFirst(12, 15, 22, flipZone2 = true)
    pssb = pssb.swapAdjacentZonesShiftFirst(21, 24, 35, flipZone2 = true)

    val backward = pssb.backward
    for (i <- 0 until 100) backward(pssb(i)) should be(i)
  }

  /* C
   */
  test("PiecewiseSequenceShiftingBijection : pivots access work as expected") {
    var pssb: PiecewiseUnitaryAffineFunction = PiecewiseUnitaryAffineFunction.identity
    pssb.firstPivotAndPosition should be(None)
    pssb.pivotWithPositionApplyingTo(10) should be(None)
    pssb.pivots.size should be(0)

    pssb = pssb.swapAdjacentZonesShiftFirst(0, 4, 9, flipZone2 = true)
    pssb = pssb.swapAdjacentZonesShiftFirst(10, 14, 19, flipZone2 = true)

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
