package oscar.cbls.test.algo.sequence

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
import oscar.cbls.algo.sequence._

class PiecewiseSequenceShiftingBijectionSuite extends AnyFunSuite {

  val sequenceSize = 200

  val genBijections: Gen[List[Update]] = {
    val genNonFlip: Gen[Update] = for {
      from   <- Gen.choose[Int](0, sequenceSize - 1)
      to     <- Gen.choose[Int](from, sequenceSize - 1)
      offset <- Gen.choose((-1) * from, sequenceSize - to)
    } yield Update(from, to, SequenceShiftingBijection(offset, false))
    val genFlip: Gen[Update] = for {
      from   <- Gen.choose[Int](0, sequenceSize - 1)
      to     <- Gen.choose[Int](from, sequenceSize - 1)
      offset <- Gen.choose(to, sequenceSize + from)
    } yield Update(from, to, SequenceShiftingBijection(offset, true))

    Gen.listOf(Gen.frequency((1, genNonFlip), (1, genFlip)))
  }

  test(s"PiecewiseSequenceShiftingBijection : Batch bijections has expected result") {
    forAll(genBijections, minSuccessful(100)) { updates: List[Update] =>
      whenever(updates.size >= 20) {
        var piecewiseSequenceShiftingBijection: PiecewiseSequenceShiftingBijection =
          PiecewiseSequenceShiftingBijection.identity
        var piecewiseSequenceShiftingBijectionNaive: PiecewiseSequenceShiftingBijectionNaive =
          IdentityNaive

        for (update <- updates) {
          piecewiseSequenceShiftingBijection = piecewiseSequenceShiftingBijection
            .updateForCompositionBefore(update.from, update.to, update.bijection)
          piecewiseSequenceShiftingBijectionNaive = piecewiseSequenceShiftingBijectionNaive
            .updateBefore(update.from, update.to, update.bijection)
        }
        for (i <- 0 until sequenceSize) {
          piecewiseSequenceShiftingBijection(i) should be(
            piecewiseSequenceShiftingBijectionNaive(i)
          )
        }
      }
    }
  }

}

case class Update(from: Int, to: Int, bijection: SequenceShiftingBijection)

sealed abstract class PiecewiseSequenceShiftingBijectionNaive {
  def apply(value: Int): Int
  def updateBefore(
    fromIncluded: Int,
    toIncluded: Int,
    update: SequenceShiftingBijection
  ): PiecewiseSequenceShiftingBijectionNaive =
    UpdatedPiecewiseLinearFunNaive(
      fromIncluded,
      toIncluded,
      update: SequenceShiftingBijection,
      this
    )
}
case object IdentityNaive extends PiecewiseSequenceShiftingBijectionNaive {
  override def apply(value: Int): Int = value
}
case class UpdatedPiecewiseLinearFunNaive(
  fromIncluded: Int,
  toIncluded: Int,
  update: SequenceShiftingBijection,
  base: PiecewiseSequenceShiftingBijectionNaive
) extends PiecewiseSequenceShiftingBijectionNaive {
  override def apply(value: Int): Int =
    if (value >= fromIncluded && value <= toIncluded) base(update(value)) else base(value)
}
