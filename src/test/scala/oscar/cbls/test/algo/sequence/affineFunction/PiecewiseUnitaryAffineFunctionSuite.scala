package oscar.cbls.test.algo.sequence.affineFunction

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
import oscar.cbls.algo.sequence.affineFunction.{PiecewiseUnitaryAffineFunction, UnitaryAffineFunction}

class PiecewiseUnitaryAffineFunctionSuite extends AnyFunSuite {

  val sequenceSize = 20

  val genBijections: Gen[List[Update]] = {
    val genNonFlip: Gen[Update] = for {
      from   <- Gen.choose[Int](0, sequenceSize - 1)
      to     <- Gen.choose[Int](from, sequenceSize - 1)
      offset <- Gen.choose((-1) * from, sequenceSize - to)
    } yield Update(from, to, UnitaryAffineFunction(offset, false))
    val genFlip: Gen[Update] = for {
      from   <- Gen.choose[Int](0, sequenceSize - 1)
      to     <- Gen.choose[Int](from, sequenceSize - 1)
      offset <- Gen.choose(to, sequenceSize + from)
    } yield Update(from, to, UnitaryAffineFunction(offset, true))

    Gen.listOf(Gen.frequency((1, genNonFlip), (1, genFlip)))
  }

  val genOperations: Gen[List[Operation]] = {
    val genSwap: Gen[Swap] = for {
      start1            <- Gen.choose[Int](0, sequenceSize - 2)
      end1              <- Gen.choose[Int](start1, sequenceSize - 2)
      end2              <- Gen.choose[Int](end1 + 1, sequenceSize - 1)
      firstSecondOrBest <- Gen.choose[Int](0, 2)
      flip              <- Gen.prob(0.5)
    } yield Swap(start1, end1, end2, firstSecondOrBest, flip)

    val genFlip: Gen[Flip] = for {
      start <- Gen.choose[Int](0, sequenceSize - 1)
      end   <- Gen.choose[Int](start, sequenceSize - 1)
    } yield Flip(start, end)
    Gen.listOf(Gen.frequency((1, genSwap), (1, genFlip)))
  }

  test(s"PiecewiseSequenceShiftingBijection : Batch bijections updates has expected result") {
    forAll(genBijections, minSuccessful(100)) { updates: List[Update] =>
      whenever(updates.size >= 20) {
        var piecewiseSequenceShiftingBijection: PiecewiseUnitaryAffineFunction =
          PiecewiseUnitaryAffineFunction.identity
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

  test(s"PiecewiseSequenceShiftingBijection : Batch swap and flip has expected result") {
    forAll(genOperations, minSuccessful(100)) { operations: List[Operation] =>
      whenever(operations.size >= 20) {
        var piecewiseSequenceShiftingBijection: PiecewiseUnitaryAffineFunction =
          PiecewiseUnitaryAffineFunction.identity
        var witnessList: List[Int] = List.tabulate(sequenceSize)(identity)

        operations.foreach {
          case Swap(start1, end1, end2, firstSecondOrBest, flip) =>
            val witnessZone1 =
              if (flip && firstSecondOrBest == 1) witnessList.slice(start1, end1 + 1).reverse
              else witnessList.slice(start1, end1 + 1)
            val witnessZone2 =
              if (flip && firstSecondOrBest == 0) witnessList.slice(end1 + 1, end2 + 1).reverse
              else witnessList.slice(end1 + 1, end2 + 1)
            witnessList =
              witnessList.take(start1) ++ witnessZone2 ++ witnessZone1 ++ witnessList.drop(end2 + 1)

            firstSecondOrBest match {
              case 0 =>
                piecewiseSequenceShiftingBijection = piecewiseSequenceShiftingBijection
                  .swapAdjacentZonesShiftFirst(start1, end1, end2, flip)
              case 1 =>
                piecewiseSequenceShiftingBijection = piecewiseSequenceShiftingBijection
                  .swapAdjacentZonesShiftSecond(start1, end1, end2, flip)
              case 2 =>
                piecewiseSequenceShiftingBijection = piecewiseSequenceShiftingBijection
                  .swapAdjacentZonesShiftBest(start1, end1, end2)
            }
          case Flip(start, end) =>
            piecewiseSequenceShiftingBijection =
              piecewiseSequenceShiftingBijection.flipPivotsInInterval(start, end)
            witnessList =
              witnessList.take(start) ++ witnessList.slice(start, end + 1).reverse ++ witnessList
                .drop(end + 1)
        }
        for (i <- 0 until sequenceSize) {
          piecewiseSequenceShiftingBijection(i) should be(witnessList(i))
        }
      }
    }
  }

}

case class Update(from: Int, to: Int, bijection: UnitaryAffineFunction)

abstract class Operation
case class Swap(start1: Int, end1: Int, end2: Int, firstSecondOrBest: Int, flip: Boolean)
    extends Operation
case class Flip(start: Int, end: Int) extends Operation

sealed abstract class PiecewiseSequenceShiftingBijectionNaive {
  def apply(value: Int): Int
  def updateBefore(
    fromIncluded: Int,
    toIncluded: Int,
    update: UnitaryAffineFunction
  ): PiecewiseSequenceShiftingBijectionNaive =
    UpdatedPiecewiseLinearFunNaive(
      fromIncluded,
      toIncluded,
      update: UnitaryAffineFunction,
      this
    )
}
case object IdentityNaive extends PiecewiseSequenceShiftingBijectionNaive {
  override def apply(value: Int): Int = value
}
case class UpdatedPiecewiseLinearFunNaive(
                                           fromIncluded: Int,
                                           toIncluded: Int,
                                           update: UnitaryAffineFunction,
                                           base: PiecewiseSequenceShiftingBijectionNaive
) extends PiecewiseSequenceShiftingBijectionNaive {
  override def apply(value: Int): Int =
    if (value >= fromIncluded && value <= toIncluded) base(update(value)) else base(value)
}
