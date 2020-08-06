/*****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */
package oscar.cbls.algo.fun

import oscar.cbls.algo.quick.QList

import scala.annotation.tailrec

object PiecewiseLinearBijectionNaive{
  def identity:PiecewiseLinearBijectionNaive = new PiecewiseLinearBijectionNaive(PiecewiseLinearFun.identity)

  @tailrec
  def computeInvertedPivots(prevPivot : Pivot,
                            remainingPivots : List[Pivot],
                            newPivots : QList[Pivot] = null): QList[Pivot] = {
    remainingPivots match {
      case Nil => newPivots
      case p1 :: p2 :: tail =>
        val fun = p1.f
        val invert = fun.invert
        val newPivot = new PivotWithTo(fun(if (fun.minus) p2.fromValue - 1 else p1.fromValue), invert, fun(if (fun.minus) p1.fromValue else p2.fromValue - 1))
        computeInvertedPivots(p1, p2 :: tail, QList(newPivot, newPivots))
      case p1 :: _ =>
        val fun = p1.f
        require(!fun.minus)
        val invert = fun.invert
        QList(new PivotWithTo(fun(p1.fromValue), invert, fun(if (fun.minus) Int.MinValue else Int.MaxValue)), newPivots)
    }
  }

  def apply(forward:PiecewiseLinearFun) = new PiecewiseLinearBijectionNaive(forward)
}

class PiecewiseLinearBijectionNaive(val forward:PiecewiseLinearFun, givenBackward:PiecewiseLinearFun = null) {
  lazy val backward : PiecewiseLinearFun = {
    if (givenBackward != null) givenBackward
    else PiecewiseLinearFun.createFromPivots(PiecewiseLinearBijectionNaive.computeInvertedPivots(null, forward.pivots, null))
  }

  def invert : PiecewiseLinearBijectionNaive = new PiecewiseLinearBijectionNaive(backward, forward)

  def checkBijection(): Unit = {
    var pivots = backward.pivots
    while (true) {
      pivots match {
        case p1 :: p2 :: tail =>
          require(p1.asInstanceOf[PivotWithTo].toValue + 1 == p2.fromValue, "not a bijection; p1:" + p1 + " p2:" + p2)
          pivots = p2 :: tail
        case _ => return
      }
    }
  }

  override def toString : String = {
    "Bijection.forward: " + forward + "\n" +
      "Bijection.backward:" + backward + "\n"
  }

  def updateBefore(updates : (Int, Int, LinearTransform)*) : PiecewiseLinearBijectionNaive = {
    new PiecewiseLinearBijectionNaive(forward.updateForCompositionBefore(updates : _*))
  }

  def swapAdjacentZonesShiftFirst(startZone1Included : Int,
                                  endZone1Included : Int,
                                  endZone2Included : Int,
                                  flipZone2 : Boolean) : PiecewiseLinearBijectionNaive = {
    new PiecewiseLinearBijectionNaive(forward.swapAdjacentZonesShiftFirst(startZone1Included, endZone1Included, endZone2Included, flipZone2))
  }

  def swapAdjacentZonesShiftSecond(startZone1Included:Int,
                                   endZone1Included:Int,
                                   endZone2Included:Int,
                                   flipZone1:Boolean):PiecewiseLinearBijectionNaive = {
    new PiecewiseLinearBijectionNaive(forward.swapAdjacentZonesShiftSecond(startZone1Included, endZone1Included, endZone2Included, flipZone1))
  }

  def swapAdjacentZonesShiftBest(startZone1Included : Int,
                                 endZone1Included : Int,
                                 endZone2Included : Int): PiecewiseLinearBijectionNaive = {
    new PiecewiseLinearBijectionNaive(forward.swapAdjacentZonesShiftBest(startZone1Included, endZone1Included, endZone2Included))
  }

  def apply(value:Int): Int = forward(value)

  def unApply(value:Int): Int = backward(value)

   def flipInInterval(startZoneIncluded : Int,
                      endZoneIncluded :Int):PiecewiseLinearBijectionNaive =
     new PiecewiseLinearBijectionNaive(forward.flipFunctionInInterval(startZoneIncluded, endZoneIncluded))
}

class PivotWithTo(fromValue:Int,f:LinearTransform, val toValue:Int) extends Pivot(fromValue,f) {
  override def toString: String =
    s"PivotWithTo(from:$fromValue toValue:$toValue $f)"
}
