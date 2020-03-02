package oscar.cbls.business.routing.model

/*******************************************************************************
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
  ******************************************************************************/


import oscar.cbls._

abstract class TravelTimeFunction {
  def getTravelDuration(from: Int, leaveTime: Int, to: Int): Int
  def getBackwardTravelDuration(from: Int, arrivalTime: Int, to: Int): Int

  def getMinMaxTravelDuration(from: Int, to: Int): (Int, Int) =
    (getMinTravelDuration(from, to), getMaxTravelDuration(from, to))

  def getMinTravelDuration(from: Int, to: Int): Int
  def getMaxTravelDuration(from: Int, to:Int): Int
}

/**
  * Stores a square matrix of primitive TTF
  *
  * @param nodeCount the number of nodes to consider; continuous ranges starting at zero
  * @param defaultTTF if we do not specify a TTF for a node, this is the value considered
  * @author renaud.delandtsheer@cetic.be
  */
class TTFMatrix(nodeCount: Int, defaultTTF: PrimitiveTravelTimeFunction) extends TravelTimeFunction {

  private val matrix: Array[Array[PrimitiveTravelTimeFunction]] = Array.fill(nodeCount, nodeCount)(defaultTTF)

  def setTTF(from: Int, to: Int, ttf: PrimitiveTravelTimeFunction) {
    matrix(from)(to) = ttf
  }

  def getTTF(from: Int, to: Int): PrimitiveTravelTimeFunction =
    matrix(from)(to)

  override def getTravelDuration(from: Int, leaveTime: Int, to: Int): Int =
    matrix(from)(to).getTravelDuration(leaveTime)

  override def getBackwardTravelDuration(from: Int, arrivalTime: Int, to: Int): Int =
    matrix(from)(to).getBackwardTravelDuration(arrivalTime)

  override def getMinTravelDuration(from: Int, to: Int): Int =
    matrix(from)(to).getMinTravelDuration

  override def getMaxTravelDuration(from: Int, to: Int): Int =
    matrix(from)(to).getMaxTravelDuration

  override def getMinMaxTravelDuration(from: Int, to: Int): (Int, Int) =
    matrix(from)(to).getMinMaxTravelDuration
}

/**
  * This stores a single TTF of a travel binding two nodes
  * @author renaud.delandtsheer@cetic.be
  */
abstract class PrimitiveTravelTimeFunction {

  /**
    * the duration to perform the travel at leave time "éleaveTime
    *
    * @param leaveTime when the travel begins
    * @return when the travel ends
    */
  def getTravelDuration(leaveTime: Int): Int

  /**
    * the duration of he travel if you plan to arive at the given time
    *
    * @param arrivalTime
    * @return the latest start to begin the travel
    */
  def getBackwardTravelDuration(arrivalTime: Int): Int

  def getMinMaxTravelDuration: (Int, Int) =
    (getMinTravelDuration, getMaxTravelDuration)

  def getMinTravelDuration: Int
  def getMaxTravelDuration: Int

}

/**
  * A TTF that is constant
  * this is similar to using a TTFHistogram with a single slot, but this class is lighter
  * @param travelDuration the duration of the travel
  * @author renaud.delandtsheer@cetic.be
  */
class TTFConst(travelDuration: Int) extends PrimitiveTravelTimeFunction {

  override def toString() = {
    "TTFConst(" + travelDuration + ")"
  }

  override def getTravelDuration(leaveTime: Int): Int = travelDuration

  override def getMinTravelDuration: Int = travelDuration

  override def getMaxTravelDuration: Int = travelDuration

  override def getBackwardTravelDuration(arrivalTime: Int): Int = travelDuration
}

/**
  * represents a TTF using histograms
  * Notice that the representation is modulo, if asked for a time after the overallDuration,
  * it is assumed to start again at position zero in time
  *
  * @param nbSlots the number of slots in the histogram
  * @param overallDuration the duration of the whole TTF
  * @author renaud.delandtsheer@cetic.be
  * THIS IS EXPERIMENTAL
  */
class TTFHistogram(val nbSlots: Int, val overallDuration: Int) extends PrimitiveTravelTimeFunction {
  private val slotDuration: Int = overallDuration / nbSlots

  private val slots: Array[Int] = Array.fill(nbSlots)(0)
  private var nimMaxAccurate = false
  private var min: Int = 0
  private var max: Int = 0

  override def toString() = {
    "TTFHistogram(nbSlots " + nbSlots + ", " + { var strSlots = ""; for (slot <- slots) { strSlots += slot + "; " }; strSlots } + ")"
  }

  def setTravelDurationAtSlot(slotNumber: Int, duration: Int) {
    slots(slotNumber) = duration
    nimMaxAccurate = false
  }
  def getTravelDurationAtSlot(slotNumber: Int): Int = slots(rectifySlot(slotNumber))

  private def updateMinMax() {
    if (!nimMaxAccurate) {
      nimMaxAccurate = true
      min = Int.MaxValue
      max = Int.MinValue
      for (v <- slots) {
        if (v < min) min = v
        if (max < v) max = v
      }
    }
  }

  private def rectifySlot(slotNr: Int): Int = {
    var tmp: Int = slotNr % nbSlots
    if (tmp < 0) tmp += nbSlots
    tmp
  }

  override def getTravelDuration(leaveTime: Int): Int =
    getTravelDurationAtSlot(leaveTime / slotDuration)

  override def getMinTravelDuration: Int = {
    updateMinMax()
    min
  }

  override def getMaxTravelDuration: Int = {
    updateMinMax()
    max
  }

  override def getBackwardTravelDuration(arrivalTime: Int): Int = {
    if (nbSlots == 1) {
      slots(0)
    }
    var maxslot: Int = arrivalTime / slotDuration;
    var minslot: Int = 0
    while (minslot * slotDuration + getTravelDurationAtSlot(minslot) >= arrivalTime) {
      minslot -= nbSlots
    }

    while (true) {
      if (minslot == maxslot) {
        return slots(rectifySlot(minslot))
      } else if (minslot + 1 == maxslot) {
        if (maxslot * slotDuration + getTravelDurationAtSlot(maxslot) <= arrivalTime) {
          return getTravelDurationAtSlot(maxslot)
        } else {
          return getTravelDurationAtSlot(minslot)
        }
      }
      val medslot = (minslot + maxslot) / 2
      val medslotstart = medslot * slotDuration

      if (medslotstart + getTravelDurationAtSlot(medslot) <= arrivalTime) {
        minslot = medslot
      }
      if (medslotstart + slotDuration + getTravelDurationAtSlot(medslot) >= arrivalTime) {
        maxslot = medslot
      }
    }
    // Default return value
    0
  }
}

/**
  * Represents a TTF as a piecewise linear function
  *
  * Notice that the representation is modulo, if asked for a time after the last point,
  * it is assumed to start again at position zero in time,
  * so that linear interpolation might happen between the last point and the first point, shifted by overallDuration
  *
  * @param NbPoints the number of points to consider
  * @param overallDuration the duration to consider
  * @author renaud.delandtsheer@cetic.be
  */
class TTFSegments(val NbPoints: Int, val overallDuration: Int) extends PrimitiveTravelTimeFunction {

  private val pointX: Array[Int] = Array.fill(NbPoints)(0)
  private val pointY: Array[Int] = Array.fill(NbPoints)(0)

  private var nimMaxAccurate = false
  private var min: Int = 0
  private var max: Int = 0

  /**throws an error if the X is smaller than the predecessor's X, or if the slope is too steep*/
  def setPoint(pointNr: Int, pointX: Int, pointY: Int) {
    this.pointX(pointNr) = pointX
    this.pointY(pointNr) = pointY
    nimMaxAccurate = false
    if (pointNr != 0) {

      val firstId = pointNr - 1
      val secondId = pointNr

      val intervalX = this.pointX(secondId) - this.pointX(firstId)
      if (intervalX <= 0) throw new Error("TTF segments are going backward in time")

      val intervalY = this.pointY(secondId) - this.pointY(firstId)
      if (intervalX < -intervalY) throw new Error("slope is too steep")
    }
  }

  def getPointX(pointNr: Int): Int = getPoint(pointNr)._1
  def getPointY(pointNr: Int): Int = getPoint(pointNr)._2

  def getPoint(pointNr: Int): (Int, Int) = {
    var rectifiedPoint: Int = pointNr % NbPoints
    var shifting: Int = math.floor(pointNr / NbPoints).toInt * overallDuration
    while (rectifiedPoint < 0) {
      rectifiedPoint += NbPoints
      shifting -= overallDuration
    }

    (pointX(rectifiedPoint) + shifting, pointY(rectifiedPoint))
  }

  private def updateMinMax() {
    if (!nimMaxAccurate) {
      nimMaxAccurate = true
      min = Int.MaxValue
      max = Int.MinValue
      for (v <- pointY) {
        if (v < min) min = v
        if (max < v) max = v
      }
    }
  }

  /**
    *
    * @param x is a point in time
    * @return a point number
    */
  private def findLastPointBefore(x: Int): Int = {
    var up: Int = NbPoints - 1
    while (getPointX(up) < x) up = up + NbPoints
    var down: Int = -1
    while (getPointX(down) > x) down = down - NbPoints

    while (down + 1 < up) {
      val mid: Int = (up + down) / 2
      if (getPointX(mid) == x) {
        return mid
      } else if (getPointX(mid) < x) {
        down = mid
      } else {
        up = mid
      }
    }
    if (getPointX(up) <= x) up
    else down
  }

  override def getTravelDuration(leaveTime: Int): Int = {
    val pointBefore = findLastPointBefore(leaveTime)
    val pointAfter = pointBefore + 1

    linearInterpol(leaveTime,
      getPointX(pointBefore), getPointY(pointBefore),
      getPointX(pointAfter), getPointY(pointAfter)).toInt
  }

  @inline
  private def linearInterpol(X: Float, X1: Float, Y1: Float, X2: Float, Y2: Float): Float = {
    ((X - X1) * (Y2 - Y1)) / (X2 - X1) + Y1
  }

  override def getMinTravelDuration: Int = {
    updateMinMax()
    min
  }

  override def getMaxTravelDuration: Int = {
    updateMinMax()
    max
  }

  def findLastPointBeforeLeave(arrivalTime: Int): Int = {
    var up: Int = NbPoints - 1
    while (getPointX(up) + getPointY(up) < arrivalTime) up = up + NbPoints
    var down: Int = -1
    while (getPointX(down) + getPointX(down) > arrivalTime - overallDuration) down = down - NbPoints

    while (down + 1 < up) {
      val mid: Int = (up + down) / 2
      if (getPointX(mid) + getPointY(mid) == arrivalTime) {
        return mid
      } else if (getPointX(mid) + getPointY(mid) < arrivalTime) {
        down = mid
      } else {
        up = mid
      }
    }
    if (getPointX(up) + getPointY(up) <= arrivalTime) up
    else down
  }

  override def getBackwardTravelDuration(arrivalTime: Int): Int = {
    var pointBefore = findLastPointBeforeLeave(arrivalTime)
    while (getPointX(pointBefore + 1) + getPointY(pointBefore + 1) <= arrivalTime) {
      pointBefore += 1
    }

    assert(getPointX(pointBefore) + getPointY(pointBefore) <= arrivalTime)
    assert(arrivalTime <= getPointX(pointBefore + 1) + getPointY(pointBefore + 1))

    linearInterpolBackward(arrivalTime,
      getPointX(pointBefore), getPointY(pointBefore),
      getPointX(pointBefore + 1), getPointY(pointBefore + 1)).toInt
  }

  @inline
  private def linearInterpolBackward(Y: Float, X1: Float, Y1: Float, X2: Float, Y2: Float): Float = {
    if (Y1 == Y2) return Y1
    val p = (X1 - X2) / (Y1 - Y2)
    ((Y + p * Y1 - X1) / (p + 1.0)).toFloat
  }

  override def toString = ("TTFSegments(NbPoints: " + NbPoints + " overallDuration: " + overallDuration + " points: ["
    + ((0 until NbPoints) map (i => "(" + pointX(i) + ";" + pointY(i) + ")") mkString ",") + "])")

}

object TTFTest extends App {

  val t = new TTFSegments(7, 24 * 60)
  t.setPoint(0, 60 * 3, 10)
  t.setPoint(1, 60 * 6, 20)
  t.setPoint(2, 60 * 9, 30)
  t.setPoint(3, 60 * 12, 20)
  t.setPoint(4, 60 * 15, 30)
  t.setPoint(5, 60 * 18, 30)
  t.setPoint(6, 60 * 21, 10)

  println(t)

  println("leave\tarrive")
  for (i <- 0 to (24 * 60) by 30) {
    println(i + "\t" + t.getTravelDuration(i) + "\t" + t.getBackwardTravelDuration(t.getTravelDuration(i) + i))
  }
  println("min: " + t.getMinTravelDuration + " max: " + t.getMaxTravelDuration)
}
object TTFHistoTest extends App{
  val t = new TTFHistogram(7, 24*60)

  t.setTravelDurationAtSlot(0, 2*60)
  t.setTravelDurationAtSlot(1, 2*60)
  t.setTravelDurationAtSlot(2, 3*60)
  t.setTravelDurationAtSlot(3, 6*60)
  t.setTravelDurationAtSlot(4, 5*60)
  t.setTravelDurationAtSlot(5, 4*60)
  t.setTravelDurationAtSlot(6, 3*60)

  println(t)
  println("leave\tarrive")
  for(i <- 0 to (24 * 60) by 30){
    println(i + "\t" + t.getTravelDuration(i) + "\t" + t.getBackwardTravelDuration(t.getTravelDuration(i) + i))
  }
}
