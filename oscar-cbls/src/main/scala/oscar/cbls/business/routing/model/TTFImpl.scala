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
package oscar.cbls.business.routing.model

/**
  * This stores a single TTF of a travel binding two nodes
  * @author renaud.delandtsheer@cetic.be
  */
abstract class TravelTimeFunction {

  /**
    * the duration to perform the travel
    *
    * @param leaveTime when the travel begins
    * @return the duration of the travel
    */
  def travelDuration(leaveTime: Long): Long

  /**
   * the arrival time
   * @param leaveTime when the ravel starts
   * @return when the travel finishes
   */
  def arrivalTime(leaveTime:Long):Long = leaveTime + travelDuration(leaveTime)

  /**
    * the duration of the travel if you plan to arrive at the given time
    *
    * @param arrivalTime when the travel finishes
    * @return the duration of the travel
    */
  def backwardTravelDuration(arrivalTime: Long): Long

  /**
   * the latest time to leave in order to arrive at a ginven time
   * @param arrivalTime when the travel finishes
   * @return when the ravel starts
   */
  def leaveTime(arrivalTime:Long):Long = arrivalTime - backwardTravelDuration(arrivalTime)

  /**
   * @return the min travel duration
   */
  def minTravelDuration: Long

  /**
   * @return the max travel duration
   */
  def maxTravelDuration: Long

  /**
   * checks that the TTF enforces the FIFO property
   * the slope of the travelDuration function can never be < -1
   * @return
   */
  def checkFIFOProperty():Boolean
}

/**
  * A TTF that is constant
  * this is similar to using a TTFHistogram with a single slot, but this class is lighter
  * @param travelDuration the duration of the travel
  * @author renaud.delandtsheer@cetic.be
  */
class TTFConst(travelDuration: Long) extends TravelTimeFunction {

  override def toString: String = {
    s"TTFConst($travelDuration)"
  }

  override def travelDuration(leaveTime: Long): Long = travelDuration

  override def minTravelDuration: Long = travelDuration

  override def maxTravelDuration: Long = travelDuration

  override def backwardTravelDuration(arrivalTime: Long): Long = travelDuration

  override def checkFIFOProperty(): Boolean = true
}

/**
  * represents a TTF using histograms
  * Notice that the representation is modulo, if asked for a time after the overallDuration,
  * it is assumed to start again at position zero in time
  *
  * @param nbSlots the number of slots in the histogram
  * @param overallDuration the duration of the whole TTF
  * @author renaud.delandtsheer@cetic.be
  */
class TTFHistogramStaircase(slotDuration:Long, startTimeOfHistogram:Long, slots:Array[Long]) extends TravelTimeFunction {

  override def toString: String = {
    s"TTFHistogram(${ var strSlots = ""; for (slot <- slots) { strSlots += s"$slot; " }; strSlots })"
  }

  override def travelDuration(leaveTime: Long): Long = {
    val slotNr:Int = ((leaveTime - startTimeOfHistogram) / slotDuration).toInt
    slots(slotNr)
  }

  override val minTravelDuration: Long = slots.min

  override val maxTravelDuration: Long = slots.max

  override def backwardTravelDuration(arrivalTime: Long): Long = {
    if (slots.length == 1)
      return slots(0)

    var maxslot: Int = slots.length
    var minslot: Int = 0

    while (true) {
      if (minslot == maxslot) {
        return slots(minslot)
      } else if (minslot + 1 == maxslot) {
        if (startTimeOfHistogram + maxslot * slotDuration + slots(maxslot) <= arrivalTime) {
          return slots(maxslot)
        } else {
          return slots(minslot)
        }
      }
      val medslot = (minslot + maxslot) / 2
      val medslotstart = startTimeOfHistogram + medslot * slotDuration

      if (medslotstart + slots(medslot) <= arrivalTime) {
        minslot = medslot
      }
      if (medslotstart + slotDuration + slots(medslot) >= arrivalTime) {
        maxslot = medslot
      }
    }
    // Default return value
    0
  }

  override def checkFIFOProperty(): Boolean = false
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
class TTFSegments(points:Array[(Long,Long)], val overallDuration: Int) extends TravelTimeFunction {

  override def minTravelDuration: Long = points.minBy(_._2)._2

  override def maxTravelDuration: Long = points.maxBy(_._2)._2

  override def travelDuration(leaveTime: Int): Int = {
    val pointBefore = findLastPointBefore(leaveTime)
    val pointAfter = pointBefore + 1

    linearInterpol(leaveTime.toFloat,
      getPointX(pointBefore).toFloat, getPointY(pointBefore).toFloat,
      getPointX(pointAfter).toFloat, getPointY(pointAfter).toFloat).toInt
  }

  @inline
  private def linearInterpol(X: Float, X1: Float, Y1: Float, X2: Float, Y2: Float): Float = {
    ((X - X1) * (Y2 - Y1)) / (X2 - X1) + Y1
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

  override def backwardTravelDuration(arrivalTime: Int): Int = {
    var pointBefore = findLastPointBeforeLeave(arrivalTime)
    while (getPointX(pointBefore + 1) + getPointY(pointBefore + 1) <= arrivalTime) {
      pointBefore += 1
    }

    assert(getPointX(pointBefore) + getPointY(pointBefore) <= arrivalTime)
    assert(arrivalTime <= getPointX(pointBefore + 1) + getPointY(pointBefore + 1))

    linearInterpolBackward(arrivalTime.toFloat,
      getPointX(pointBefore).toFloat, getPointY(pointBefore).toFloat,
      getPointX(pointBefore + 1).toFloat, getPointY(pointBefore + 1).toFloat).toInt
  }

  @inline
  private def linearInterpolBackward(Y: Float, X1: Float, Y1: Float, X2: Float, Y2: Float): Float = {
    if (Y1 == Y2) return Y1
    val p = (X1 - X2) / (Y1 - Y2)
    ((Y + p * Y1 - X1) / (p + 1.0)).toFloat
  }

  override def toString: String = s"TTFSegments(NbPoints: $NbPoints overallDuration: $overallDuration points: [${(0 until NbPoints) map (i => "(" + pointX(i) + ";" + pointY(i) + ")") mkString ","}])"
}
