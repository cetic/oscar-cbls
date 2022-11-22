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
   * this is the default functin; it returns the travelDuration
   *
   * @param leaveTime when the travel begins
   * @return the duration of the travels
   */
  def apply(leaveTime:Long):Long = travelDuration(leaveTime)

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
  def earliestArrivalTime(leaveTime:Long):Long = leaveTime + travelDuration(leaveTime)

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
  def latestLeaveTime(arrivalTime:Long):Long = arrivalTime - backwardTravelDuration(arrivalTime)

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
  def requireFifoProperty():Unit
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

  override def requireFifoProperty():Unit = {}
}

/**
 * represents a TTF using histograms with staircase
 *
 * @param slotDuration the duration of a slot in the histogram
 * @param slots slots of the histogram. The first one is at time zero
 */
class TTFHistogramStaircase(slotDuration:Long, slots:Array[Long]) extends TravelTimeFunction {

  override def toString: String = {
    s"TTFHistogram(${ var strSlots = ""; for (slot <- slots) { strSlots += s"$slot; " }; strSlots })"
  }

  override def travelDuration(leaveTime: Long): Long = {
    val slotNr:Int = (leaveTime.toDouble / slotDuration.toDouble).toInt
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
        if (maxslot * slotDuration + slots(maxslot) <= arrivalTime) {
          return slots(maxslot)
        } else {
          return slots(minslot)
        }
      }else {
        val medslot = (minslot + maxslot) / 2
        val medslotstart = medslot * slotDuration

        if (medslotstart + slots(medslot) <= arrivalTime) {
          minslot = medslot
        }
        if (medslotstart + slotDuration + slots(medslot) >= arrivalTime) {
          maxslot = medslot
        }
      }
    }
    // Default return value
    0
  }

  /**
   * checks that the TTF enforces the FIFO property
   * the slope of the travelDuration function can never be < -1
   *
   * @return
   */
  override def requireFifoProperty(): Unit = require(false,"Staircase histograms have infinite slope at each step; they do not enforce FIFO property")
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
class TTFSegments(points:Array[(Long,Long)]) extends TravelTimeFunction {

  override def requireFifoProperty(): Unit = {
    for(i <- 0 until points.length-1){
      val j = i+1
      require((points(i)._2 - points(j)._2) <= (points(j)._1 - points(i)._1),
        s"TTF not FIFO compliant slope between $i ${points(i)} and $j ${points(j)} dx:${(points(j)._1 - points(i)._1)} dy:${(points(j)._2 - points(i)._2)}")
    }
  }

  val fwdFun = new PiecewiseAffineFunction(points)
  val bwdFun = new PiecewiseAffineFunction(  {
    val pivotPoints =  points.map(_._1)
    val firstPivotX = pivotPoints(0)
    val lastPivotX = pivotPoints.last
    val enlargedPivots = pivotPoints
    val pivotAndValue = enlargedPivots.map(x => {
      var start = x
      val dur = fwdFun(x)
      while(start+1 + fwdFun(start+1) == x + dur) start+=1
      (start,fwdFun(start))
    })
    println("pivotAndValue:" + pivotAndValue.mkString(","))
    pivotAndValue.map({case (start,dur) =>  (start+dur,dur)}).groupBy(_._1).map({case (x,pairs) => (x,pairs.map(_._2).min)}).toArray.sortBy(_._1)
  })

  override def minTravelDuration: Long = points.minBy(_._2)._2

  override def maxTravelDuration: Long = points.maxBy(_._2)._2

  override def travelDuration(leaveTime: Long): Long = fwdFun(leaveTime)

  override def backwardTravelDuration(arrivalTime: Long): Long = bwdFun(arrivalTime)

  override def toString: String = s"TTFSegments(NbPoints: ${points.length})"
}

object testPiecewiseAffineBijection extends App{
  val points:Array[(Long,Long)] = Array((1,55),(10,50),(56,100),(74,83),(100,200))
  //val f:PiecewiseAffineFunction = new PiecewiseAffineFunction(points)
  val f = new TTFSegments(points)

  f.requireFifoProperty()

  for(time <- 1 to 100){
    val forwardTravelTime = f(time)
    val arrivalTime = f.earliestArrivalTime(time)
    val backwardTravelTIme = f.backwardTravelDuration(arrivalTime)
    val leaveTime = f.latestLeaveTime(arrivalTime)
    println(s"time:$time forwardTravelTime:$forwardTravelTime arrivalTime:$arrivalTime backwardTravelTIme:$backwardTravelTIme latestLeaveTime:$leaveTime")
  }
}


class PiecewiseAffineFunction(points:Array[(Long,Long)]){

  println(toString)

  override def toString: String = "PiecewiseAffineFunction(" + points.mkString(",") +")"

  @inline
  private def linearInterpol(X: Double, X1: Double, Y1: Double, X2: Double, Y2: Double): Double = {
    ((X) * (Y2 - Y1)) / (X2 - X1) + Y1
  }

  def apply(x:Long):Long = {
    var down: Int = 0 //strictly below
    var up: Int = points.length - 1 //Strictly above

    if(x == points(down)._1) return points(down)._2
    if(x == points(up)._1) return points(up)._2

    while (true) {
      if (up == down) {
        return points(up)._2
      } else if (down + 1 == up) {
        return linearInterpol(x - points(down)._1, points(down)._1,points(down)._2,points(up)._1,points(up)._2).toLong
      }
      val medSlot = (up + down) / 2
      val medSlotStart = points(medSlot)._1

      if (medSlotStart <= x) {
        down = medSlot
      }else{
        up = medSlot
      }
    }
    ??? //never reached
  }
}
