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
/**
 * @author Jannou Brohée
 * @author renaud.delandtsheer@cetic.be
 */

package oscar.cbls.business.routing.model

import oscar.cbls.algo.seq.IntSequence

object VehicleLocation {
  def apply(v:Int,nodeToPosition:Int=>Int): ConcreteVehicleLocation = new ConcreteVehicleLocation(Array.tabulate(v)(nodeToPosition))
  def apply(startPositionOfVehicleThatIWillNeverModify:Array[Int]) = new ConcreteVehicleLocation(startPositionOfVehicleThatIWillNeverModify)
}

/**
 * @author Jannou Brohée
 * @author renaud.delandtsheer@cetic.be
 */
abstract class VehicleLocation(val v : Int, val level:Int){

  def checkOnSequence(s:IntSequence): Unit ={
    for(vehicle <- 0 until v){
      require(s.positionOfAnyOccurrence(vehicle).get == startPosOfVehicle(vehicle),vehicle)
    }
  }

  def regularize:ConcreteVehicleLocation = VehicleLocation(v,startPosOfVehicle)

  /**
   *
   * @param oldPosToNewPos a description of the move through a function that maps old position to new position for all values in the sequence.
   * @param maxStackLevel is the maximal level of stack before a regularization automatically takes place. by default it is set to 2L*v
   * @return a vehicle location reflecting the start position of vehicle after the move is performed
   */
  def push(oldPosToNewPos:(Int)=> Option[Int], maxStackLevel:Int = 2*v): VehicleLocation = {
    val tmp = new StackedVehicleLocation(oldPosToNewPos,this)
    if(tmp.level > maxStackLevel) tmp.regularize
    else tmp
  }

  def startPosOfVehicle(vehicle:Int):Int

  def vehicleReachingPosition(position:Int): Int ={
    var upperVehicle:Int = v -1
    var upperVehiclePosition = startPosOfVehicle(upperVehicle)
    if(position>=upperVehiclePosition) return upperVehicle
    var lowerVehicle = 0
    var lowerVehiclePosition = 0
    while(lowerVehicle + 1 < upperVehicle){
      val midVehicle = (lowerVehicle + upperVehicle) /2
      val midVehiclePosition = startPosOfVehicle(midVehicle)
      if(midVehiclePosition == position){
        return midVehicle
      }
      if(midVehiclePosition <= position){
        lowerVehicle = midVehicle
        lowerVehiclePosition = midVehiclePosition
      }else{
        upperVehicle = midVehicle
        upperVehiclePosition = midVehiclePosition
      }
    }
    lowerVehicle
  }
}

/**
 * @author Jannou Brohée
 * @author renaud.delandtsheer@cetic.be
 */
class ConcreteVehicleLocation(private val startPositionOfVehicle:Array[Int]) extends VehicleLocation(startPositionOfVehicle.length, 0){

  override def regularize: ConcreteVehicleLocation = this

  def startPosOfVehicle(vehicle: Int): Int = startPositionOfVehicle(vehicle)

  override def toString: String = s"ConcreteVehicleLocation(${startPositionOfVehicle.mkString(",")})"
}

/**
 * @author Jannou Brohée
 * @author renaud.delandtsheer@cetic.be
 */
class StackedVehicleLocation(val oldPosToNewPos:Int=> Option[Int], val prev: VehicleLocation) extends VehicleLocation(prev.v, prev.level + 1){

  def startPosOfVehicle(vehicle: Int): Int = oldPosToNewPos(prev.startPosOfVehicle(vehicle)).get

  override def toString: String = s"StackedVehicleLocation([${(0 until v).map(startPosOfVehicle).mkString(",")}] depth:$level prev:$prev)"
}
