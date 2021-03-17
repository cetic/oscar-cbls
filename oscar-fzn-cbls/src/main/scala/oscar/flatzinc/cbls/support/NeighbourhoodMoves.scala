/*
 * *****************************************************************************
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

package oscar.flatzinc.cbls.support

import oscar.cbls.core.computation.CBLSIntVar

/**
  * @author Gustav Bjordal
  */
abstract class Move(val value:Int){
  def commit():Unit
  def getModified: Set[CBLSIntVar]
}
case class AssignMove(x: CBLSIntVar,k:Int,override val value:Int) extends Move(value){
  def commit(): Unit ={x := k}
  def getModified=Set(x)
  override def toString() = x + " assigned to " +k + " doms: "+x.domain + " resulting objective: " + value
}
case class AssignsMove(xk: List[(CBLSIntVar,Int)],override val  value:Int) extends Move(value){
  def commit(): Unit ={xk.foreach(x => x._1 := x._2)}
  def getModified = xk.map(_._1).toSet
  override def toString() = xk.foldLeft("")((acc,x) => acc+x._1+ " assigned to " +x._2 +"; ") + " resulting objective: " + value
}
case class SwapMove(x: CBLSIntVar,y:CBLSIntVar,override val value:Int) extends Move(value){
  def commit(): Unit ={x :=: y}
  def getModified=Set(x,y)
  override def toString() = x + " swapped with " +y + " resulting objective: " + value
}
case class ChainMoves(ms:Array[Move],override val value:Int) extends Move(value){
  def commit(): Unit ={ms.foreach(_.commit())}
  def getModified=ms.foldLeft(Set.empty[CBLSIntVar])((acc,m) => acc ++ m.getModified)
  override def toString() = "Chain of: " + ms.mkString("\t","\n\t","") + "."
}
case class NoMove(override val value:Int = Int.MaxValue) extends Move(value){
  def commit(): Unit ={}
  def getModified = Set.empty[CBLSIntVar]
  override def toString() = "No-Op"
}
case class BeforeMove(m: Move,act:()=>Unit) extends Move(m.value) {
  def commit(): Unit ={
    act()
    m.commit()
  }
  def getModified = m.getModified
  override def toString() = m.toString()
}