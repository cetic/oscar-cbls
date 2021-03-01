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
package oscar.examples.des


import oscar.des.engine._


/**
 * Two machines can be broken, they are two repair person to fix it so it can be done in parallel
 * @author pschaus
 */
class Machine1(m : Model, name: String) extends Process(m,name) {
  
  val liveDur = new scala.util.Random()
  val breakDur = new scala.util.Random()
  
  def beAlive(): Unit = {
    println(name+" is alive")
    val aliveDur = 5+liveDur.nextInt(5)
    m.wait (aliveDur.toDouble) {
      beBroken()
    }
  }
  
  def beBroken(): Unit = {
    println(name+" is broken at time "+m.clock())
    val repairDur = 5+breakDur.nextInt(10)
    m.wait(repairDur.toDouble) {
      beAlive()
    }
  }
  
  def run(): Unit = {
    beAlive()
  }
  
}

object Machine1 {
  def main(args: Array[String]): Unit = {
    val mod = new Model()
    val m1 = new Machine1(mod,"machine1")
    m1.run()
    val m2 = new Machine1(mod,"machine2")
    m2.run()
    println("simulate---")
    mod.simulate(100,true);
  }
}
