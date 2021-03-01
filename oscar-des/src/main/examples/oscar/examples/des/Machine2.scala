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
 * Two machines can be broken, there is only one repair person that can fix it at a time,
 * so one of the machines must wait if the two machines are broken at the same time
 *  @author pschaus
 */
class Machine2(m : Model, name: String, val repairPerson: UnaryResource) extends Process(m,name) {
  
  val liveDur = new scala.util.Random()
  val repairDur = new scala.util.Random()
  
  def beAlive(): Unit = {
    println(name+" is alive")
    val aliveDur = 5+liveDur.nextInt(20)
    m.wait (aliveDur) {
      beBroken()
    }
  }
  
  def beBroken(): Unit = {
    println(name+" is broken waiting to be repaired at time "+m.clock())
    
    m.request(repairPerson) {
      beRepaired()
    }
  }
  
  def beRepaired(): Unit = {
    val brokenDur = 2+repairDur.nextInt(5)
    println(name+" reparation starts at time "+m.clock()+" duration of reparation="+brokenDur)
    m.wait(brokenDur) {
      println(name+" releases the machine at time "+m.clock())
      m.release(repairPerson)
      beAlive()
    }
  }   
  
  def run(): Unit = {
    beAlive()
  }
  
}

object Machine2 {
  def main(args: Array[String]): Unit = {
    val mod = new Model()
    val repairPerson = new UnaryResource(mod)
    val m1 = new Machine2(mod,"machine1",repairPerson)
    m1.run()
    val m2 = new Machine2(mod,"machine2",repairPerson)
    m2.run()
    mod.simulate(1000,true)
  }
}