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
 * two machines can be broken, there is only one repair person that can repair it at a time
 * but this person must wait that the two machines are broken to start repairing any of them
 *  @author pschaus
 */
class Machine3(m : Model, name: String, machineList : MachineList) extends Process(m,name) {
	
	val liveDur = new scala.util.Random(0)
	val repairDur = new scala.util.Random(0)
	val repairPerson = new UnaryResource(m)
	private var broken = false
	private var repairInProgress = false
	machineList + this //add this to the list of machines
	
	def isBroken : Boolean = broken
	
	def isRepairInProgress : Boolean =  repairInProgress 
	
	def beAlive(): Unit = {
		println(name+" is alive")
		broken = false
		repairInProgress = false
		m.wait (liveDur.nextInt(10).max(0)) {
			beBroken()
		}
	}
	
	def beBroken(): Unit = {
		println(name+" is broken waiting to be repaired")
		broken = true
		
		if (machineList.notAllBroken()) {
			m.suspend(this) {
				askToBeRepaired() //we wait because some of the machines are not yet broken
			}
		} else {
			//all machines are broken but some of them are in the process of being repaired
			// so we reactivate only those not currently being repaired
			machineList.notBeingRepaired().foreach(ma => if(ma != this) m.resume(ma))
			askToBeRepaired()
		}
	}
	
	def askToBeRepaired(): Unit = {
		println(name+" is asking to be repaired")
		m.request(repairPerson) { //ask for the repair person resource
			beRepaired()
		}
	}
	
	def beRepaired(): Unit = {
		println(name+" being repaired")
		m.wait(repairDur.nextInt(3).max(0)) {
			m.release(repairPerson)
			beAlive()
		}
	}		
	
	def run(): Unit = {
		beAlive()
	}
}

class MachineList{
	
	var machines : List[Machine3] = List()
	
	def +(m: Machine3): Unit = {
		machines = m :: machines
	}
	
	def notBeingRepaired() : List[Machine3] = {
		machines.filter(_.isRepairInProgress)
	}
	
	def notAllBroken() : Boolean = {
		machines.foldLeft(false)(_ || !_.isBroken) //at least one not broken
	}
}


object Machine3 {
	def main(args: Array[String]): Unit = {
  		val mod = new Model()
  		val mlist = new MachineList()
		val m1 = new Machine3(mod,"machine1",mlist)
		m1.run()
		val m2 = new Machine3(mod,"machine2",mlist)
		m2.run()
		mod.simulate(100,true);
  		println("done1")
	}
}