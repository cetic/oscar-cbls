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
package oscar.des.engine

import scala.collection.mutable._

/**
 * This is the main engine of the simulation.
 * Every Process in the simulation should wait, require resource ... on an instance of this class.
 * @author pschaus
 */
class Model {
   
	private val eventQueue = new PriorityQueue[SimEvent]()
	private var currentTime = 0.0
	
	def clock() : Double = currentTime
	
	private def addEvent(e : SimEvent) = eventQueue += e
	
	def simulate(horizon: Int,verbose: Boolean = true): Unit = {
		while (eventQueue.nonEmpty) {
			val e = eventQueue.dequeue()
			if(verbose && e.time <= horizon && e.time != currentTime){
				println("-----------> time: "+  e.time)
			}
			currentTime = e.time;
			if(currentTime <= horizon){
				e.process
			}
			else {
				currentTime = horizon;
				return
			}
		}
	}

	def wait(duration : Double)(block : => Unit): Unit = {
		assert(duration >= 0)
		addEvent(new WaitEvent(clock + duration, block))
	}
	
    def wait(duration : Int)(block : => Unit): Unit = {
		wait(duration.toDouble)(block)
	}
	
	def request(r : Resource)(block : => Unit): Unit = {
		r.request(block)
	}

	def release(r : Resource): Unit = {
		r.release()
	}
	
	def suspend(proc : Process)(block : => Unit): Unit = {
		proc.suspend(block)
	}

	def resume(proc : Process): Unit ={
		proc.resume()
	}

}