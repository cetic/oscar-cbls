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
package oscar.examples.cbls.old.scheduling
/*
import oscar.cbls._
import oscar.cbls.business.scheduling.model.Activity
import oscar.cbls.business.scheduling.model.Planning
import oscar.cbls.business.scheduling.model.VariableResources
import oscar.cbls.business.scheduling.solver.IFlatIRelax

/**
 * A simple model of a steelworks.
 * 
 * Steelers have five tasks to achieve:
 * - iron making (needs 2 steelers)
 * - steel making (needs 3 steelers)
 * - casting (needs 1 steeler)
 * - roughing rolling (needs 2 steelers)
 * - product rolling (needs 2 steelers)
 * 
 * There is five steelers, with different work hours :
 * - from 5AM to 12AM (1 steeler)
 * - from 8AM to 5PM, with one hour to eat between 12AM and 14PM (3 steelers)
 * - from 2PM to 9PM (2 steelers)
 * 
 * Iron making and then steel making must be achieved in the same day.
 * Roughing rolling and then product rolling, must be achieved in the same day.
 */
object Steelworks extends App {
  val model = new Store(noCycle=false)

  val planning = new Planning(model, 24) with VariableResources
  
  val hours = Array(0, 0, 0, 0, // from 1 to 4 AM
                    1, 1, 1, 4, // from 5 to 8 AM
                    4, 4, 4, 3, // from 9 to 12 AM
                    1, 5, 5, 5, // from 1 to 4 PM
                    2, 2, 2, 2, // from 5 to 8 PM
                    2, 0, 0, 0) // from 9 to 12 PM
  val steelers = planning.postVariableResource(hours, "Steelers")

  val ironMaking = Activity(5, planning, "iron making")
  ironMaking uses 2 ofResource steelers
  
  val steelMaking = Activity(3, planning, "steel making")
  steelMaking uses 3 ofResource steelers
  
  val casting = Activity(3, planning, "casting")
  casting uses 1 ofResource steelers
  
  val roughingRolling = Activity(4, planning, "roughingRolling")
  roughingRolling uses 2 ofResource steelers
  
  val productRolling = Activity(8, planning, "productRolling")
  productRolling uses 2 ofResource steelers
  
  ironMaking precedes steelMaking
  roughingRolling precedes productRolling

  planning.close()
  model.close(false)
  
  val solver = new IFlatIRelax(planning)
  //println(model.dumpToDot(true, true))
  solver.solve(15, 10)

  println(planning.toAsciiArt)
  println(planning.resourceUsage)
  println(planning.dependencies)

}

*/