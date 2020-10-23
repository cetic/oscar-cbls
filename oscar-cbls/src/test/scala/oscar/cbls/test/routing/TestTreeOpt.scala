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

package oscar.cbls.test.routing

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.business.routing.neighborhood.ThreeOpt
import oscar.cbls.core.computation.Store
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{Best, First}
import oscar.cbls.util.StopWatch

object  TestTreeOpt extends App {

  val n = 7
  val v = 3
  val model = Store()

  //Vector(0 -> 0, 1 -> 3 -> 4 -> 5 -> 6 -> 1, 2 -> 2)
  val myVRP = new VRP(model,n,v)
  val obj:Objective = CBLSIntVar(model,0)
  model.close()

  myVRP.setCircuit(List(0, 1, 3, 4, 5 ,6 ,2))

  println(myVRP)

  val threeOpt =
    ThreeOpt(
      potentialInsertionPoints = myVRP.routed,
      relevantNeighbors = () => n => myVRP.routed.value,
      vrp = myVRP,
      neighborhoodName = "Three Opt",
      tryFlip = false
    )

  threeOpt.verbose = 4

  threeOpt.getMove(obj,0)


}
