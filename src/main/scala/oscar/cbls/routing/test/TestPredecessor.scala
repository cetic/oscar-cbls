/*******************************************************************************
  * This file is part of OscaR (Scala in OR).
  *
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/gpl-3.0.html
  ******************************************************************************/

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.routing.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.routing.model._
import oscar.cbls.routing.initialSolution.RandomNeighbor
import scala.language.reflectiveCalls

class TestPredecessor extends FunSuite with ShouldMatchers {

  def fixture(n:Int,v:Int) =
    new {
      val UNROUTED = n
      val ROUTE_ARRAY_UNROUTED = v
      val V:Int = v
      val N:Int = n
      // model with check internal
      val model: Model = new Model(false,None,false,false)
      val vrp = new VRP(N, V, model) with ObjectiveFunction with HopDistance with PositionInRouteAndRouteNr
        with Unrouted with Predecessors
      model.close()

      RandomNeighbor(vrp)
      model.propagate()
    }

  test("check internal of 50 points and 1 vehicle"){
    fixture(50,1)
  }

  test("check internal of 50 points and 2 vehicle"){
    fixture(50,2)
  }

  test("check internal of 50 points and 5 vehicle"){
    fixture(50,5)
  }

  test("check internal of 100 points and 1 vehicle"){
    fixture(100,1)
  }

  test("check internal of 100 points and 2 vehicle"){
    fixture(100,2)
  }

  test("check internal of 100 points and 5 vehicle"){
    fixture(100,5)
  }

  test("check internal of 500 points and 1 vehicle"){
    fixture(500,1)
  }

  test("check internal of 500 points and 2 vehicle"){
    fixture(500,2)
  }

  test("check internal of 500 points and 5 vehicle"){
    fixture(500,5)
  }




}