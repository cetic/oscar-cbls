// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.test.lib.neighborhoods.combinator

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.search.Move
import oscar.cbls.lib.invariant.routing.TotalRouteLength
import oscar.cbls.lib.neighborhoods.combinator.DoOnMove
import oscar.cbls.lib.neighborhoods.routing.TwoOpt
import oscar.cbls.modeling.routing.VRP

class DoOnMoveTests extends AnyFunSuite with Matchers {

  private def generateManhattanDistMatrix(points: Array[(Int, Int)]): Array[Array[Long]] = {
    def dist(x1: (Int, Int), x2: (Int, Int)): Long = (x1._1 - x2._1).abs + (x1._2 - x2._2).abs

    Array.tabulate(points.length, points.length)((i: Int, j: Int) => dist(points(i), points(j)))
  }

  private val points: Array[(Int, Int)] =
    Array(
      (0, 0),
      (0, -10),
      (-2, 4),
      (-2, 6),
      (2, 6),
      (2, 4),
      (2, -14),
      (2, -16),
      (-2, -16),
      (-2, -14)
    )

  private val distMatrix: Array[Array[Long]] = generateManhattanDistMatrix(points)

  test("DoOnMove works as expected") {
    val model  = new Store()
    val vrp    = VRP(model, 10, 2, debug = true)
    val output = TotalRouteLength(vrp, distMatrix).routeLength
    val obj    = Minimize(output)
    model.close()
    vrp.routes := IntSequence(List(0, 4, 3, 5, 2, 1, 9, 6, 8, 7))
    model.propagate()

    val routedNodeExceptVehicle = vrp.routedWithoutVehicles.pendingValue

    val relevantStartSegment = () => routedNodeExceptVehicle

    var seqBefore = vrp.routes.pendingValue

    val procBeforeMove = (_: Move) =>
      require(
        seqBefore.equals(vrp.routes.pendingValue),
        s"Seq before move $seqBefore is not the same as routes.pending value ${vrp.routes.pendingValue}"
      )
    val procAfterMove = (_: Move) => seqBefore = vrp.routes.pendingValue

    val twoOpt = TwoOpt(vrp, relevantStartSegment)
    val search = DoOnMove(twoOpt, procBeforeMove, procAfterMove)

    noException mustBe thrownBy(search.doAllMoves(obj))
  }
}
