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

package oscar.cbls.test.lib.neighborhoods

import oscar.cbls.algo.sequence.IntSequence

import scala.annotation.tailrec
import scala.util.Random

private[neighborhoods] object ToolsForTestingNeighborhood {

  def generateRandomDomain(rng: Random): List[Long] = {
    val lowerBound = rng.between(-100L, 101L)
    // The global minimum of the objective function is reached when a & b have their smallest value
    // To avoid that the smallest value is always the first explored, we shuffle the domain
    rng.shuffle((lowerBound to lowerBound + 10L).toList)
  }

  def generateRandomValidRoute(n: Int, v: Int, rng: Random): IntSequence = {
    val nodes           = rng.shuffle((v until n).toList)
    val vehiclePosition = List.fill(v - 1)(rng.between(0, nodes.length)).sortWith(_ > _)

    @tailrec
    def insertVehicleNodes(
      routedNodes: List[Int],
      vehiclePositions: List[Int],
      currentVehicle: Int = v - 1
    ): List[Int] = {
      vehiclePositions match {
        case Nil => 0 :: routedNodes
        case h :: t =>
          insertVehicleNodes(
            routedNodes.take(h) ::: (currentVehicle :: routedNodes.drop(h)),
            t,
            currentVehicle - 1
          )
      }
    }

    IntSequence(insertVehicleNodes(nodes, vehiclePosition))
  }

}
