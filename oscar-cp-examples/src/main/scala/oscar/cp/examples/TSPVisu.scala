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
  *******************************************************************************/

package oscar.cp.examples

import oscar.cp._
import oscar.util._

/**
  * Traveling Salesman Problem with Visualization
  *
  * Given a distance matrix between 20 cities,
  * find the shortest tour visiting each city exactly once.
  *
  * @author Pierre Schaus  pschaus@gmail.com
  * @author Renaud Hartert ren.hartert@gmail.com
  */
object TSPVisu extends CPModel with App {

  // Data
  val nCities = 40
  val Cities = 0 until nCities
  val (distMatrix, coordinates) = TSPGenerator.randomInstance(nCities)

  // Variables
  val succ = Array.fill(nCities)(CPIntVar(Cities))
  val totDist = CPIntVar(0 to distMatrix.flatten.sum)
  add(sum(Cities)(i => distMatrix(i)(succ(i))) === totDist)

  // Constraints
  add(minCircuit(succ, distMatrix, totDist), Weak)

  // Search heuristic
  minimize(totDist)

  search {
    // Select the not yet bound city with the smallest number of possible successors
    selectMin(Cities)(!succ(_).isBound)(succ(_).size) match {
      case None => noAlternative
      case Some(x) => {
        // Select the closest successors of the city x
        val v = selectMin(Cities)(succ(x).hasValue(_))(distMatrix(x)(_)).get
        branch(add(succ(x) === v))(add(succ(x) !== v))
      }
    }
  }

  // Visual Component
  val visual = new VisualTSP(coordinates, succ)

  var nSols = 0
  onSolution {
    nSols += 1
    visual.updateTour(nSols, totDist.value)
  }

  println(start())
}

/** Generates a random TSP instance */
object TSPGenerator {
  def randomInstance(nCities: Int, seed: Int = 0): (Array[Array[Int]], Array[(Int, Int)]) = {
    val rand = new scala.util.Random(seed)
    val coord = Array.tabulate(nCities)(i => (100 + rand.nextInt(400), rand.nextInt(400)))
    val distMatrix = Array.tabulate(nCities, nCities)((i, j) => getDist(coord(i), coord(j)))
    (distMatrix, coord)
  }

  def getDist(p1: (Int, Int), p2: (Int, Int)): Int = {
    val dx = p2._1 - p1._1
    val dy = p2._2 - p1._2
    math.sqrt(dx * dx + dy * dy).toInt
  }
}

/** Visualization for TSP */
class VisualTSP(coordinates: Array[(Int, Int)], succ: Array[CPIntVar]) {

  import oscar.visual._
  import oscar.visual.plot.PlotLine

  val Cities = 0 until coordinates.size
  val frame = VisualFrame("TSP")

  // Creates the plot and place it into the frame
  val plot = new PlotLine("", "Solution number", "Distance")
  frame.createFrame("TSP Objective Function").add(plot)

  // Creates the visualization of the tour and place it into the frame
  val tour = VisualTour(coordinates)
  frame.createFrame("TSP Tour").add(tour)
  frame.pack()

  // Updates the visualization
  def updateTour(nSol: Int, dist: Int): Unit = {
    Cities.foreach(i => tour.edgeDest(i, succ(i).value))
    tour.repaint()
    plot.addPoint(nSol, dist)
  }
}
