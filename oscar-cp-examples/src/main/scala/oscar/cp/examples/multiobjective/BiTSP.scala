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
package oscar.cp.examples.multiobjective

import oscar.cp._
import oscar.cp.constraints.Inverse
import oscar.cp.multiobjective.Pareto
import scala.collection.mutable.Queue
import oscar.cp.constraints.MinAssignment
import oscar.cp.multiobjective.ListPareto
import oscar.cp.multiobjective.visual._
import oscar.cp.examples.util.reader.TSPUtils

object BiTSP extends App {
  
  // Parsing
  val nObjs = 2
  val Objs = 0 until nObjs
  val coord1 = TSPUtils.parseCoordinates("data/TSP/renA10.tsp")
  val coord2 = TSPUtils.parseCoordinates("data/TSP/renB10.tsp")
  val distMatrix1 = TSPUtils.buildDistMatrix(coord1)
  val distMatrix2 = TSPUtils.buildDistMatrix(coord2)
  val distMatrices = Array(distMatrix1, distMatrix2)
  val nCities = distMatrix1.size
  val Cities = 0 until nCities
  
  val visu = new PlotPareto()
  
  // Model
  // -----
  implicit val cp = new CPSolver()
  cp.silent = true
  
  // Successors & Predecessors
  val succ = Array.fill(nCities)(CPIntVar(Cities)(cp))
  val pred = Array.fill(nCities)(CPIntVar(Cities)(cp))

  // Total distance
  val totDists = Array.tabulate(nObjs)(o => CPIntVar(0 to distMatrices(o).flatten.sum)(cp))

  // Constraints
  // -----------
  cp.paretoMinimize(totDists: _*) 
  for (o <- Objs) {
    cp.add(minCircuit(succ, distMatrices(o), totDists(o)), Strong)
  }
  search {
    binaryFirstFail(succ)
  }  
  
  // Run
  // ---  
  println("Search...")
  println(cp.start())
 
  println("Pareto Set")
  println(cp.nonDominatedSolutionsObjs.mkString("\n"))
}
