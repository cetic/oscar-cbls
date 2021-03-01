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
package oscar.cp.constraints

import oscar.algo.reversible._
import oscar.cp._

import scala.jdk.CollectionConverters._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar

/**
 * Implementation of Path Constraint decomposition as described in 
 * "Solving the Longest Simple Path Problem with Constraint-Based Techniques" by Quang Dung, Yves Deville (CPAIOR2012)
 * 
 * - succ[i] is the successor of node i (also place i inside the domain of succ[i] if you want to allow it not to be part of the path
 * - start is the index of the first node on the path
 * - end is the index of the last node on the path
 * - length is the length of the path (number edges)
 * 
 * Example: 
 * succ [1, 3, 2, 5, 4, 0], start = 0, end = 5, length = 3 represents the path 0 -> 1 -> 3 -> 5
 * Notice that nodes that do not belong to the path, have them-self as successor and that 
 * the successor of the last node of the path is the first node by convention
 * @author Pierre Schaus
 */
class Path(succ: Array[CPIntVar], start: CPIntVar, end: CPIntVar, length: CPIntVar) extends Constraint(succ(0).store, "Path") {

  override def associatedVars(): Iterable[CPVar] = succ ++ Array(start, end, length)

  // for each node, it's position in the path
  val y = Array.fill(succ.length)(CPIntVar(succ.indices)(s))

  override def setup(l: CPPropagStrength): Unit = {
    s.post(elementVar(y,start,0))
    s.post(elementVar(y,end,length))
    
    for (v <- succ.indices)
       s.post(elementVar(y, succ(v), y(v) + 1).when((succ(v) ?!== v) && (end ?!== v)))
    s.post(allDifferent(succ),l)
  }

}
