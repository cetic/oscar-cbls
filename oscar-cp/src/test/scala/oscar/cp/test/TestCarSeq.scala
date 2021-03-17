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
package oscar.cp.test

import org.scalatest.FunSuite
import oscar.cp._
import collection.immutable.SortedSet
import oscar.algo.reversible.SparseSet
import org.scalatest.Matchers

class TestCarSeq extends FunSuite with Matchers {

  test("Car Sequencing") {

    //   -----------------  data -----------------------
    val nbCars = 10
    val nbConfigs = 6
    val nbOptions = 5
    val lb = Array(1, 2, 1, 2, 1)
    val ub = Array(2, 3, 3, 5, 5)
    val demand = Array(1, 1, 2, 2, 2, 2) // demand for each config
    val requires = Array(Array(1, 0, 1, 1, 0), // nbConfigs x nbOptions
      Array(0, 0, 0, 1, 0),
      Array(0, 1, 0, 0, 1),
      Array(0, 1, 0, 1, 0),
      Array(1, 0, 1, 0, 0),
      Array(1, 1, 0, 0, 0));
    val options = Array.fill(nbOptions)(new SparseSet(0, nbConfigs, true))
    for (o <- 0 until nbOptions; c <- 0 until nbConfigs; if (requires(c)(o) == 1)) {
      options(o).insert(c);

    }
    //   -----------------  model -----------------------

    val cp = CPSolver()

    val line = Array.fill(nbCars)(CPIntVar(0, nbConfigs - 1)(cp))

    var nbSol = 0

    for (o <- 1 until nbOptions) {
      cp.add(new oscar.cp.constraints.Sequence(line, options(o), ub(o), 0, lb(o)))
    }
    cp.add(new oscar.cp.constraints.GCC(line, 0, Array.fill(nbConfigs)(0), demand));
    cp.search {
      binaryFirstFail(line)
    } onSolution {
      nbSol += 1
    }
    cp.start
    nbSol should be(860)

  }

}
