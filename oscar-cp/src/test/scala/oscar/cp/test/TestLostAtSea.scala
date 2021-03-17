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

import oscar.cp.constraints._
import oscar.cp._
import oscar.cp.testUtils._
import oscar.cp.core.CPPropagStrength


class TestLostAtSea extends TestSuite {



  val proba = Array(Array(3, 0, 0, 3, 2, 4, 2, 3),
    Array(3, 3, 3, 1, 2, 4, 1, 4),
    Array(0, 4, 0, 1, 2, 3, 4, 0),
    Array(1, 1, 0, 3, 4, 1, 1, 0),
    Array(1, 1, 3, 3, 1, 2, 2, 4),
    Array(0, 2, 3, 3, 3, 0, 2, 4),
    Array(2, 3, 2, 4, 2, 4, 1, 1),
    Array(2, 1, 2, 2, 2, 4, 1, 3))
  def getLineCol(i: Int) = (i / 8, i % 8)

  def neighbors(i: Int) = {
    val (l, c) = getLineCol(i)
    def toInt(lc: (Int, Int)) = lc._1 * 8 + lc._2
    Set((l + 1, c), (l - 1, c), (l, c + 1), (l, c - 1)).filter { case (l, c) => (l >= 0 && l < 8 && c >= 0 && c < 8) }.map(toInt(_))
  }

  test("Table Model") {
    // set of valid transitions pair
    val tuples = (for (i <- 0 until 64; j <- neighbors(i)) yield (i, j)).toSet
    
    val cp = CPSolver()
    cp.silent = true
    var best = 1000

    val path = Array.fill(10)(CPIntVar(0 until 64)(cp))

    val sol = Array.fill(10)(0)
    val prob = proba.flatten
    
    for (i <- 0 until 9) {
      cp.add(table(path(i), path(i + 1), tuples)) // for each consecutive visits, give the possible valid transitions
    }
    cp.add(allDifferent(path), Strong) // each visit must be different
    val obj = sum(0 until 10)(i => element(prob, path(i)))
    cp.maximize(obj)
    cp.search {
      binaryFirstFail(path)
    } onSolution {
      (0 until 10).foreach(i => sol(i) = path(i).value) // record the solution
      best = obj.value
    }
    cp.start
    best should be(33)
  }

  test("Circuit Model") {

    def testCircuit(cons: CPPropagStrength): Int = {

      val cp = CPSolver()
      cp.silent = true
      val succ = Array.tabulate(64)(i => CPIntVar(neighbors(i))(cp))

      val path = Array.fill(10)(CPIntVar(0 until 64)(cp)) // represent the path of length ten which is the solution

      val sol = Array.fill(10)(0)

      var best = 1000
      val prob = proba.flatten

      for (i <- 0 until 9) {
        cp.add(elementVar(succ, path(i), path(i + 1)),cons)
      }
      cp.add(circuit(succ), cons)

      val obj = sum(0 until 10)(i => element(prob, path(i)))
      cp.maximize(obj)
      cp.search {
        binaryFirstFail(path)
      } onSolution {
        (0 until 10).foreach(i => sol(i) = path(i).value) // record the solution
        best = obj.value
      }
      cp.start()
      best

    }

    assert(testCircuit(CPPropagStrength.Weak) == 33)
    assert(testCircuit(CPPropagStrength.Medium) == 33)
    assert(testCircuit(CPPropagStrength.Strong) == 33)

  } 

}
