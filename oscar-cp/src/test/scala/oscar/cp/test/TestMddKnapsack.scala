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

import oscar.cp._
import oscar.cp.constraints._
import oscar.cp.constraints.mdd.MddKnapsack
import oscar.cp.testUtils.TestSuite

/**
 * Testing of Count constraint
 * @author: Pierre Schaus pschaus@gmail.com, Romain et Helene
 */
class TestMddKnapsack extends TestSuite {

  //val rand = new scala.util.Random(0)

  //def randomDom = Array.fill(10)(rand.nextInt(10)).toSet

  test("test MDD knapsack 1") {
    val cp = CPSolver()

    val x = Array.fill(5)(CPBoolVar()(cp))

    val w = Array(60,4,3,8,2)

    val loads = Set(60,4)

    cp.add(MddKnapsack(x,w,loads))

    assert(!x(0).isBound)
    assert(!x(1).isBound)
    assert(x(2).isFalse)
    assert(x(3).isFalse)
    assert(x(4).isFalse)

  }


  test("test MDD knapsack 2") {
    val cp = CPSolver()

    val x = Array.fill(5)(CPBoolVar()(cp))

    val w = Array(6,4,3,7,2)

    val loads = Set(10)

    cp.add(MddKnapsack(x,w,loads))


    for (i <- 0 until 4) {
      assert(!x(i).isBound)
    }
    assert(x(4).isFalse)

  }

  test("test MDD knapsack 3") {
    val cp = CPSolver()

    val x = Array.fill(5)(CPBoolVar()(cp))

    val w = Array(6,4,3,7,2)

    val loads = Set(10)

    cp.add(MddKnapsack(x,w,loads))

    cp.add(x(0) !== 1)

    assert(x(0).isFalse)
    assert(x(1).isFalse)
    assert(x(2).isTrue)
    assert(x(3).isTrue)
    assert(x(4).isFalse)

  }


  for (i <- 0 until 100) {
    test(s"test MDD knapsack random $i") {
      val cp = CPSolver()

      val rand = new scala.util.Random(i)
      val weights = Array.fill(10)(rand.nextInt(10))

      val x = Array.fill(10)((CPBoolVar()(cp)))
      val loads = Set(35, 40, 45, 52, 55, 57)

      cp.search(binaryStatic(x))

      val stat1 = cp.startSubjectTo() {
        cp.add(MddKnapsack(x, weights, loads))
      }

      val stat2 = cp.startSubjectTo() {
        cp.add(binaryKnapsack(x, weights, CPIntVar(loads)(cp)))
      }

      assert(stat1.nSols == stat2.nSols)
      assert(stat1.nNodes <= stat2.nNodes)
      assert((stat1.nFails-stat1.nSols).abs <= 1) // dom consistency

    }
  }


}
