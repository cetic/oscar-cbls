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
import oscar.cp.testUtils.TestSuite

import oscar.cp.constraints._

import oscar.cp._

/**
 * Testing of Count constraint
 * @author: Pierre Schaus pschaus@gmail.com
 */
class TestCount extends TestSuite {

  val rand = new scala.util.Random(0)

  def countDecomp(cp: CPSolver, N: CPIntVar, X: Array[CPIntVar], Y: CPIntVar) = {
    cp.add(sum(X.map(_ ?=== Y)) === N)
  }

  def nbSol(nmin: Int, nmax: Int, domx: Array[Set[Int]], domy: Set[Int], decomp: Boolean = false): (Int, Int) = {
    var nbSol = 0
    val cp = CPSolver()

    val N = CPIntVar(nmin to nmax)(cp)
    val X = Array.tabulate(domx.size)(i => CPIntVar(domx(i))(cp))
    val Y = CPIntVar(domy)(cp)
    //println(domx.mkString(",")+" domY:"+domy)

    if (decomp) countDecomp(cp, N, X, Y)
    else {
      //countDecomp(cp,N,X,Y)
      cp.add(new Count(N, X, Y))
    }
    cp.search {
      binaryStatic(X :+ Y)
    } onSolution {
      X.count(_.value == Y.value) should equal(N.value)
      nbSol += 1
    }
    val stat = cp.start()
    (nbSol, stat.nFails)
  }

  def randomDom = Array.fill(10)(rand.nextInt(10)).toSet

  test("count1") {
    val cp = CPSolver()

    val N = CPIntVar(0 to 3)(cp)
    val X = Array.fill(5)(CPIntVar(0 to 5)(cp))
    val Y = CPIntVar(1 to 3)(cp)

    cp.add(new Count(N, X, Y))
    cp.search {
      binaryStatic(X :+ Y)
    } onSolution {
      X.count(_.value == Y.value) should equal(N.value)
    }
    cp.start
  }

  test("count2") {

    for (i <- 0 to 5) {

      val X = Array.fill(6)(randomDom)
      val Y = randomDom
      var (nsol1, nsol2, nsol3) = (0, 0, 0)
      var (bkt1, bkt2, bkt3) = (0, 0, 0)
      val t1 = oscar.util.time {
        val (a, b) = nbSol(4, 5, X, Y, false);
        nsol1 = a
        bkt1 = b
      }
      val t2 = oscar.util.time {
        val (a, b) = nbSol(4, 5, X, Y, true);
        nsol2 = a
        bkt2 = b
      }
      nsol1 should equal(nsol2)
    }

  }

}
