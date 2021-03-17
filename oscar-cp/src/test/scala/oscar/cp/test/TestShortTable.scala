/** *****************************************************************************
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
  * *****************************************************************************/
package oscar.cp.test

import oscar.cp._
import oscar.cp.constraints.tables.{ShortTableAlgo, TableDecomp, TableUtil, shortTable}
import oscar.cp.testUtils._

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */
class TestShortTable extends TestSuite {

  private val rand = new scala.util.Random(16)
  private val star = -1

  private def randomTuples(dim: Int, n: Int, minValue: Int, maxValue: Int, star: Int, d: Int) = {
    Array.fill(n, dim)(if (rand.nextInt(100) < d) star else rand.nextInt(maxValue - minValue) + minValue)
  }

  // Create the unit tests
  for (i <- 1 to 100) {

    val tuples1 = randomTuples(3, 100, 3, 8, star, 2)
    val tuples2 = randomTuples(3, 100, 2, 7, star, 5)
    val tuples3 = randomTuples(3, 100, 1, 6, star, 1)

    for (algo <- ShortTableAlgo.values) {
      test("shortTable : Test random " + i + " (" + algo.toString + ")") {
        testTable(Array(tuples1, tuples2, tuples3), algo)
      }
    }
  }

  private def testTable(tables: Array[Array[Array[Int]]], algo: ShortTableAlgo.Value): Unit = {

    implicit val solver = CPSolver()
    val x = Array.fill(5)(CPIntVar(1 to 8))

    solver.add(allDifferent(x))
    solver.search(binaryFirstFailIdx(x, i => x(i).max))

    val subX0 = Array(x(0), x(1), x(2))
    val subX1 = Array(x(2), x(3), x(4))
    val subX2 = Array(x(0), x(2), x(4))

    val statRef = solver.startSubjectTo() {
      val ground0 = TableUtil.decompressToGroundTable(subX0,tables(0),star)
      val ground1 = TableUtil.decompressToGroundTable(subX1,tables(1),star)
      val ground2 = TableUtil.decompressToGroundTable(subX2,tables(2),star)
      val cons = Seq(
        new TableDecomp(subX0, ground0),
        new TableDecomp(subX1, ground1),
        new TableDecomp(subX2, ground2)
      )
      solver.add(cons)
    }

    val stat = solver.startSubjectTo() {
      val cons = Seq(
        shortTable(subX0, tables(0),star, algo),
        shortTable(subX1, tables(1),star, algo),
        shortTable(subX2, tables(2),star, algo)
      )
      solver.add(cons)
    }

    if (stat.nSols != statRef.nSols) {
      println(algo + " " + stat.nSols + " " + statRef.nSols)
      tables(0).foreach(a => println(a.mkString(",")))
      println("")
    }

    assert(stat.nSols == statRef.nSols)
    assert(stat.nFails == statRef.nFails)
  }


  for (algo <- ShortTableAlgo.values) {

    test("shortTable : test 1 " + algo) {
      val cp = CPSolver()
      var x = Array.fill(3)(CPIntVar(1 to 3)(cp))

      val tuples = Array(Array(star, 1, 1), Array(1, star, 1))

      cp.post(shortTable(Array(x(0), x(1), x(2)), tuples,star, algo))

      x(2).isBound should be(true)
      x(2).value should be(1)

      cp.post(x(1) !== 1)

      cp.isFailed should be(false)
      x(0).isBound should be(true)
      x(0).value should be(1)
      x(1).isBound should be(false)

      cp.post(x(1) === 2)
      cp.isFailed should be(false)
      x(1).isBound should be(true)
      x(1).value should be(2)

    }

    test("shortTable : test 2 " + algo) {
      val cp = CPSolver()
      var x = Array.fill(3)(CPIntVar(1 to 7)(cp))
      val tuples = Array(Array(1, 1, star), Array(1, star, 3), Array(star, 2, 7), Array(star, 1, 4))
      var nbSol = 0
      cp.add(shortTable(x, tuples,star, algo))
      cp.search {
        binaryStatic(x)
      } onSolution {
        nbSol += 1
      }
      cp.start
      nbSol should be(26)
    }

    test("shortTable : test 3 " + algo) {
      val cp = CPSolver()
      val x = Array.fill(3)(CPIntVar(1 to 1)(cp))

      val tuples = Array(Array(1, 2,star), Array(2, 1,star))

      postAndCheckFailure(cp, shortTable(x, tuples,star, algo))
      cp.isFailed should be(true)
    }

  }

}

