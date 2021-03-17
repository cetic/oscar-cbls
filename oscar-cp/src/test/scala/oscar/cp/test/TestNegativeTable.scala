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

import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.tables.{TableUtil, NegativeTableAlgo, TableDecompNeg}
import oscar.cp.testUtils._

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */
class TestNegativeTable extends TestSuite {

  private val rand = new scala.util.Random(16)

  private def randomTuples(dim: Int, n: Int, minValue: Int, maxValue: Int) = {
    Array.fill(n, dim)(rand.nextInt(maxValue - minValue) + minValue)
  }

    // Create the unit tests
    for (i <- 1 to 10) {

      val tuples1 = TableUtil.removeDuplicate(randomTuples(3, 100, 3, 8))
      val tuples2 = TableUtil.removeDuplicate(randomTuples(3, 100, 2, 7))
      val tuples3 = TableUtil.removeDuplicate(randomTuples(3, 100, 1, 6))

      for (algo <- NegativeTableAlgo.values) {
        test("negativeTable : Test random tables " + i + " (" + algo.toString + ")") {
          testTable(Array(tuples1, tuples2, tuples3), algo)
        }
      }
    }

  private def testTable(tables: Array[Array[Array[Int]]], algo: NegativeTableAlgo.Value): Unit = {

    implicit val solver = CPSolver()
    val x = Array.fill(5)(CPIntVar(1 to 8))

    solver.add(allDifferent(x))
    solver.search(binaryFirstFailIdx(x, i => x(i).max))

    val statRef = solver.startSubjectTo() {
      val cons = Seq(
        new TableDecompNeg(Array(x(0), x(1), x(2)), tables(0)),
        new TableDecompNeg(Array(x(2), x(3), x(4)), tables(1)),
        new TableDecompNeg(Array(x(0), x(2), x(4)), tables(2))
      )
      solver.add(cons)
    }

    val stat = solver.startSubjectTo() {
      val cons = Seq(
        negativeTable(Array(x(0), x(1), x(2)), tables(0), algo),
        negativeTable(Array(x(2), x(3), x(4)), tables(1), algo),
        negativeTable(Array(x(0), x(2), x(4)), tables(2), algo)
      )
      solver.add(cons)
    }

    assert(stat.nSols == statRef.nSols)


    assert(stat.nSols == statRef.nSols)
    assert(stat.nFails == statRef.nFails)
  }

  for (algo <- NegativeTableAlgo.values) {

    test("negativeTable : test 1 " + algo) {
      val cp = CPSolver()
      var x = Array.fill(3)(CPIntVar(1 to 2)(cp))

      val tuples = Array(Array(1, 1, 1), Array(1, 1, 2), Array(1, 2, 1), Array(1, 2, 2))

      cp.post(negativeTable(Array(x(0), x(1), x(2)), tuples, algo))

      x(0).isBound should be(true)
      x(0).value should be(2)
      x(1).isBound should be(false)
      x(2).isBound should be(false)
    }

    test("negativeTable : test 2 " + algo) {
      val cp = CPSolver()

      val x = CPIntVar(0 to 4)(cp)
      val y = CPIntVar(0 to 4)(cp)

      val tuples = (for (i <- 0 until 5; j <- 0 until 4) yield Array(i, j)).toArray
      cp.post(negativeTable(Array(x, y), tuples, algo))

      x.isBound should be(false)
      y.isBound should be(true)
      y.value should be(4)
      intercept[Inconsistency] {
                cp.post(y !== 4)
              }
    }

    test("negativeTable : test 3 " + algo) {
      val cp = CPSolver()
      val x = Array.fill(3)(CPIntVar(1 to 7)(cp))
      val tuples = Array(Array(1, 1, 1), Array(1, 2, 3), Array(1, 2, 7), Array(2, 1, 4))
      var nbSol = 0
      cp.add(negativeTable(x, tuples, algo))
      cp.search {
        binaryStatic(x)
      } onSolution {
        nbSol += 1
      }
      cp.start
      nbSol should be(339)
    }

    test("negativeTable : test 4 " + algo) {
      val cp = CPSolver()
      val x = Array.fill(2)(CPIntVar(1 to 1)(cp))

      val tuples = Array(Array(1, 1))

      postAndCheckFailure(cp, negativeTable(x, tuples, algo))
      cp.isFailed should be(true)
    }

    test("negativeTable : test 5 " + algo) {

      def nbSol(newcons: Boolean) = {
        val cp = CPSolver()
        val x = Array.fill(4)(CPIntVar(Set(1, 3, 6, 9))(cp))

        val tuples = Array(Array(1, 2, 2, 4),
          Array(1, 2, 4, 8),
          Array(1, 1, 9, 6),
          Array(1, 1, 8, 6),
          Array(3, 1, 6, 9),
          Array(1, 9, 3, 1),
          Array(1, 9, 9, 9),
          Array(3, 6, 6, 6))

        val cons = if (newcons) negativeTable(x, tuples, algo) else new TableDecompNeg(x, tuples)
        cp.post(cons)
        var nbSol = 0
        cp.search {
          binaryFirstFail(x)
        } onSolution {
          nbSol += 1
        }
        cp.start
        nbSol
      }
      nbSol(false) should be(nbSol(true))

    }

    test("negativeTable : test 6 " + algo) {
      implicit val cp = CPSolver()
      var x = Array.fill(6)(CPIntVar(2 to 3)(cp))
      var nbSol = 0

      val tuples = Array(
        Array(2, 2, 3, 2, 2, 3),
        Array(2, 3, 2, 2, 3, 2)
      )


      cp.post(negativeTable(x, tuples, algo))
      cp.search(binaryStatic(x))
      cp.onSolution {
        //println(x.mkString(", "))
        nbSol += 1
      }
      cp.start()
      nbSol should be(62)

    }

    test("negativeTable : test 7 " + algo) {
      implicit val cp = CPSolver()
      var x = Array.fill(6)(CPIntVar(0 to 5)(cp))
      var nbSol = 0

      val tuples = Array(
        Array(5, 5, 0, 5, 1, 4),
        Array(5, 5, 0, 3, 1, 1),
        Array(3, 1, 4, 2, 3, 1)
      )

      cp.post(negativeTable(x, tuples, algo))
      cp.search(binaryStatic(x))
      cp.onSolution {
        //println(x.mkString(", "))
        nbSol += 1
      }
      cp.start()
      nbSol should be(46653)
    }

    test("negativeTable : test 8 " + algo) {
      implicit val cp = CPSolver()
      var x = Array.fill(7)(CPIntVar(0 to 3)(cp))
      var nbSol = 0

      val tuples = Array(
        Array(1, 0, 3, 1, 0, 2, 1),
        Array(1, 3, 3, 3, 2, 1, 3),
        Array(1, 0, 3, 0, 0, 3, 1)
      )

      cp.post(negativeTable(x, tuples, algo))
      cp.search(binaryStatic(x))
      cp.onSolution {
        nbSol += 1
      }
      cp.start()
      nbSol should be(16381)
    }

    test("negativeTable : test 9 (empty table) " + algo) {
      implicit val cp = CPSolver()
      val x = Array.fill(3)(CPIntVar(0 to 3)(cp))
      val tuples: Array[Array[Int]] = Array()

      val cst = negativeTable(x, tuples, algo)
      cp.post(cst)

      cst.isActive should be(false)

    }

    test("negativeTable : test 10 (becoming empty table) " + algo) {
      implicit val cp = CPSolver()
      val x = Array.fill(3)(CPIntVar(0 to 3)(cp))
      val tuples: Array[Array[Int]] = Array(Array(0, 1, 2))

      cp.post(x(0) !== 0)
      val cst = negativeTable(x, tuples, algo)
      cp.post(cst)
      cst.isActive should be(false)

    }

    test("negativeTable : test 11 " + algo) {
      implicit val cp = CPSolver()
      val x = Array.fill(3)(CPIntVar(0 to 3)(cp))
      val tuples: Array[Array[Int]] = (for (i <- 0 to 3; j <- 0 to 3; k <- 0 to 3) yield Array(i, j, k)).toArray

      intercept[Inconsistency] {
            cp.post(negativeTable(x, tuples, algo))
          }
    }

  }

}

