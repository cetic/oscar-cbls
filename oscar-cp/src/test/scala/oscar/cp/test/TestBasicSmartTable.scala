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

import oscar.cp.constraints.tables._
import oscar.cp._
import oscar.cp.constraints.tables.basicSmartTable
import oscar.cp.testUtils._

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */
class TestBasicSmartTable extends TestSuite {

  private val rand = new scala.util.Random(16)

  private def randomTuples(dim: Int, n: Int, minValue: Int, maxValue: Int, d: Int): Array[Array[BasicSmartElement]] = {
    Array.fill(n, dim) {
      val v = rand.nextInt(100)
      if (v < d)
        Star()
      else if (v < 2 * d)
        NotEqual(rand.nextInt(maxValue - minValue) + minValue)
      else if (v < 3 * d)
        LessEq(rand.nextInt(maxValue - minValue) + minValue)
      else if (v < 4 * d)
        GreatEq(rand.nextInt(maxValue - minValue) + minValue)
      else
        Equal(rand.nextInt(maxValue - minValue) + minValue)
    }
  }

  private def randomTuplesWithSet(dim: Int, n: Int, minValue: Int, maxValue: Int, d: Int): Array[Array[BasicSmartElement]] = {
    Array.fill(n, dim) {
      val v = rand.nextInt(100)
      if (v < d)
        Star()
      else if (v < 2 * d)
        NotEqual(rand.nextInt(maxValue - minValue) + minValue)
      else if (v < 3 * d)
        LessEq(rand.nextInt(maxValue - minValue) + minValue)
      else if (v < 4 * d)
        GreatEq(rand.nextInt(maxValue - minValue) + minValue)
      else if (v < 5 * d) {
        val set: Set[Int] = Set()
        for (value <- minValue to maxValue; if rand.nextInt(100) < 25)
          set + value
        println(set.mkString(","))
        InSet(set)
      }
      else if (v < 6 * d) {
        val set: Set[Int] = Set()
        for (value <- minValue to maxValue; if rand.nextInt(100) < 25)
          set + value
        NotInSet(set)
      }
      else
        Equal(rand.nextInt(maxValue - minValue) + minValue)
    }
  }

  // Create the unit tests
  for (i <- 1 to 100) {

    val tuples1 = randomTuples(3, 100, 3, 8, 2)
    val tuples2 = randomTuples(3, 100, 2, 7, 5)
    val tuples3 = randomTuples(3, 100, 1, 6, 1)
    val tuples1set = randomTuplesWithSet(3, 100, 3, 8, 2)
    val tuples2set = randomTuplesWithSet(3, 100, 2, 7, 5)
    val tuples3set = randomTuplesWithSet(3, 100, 1, 6, 1)

    for (algo <- BasicSmartTableAlgo.values) {
      test("basicSmartTable : Test random " + i + " (" + algo.toString + ")") {
        testTable(Array(tuples1, tuples2, tuples3), algo)
      }
      test("basicSmartTable : Test random with set " + i + " (" + algo.toString + ")") {
        testTable(Array(tuples1set, tuples2set, tuples3set), algo)
      }
    }
  }

  private def testTable(tables: Array[Array[Array[BasicSmartElement]]], algo: BasicSmartTableAlgo.Value): Unit = {

    implicit val solver = CPSolver()
    val x = Array.fill(5)(CPIntVar(1 to 8))

    solver.add(allDifferent(x))
    solver.search(binaryFirstFailIdx(x, i => x(i).max))

    val subX0 = Array(x(0), x(1), x(2))
    val subX1 = Array(x(2), x(3), x(4))
    val subX2 = Array(x(0), x(2), x(4))

    val statRef = solver.startSubjectTo() {
      val ground0 = TableUtil.decompressToGroundTable(subX0, tables(0))
      val ground1 = TableUtil.decompressToGroundTable(subX1, tables(1))
      val ground2 = TableUtil.decompressToGroundTable(subX2, tables(2))
      val cons = Seq(
        new TableDecomp(subX0, ground0),
        new TableDecomp(subX1, ground1),
        new TableDecomp(subX2, ground2)
      )
      solver.add(cons)
    }

    val stat = solver.startSubjectTo() {
      val cons = Seq(
        basicSmartTable(subX0, tables(0), algo),
        basicSmartTable(subX1, tables(1), algo),
        basicSmartTable(subX2, tables(2), algo)
      )
      solver.add(cons)
    }

    if (stat.nSols != statRef.nSols) {
      println(s"$algo ${stat.nSols} ${statRef.nSols}")
      tables(0).foreach(a => println(a.mkString(",")))
      println("")
    }

    assert(stat.nSols == statRef.nSols)
    assert(stat.nFails == statRef.nFails)
  }

}

