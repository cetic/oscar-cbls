/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.test

import oscar.cp._
import oscar.cp.testUtils._

class TestOr extends TestSuite {

  test("or1") {
    val cp = CPSolver()
    val A = Array.fill(3)(CPBoolVar()(cp))
    val B = CPBoolVar()(cp)
    cp.add(or(A, B))
    cp.add(A(0) === 1)

    B.isBoundTo(1) should be(true)
  }

  test("or2") {
    val cp = CPSolver()
    val A = Array.fill(3)(CPBoolVar()(cp))
    val B = CPBoolVar()(cp)
    cp.add(A(0) === 1)
    cp.add(or(A, B))
    B.isBoundTo(1) should be(true)
  }

  test("or3") {
    val cp = CPSolver()
    val A = Array.fill(3)(CPBoolVar()(cp))
    A.foreach(x => cp.add(x === 0))
    val B = CPBoolVar()(cp)
    cp.add(or(A, B))
    B.isBoundTo(0) should be(true)
  }

  test("or4") {
    val cp = CPSolver()
    val A = Array.fill(3)(CPBoolVar()(cp))
    val B = CPBoolVar()(cp)
    cp.add(or(A, B))
    A.foreach(x => cp.add(x === 0))
    B.isBoundTo(0) should be(true)
  }

  test("or5") {
    val cp = CPSolver()
    val A = Array.fill(3)(CPBoolVar()(cp))
    val B = CPBoolVar()(cp)
    cp.add(B === 1)
    cp.add(A(0) === 0)
    cp.add(A(1) === 0)

    cp.add(or(A, B))

    A(2).isBoundTo(1) should be(true)
  }

  test("or6") {
    val cp = CPSolver()
    val A = Array.fill(3)(CPBoolVar()(cp))
    val B = CPBoolVar()(cp)

    cp.add(or(A, B))

    cp.add(B === 1)
    cp.add(A(0) === 0)
    cp.add(A(1) === 0)

    A(2).isBoundTo(1) should be(true)
  }

  test("or7") {
    val cp = CPSolver()
    val A = Array.fill(3)(CPBoolVar()(cp))
    val B = CPBoolVar()(cp)

    cp.add(or(A, B))
    cp.add(B === 0)

    A.forall(_.isBoundTo(0)) should be(true)
  }

  test("or8") {
    val cp = CPSolver()
    val A = Array.fill(3)(CPBoolVar()(cp))
    val B = CPBoolVar()(cp)
    cp.add(B === 0)
    cp.add(or(A, B))

    A.forall(_.isBoundTo(0)) should be(true)
  }

  test("or9") {
    val cp = CPSolver()
    val A = Array.fill(4)(CPBoolVar()(cp))
    cp.add(or(A))
    cp.add(A(0) === 0)
    cp.add(A(3) === 0)
    A(2).isBound should be(false)
    cp.add(A(1) === 0)
    A(2).isBoundTo(1) should be(true)
  }

  test("or10") {
    val cp = CPSolver()
    val A = Array.fill(4)(CPBoolVar()(cp))

    cp.add(or(A))

    cp.pushState()

    cp.add(A(3) === 0)
    cp.add(A(2) === 0)
    A(0).isBound should be(false)
    cp.add(A(1) === 0)
    A(0).isBoundTo(1) should be(true)

    cp.pop()

    cp.add(A(0) === 0)
    cp.add(A(3) === 0)
    A(2).isBound should be(false)
    cp.add(A(1) === 0)
    A(2).isBoundTo(1) should be(true)
  }

  test("or11") {
    val cp = CPSolver()
    val A = Array.fill(4)(CPBoolVar()(cp))

    cp.add(or(A))

    cp.pushState()

    cp.post(A(3) === 0)
    cp.post(A(2) === 0)
    A(0).isBound should be(false)
    cp.post(A(1) === 0)
    A(0).isBoundTo(1) should be(true)

    cp.pop()

    cp.post(A(0) === 0)
    cp.post(A(3) === 0)
    A(2).isBound should be(false)
    var exception = false
    try {
      cp.add(Seq(A(1) === 0, A(2) === 0))
    } catch {
      case e: NoSolutionException => exception = true
    }

    exception should be(true)
  }

  test("or12") {

    val rand = new scala.util.Random()

    for (i <- 0 until 200) {
      implicit val cp = CPSolver()
      cp.deactivateNoSolExceptions()

      val A = Array.fill(10)(CPBoolVar())

      def randClause(): Iterable[CPBoolVar] = {
        val rset = (for (i <- 0 until A.size / 3) yield rand.nextInt(10)).toSet
        return (for (i <- rset) yield A(i))
      }

      val randClauses = for (i <- 0 until 10) yield randClause().toArray

      for (i <- 0 until 3) {
        val varId = rand.nextInt(A.length)
        val value = rand.nextInt(2)
        cp.add(A(varId) === value)
      }

      def myOr(decomp: Boolean): Unit = {
        for (c: Array[CPBoolVar] <- randClauses) {
          if (decomp) add(sum(c) >= 1)
          else add(or(c.toArray))
        }
      }

      search(binaryFirstFail(A, _.randomValue))

      val stat1 = startSubjectTo() { myOr(false) }
      val stat2 = startSubjectTo() { myOr(true) }
      stat1.nSols should be(stat2.nSols)
      stat1.nFails should be(stat2.nFails)
    }
  }

  test("binary implication 1") {

    val cp = CPSolver()
    val x = CPBoolVar()(cp)
    val y = CPBoolVar()(cp)
    val clause = Array(!x, y)
    cp.add(or(clause))

    x shouldContain 0
    x shouldContain 1
    y shouldContain 0
    y shouldContain 1
  }

  test("binary implication 2") {

    val cp = CPSolver()
    val x = CPBoolVar()(cp)
    val y = CPBoolVar()(cp)
    val clause = Array(!x, y)

    cp.add(or(clause))
    cp.add(x === 0)

    x shouldBeAssignedTo 0
    y shouldContain 0
    y shouldContain 1
  }

  test("binary implication 3") {

    val cp = CPSolver()
    val x = CPBoolVar()(cp)
    val y = CPBoolVar()(cp)
    val clause = Array(!x, y)

    cp.add(or(clause))
    cp.add(y === 1)

    x shouldContain 0
    x shouldContain 1
    y shouldBeAssignedTo 1
  }

  test("binary implication 4") {

    val cp = CPSolver()
    val x = CPBoolVar()(cp)
    val y = CPBoolVar()(cp)
    val clause = Array(!x, y)

    cp.add(or(clause))
    cp.add(x === 1)

    x shouldBeAssignedTo 1
    y shouldBeAssignedTo 1 // here
  }

  test("binary implication 5") {

    val cp = CPSolver()
    val x = CPBoolVar()(cp)
    val y = CPBoolVar()(cp)
    val clause = Array(!x, y)

    cp.add(or(clause))
    cp.add(y === 0)

    x shouldBeAssignedTo 0
    y shouldBeAssignedTo 0
  }

}
