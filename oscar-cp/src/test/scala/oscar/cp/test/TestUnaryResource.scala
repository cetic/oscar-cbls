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

import org.scalatest.{FunSuite, Matchers}
import oscar.cp._
import oscar.cp.core.CPPropagStrength

/**
  * @author Pierre Schaus pschaus@gmail.com
  */
class TestUnaryResource extends FunSuite with Matchers {

  // decomp without resource variables
  def decomp(cp: CPSolver, starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar]): Unit = {
    val n = starts.length
    for (i <- 0 until n; j <- i + 1 until n) {
      cp.add((ends(i) ?<= starts(j)) || (ends(j) ?<= starts(i)))
    }
  }

  // decomp with resource variables
  def decomp(cp: CPSolver, starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int): Unit = {
    val n = starts.length
    for (i <- 0 until n; j <- i + 1 until n) {
      cp.add((ends(i) ?<= starts(j)) || (ends(j) ?<= starts(i)) || (resources(i) ?!== id) || (resources(j) ?!== id))
    }
  }


  def randomDurations(n: Int, seed: Int = 0): Array[Int] = {
    val rand = new scala.util.Random(seed)
    Array.tabulate(n)(i => 1 + rand.nextInt(15))
  }

  def randomInstance(n: Int, seed: Int = 0) = {
    val rand = new scala.util.Random(seed)
    Array.tabulate(n)(i => (1 + rand.nextInt(3), rand.nextBoolean()))
  }


  for (strength <- CPPropagStrength.values()) {


    test("unary unit 2"+strength) {
      // s:{2..4},{2..4} dur:1,1 ends:{3..5},{3..5}newMin:2,2
      implicit val cp = CPSolver()
      cp.silent = true
      val starts = Array(CPIntVar(2 to 4), CPIntVar(2 to 4))
      val durations = Array(1, 1)
      val durs = durations.map(d => CPIntVar(d))
      val ends = Array.tabulate(starts.length)(i => starts(i) + durations(i))
      add(unaryResource(starts, durs, ends),strength)
      cp.isFailed shouldBe false
    }

    test("decomp vs global, permutations"+strength) {

      def testPermutations(seed: Int): Unit = {
        def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)

        val n = 4
        val durations = randomDurations(n, seed)
        val horizon = durations.sum
        implicit val cp = CPSolver()
        cp.silent = true
        val starts = Array.tabulate(n)(i => CPIntVar(0 until (horizon - durations(i) + 1)))
        val durs = Array.tabulate(n)(i => CPIntVar(durations(i)))
        val ends = Array.tabulate(n)(i => starts(i) + durations(i))

        cp.search {
          binaryFirstFail(starts)
        }

        val statDecomp = cp.startSubjectTo() {
          decomp(cp, starts, durs, ends)
        }

        val statGlobal = cp.startSubjectTo() {
          add(unaryResource(starts, durs, ends),strength)
        }

        statDecomp.nSols shouldBe statGlobal.nSols
        statDecomp.nSols shouldBe factorial(n)
      }
      for (i <- 0 until 100) {
        testPermutations(i)
      }

    }


    test("Test 1: packing"+strength) {

      val horizon = 5
      implicit val cp = new CPSolver()
      cp.silent = true
      val starts = Array.fill(4)(CPIntVar(0 to horizon))
      val ends = Array.fill(4)(CPIntVar(0 to horizon))
      val durs = Array(CPIntVar(3), CPIntVar(1), CPIntVar(2), CPIntVar(2))
      for (i <- 0 until 4) {
        add(starts(i) + durs(i) === ends(i))
      }
      add(ends(0) <= starts(1)) // 0 precedes act 1
      add(ends(2) <= starts(3)) // 2 precedes act 3

      val r1 = Array(0, 3) // activity on r1
      val r2 = Array(1, 2) // activity on r2

      add(unaryResource(r1.map(starts(_)), r1.map(durs(_)), r1.map(ends(_))),strength)
      add(unaryResource(r2.map(starts(_)), r2.map(durs(_)), r2.map(ends(_))),strength)

      val expectedSol = Set((0, 3, 0, 3), (0, 4, 0, 3), (0, 3, 1, 3), (0, 4, 1, 3))
      cp.search {
        binaryStatic(starts)

      } onSolution {
        val sol = (starts(0).value, starts(1).value, starts(2).value, starts(3).value)
        expectedSol.contains(sol) shouldBe true
      }

      start().nSols shouldBe 4
    }

    test("Test 2: durations"+strength) {

      val horizon = 5
      implicit val cp = new CPSolver()
      cp.silent = true
      val starts = Array.fill(4)(CPIntVar(0 to horizon))
      val ends = Array.fill(4)(CPIntVar(0 to horizon))
      val durs = Array(CPIntVar(3 to 4), CPIntVar(1), CPIntVar(2), CPIntVar(2))
      for (i <- 0 until 4) {
        add(starts(i) + durs(i) === ends(i))
      }
      add(ends(0) <= starts(1)) // 0 precedes act 1
      add(ends(2) <= starts(3)) // 2 precedes act 3

      val r1 = Array(0, 3) // activity on r1
      val r2 = Array(1, 2) // activity on r2

      add(unaryResource(r1.map(starts(_)), r1.map(durs(_)), r1.map(ends(_))),strength)
      add(unaryResource(r2.map(starts(_)), r2.map(durs(_)), r2.map(ends(_))),strength)

      val expectedSol = Set((0, 3, 0, 3), (0, 4, 0, 3), (0, 3, 1, 3), (0, 4, 1, 3))
      cp.search {
        binaryStatic(starts)

      } onSolution {
        durs(0).value shouldBe 3
        durs(0).isBound shouldBe true
        val sol = (starts(0).value, starts(1).value, starts(2).value, starts(3).value)
        expectedSol.contains(sol) shouldBe true
      }

      start().nSols shouldBe 4
    }


    test("Test 3: durations"+strength) {
      val horizon = 5
      implicit val cp = new CPSolver()
      cp.silent = true
      val starts = Array.fill(4)(CPIntVar(0 to horizon))
      val ends = Array.fill(4)(CPIntVar(0 to horizon))
      val durs = Array(CPIntVar(3 to 4), CPIntVar(2), CPIntVar(2), CPIntVar(1))
      for (i <- 0 until 4) {
        add(starts(i) + durs(i) === ends(i))
      }


      val r1 = Array(0, 1) // activity on r1
      val r2 = Array(2, 3) // activity on r2

      add(unaryResource(r1.map(starts(_)), r1.map(durs(_)), r1.map(ends(_))))
      add(unaryResource(r2.map(starts(_)), r2.map(durs(_)), r2.map(ends(_))))

      val expectedSol = Set((0, 3, 0, 2),
        (0, 3, 0, 3),
        (0, 3, 0, 4),
        (0, 3, 1, 0),
        (0, 3, 1, 3),
        (0, 3, 1, 4),
        (0, 3, 2, 0),
        (0, 3, 2, 1),
        (0, 3, 2, 4),
        (0, 3, 3, 0),
        (0, 3, 3, 1),
        (0, 3, 3, 2),
        (2, 0, 0, 2),
        (2, 0, 0, 3),
        (2, 0, 0, 4),
        (2, 0, 1, 0),
        (2, 0, 1, 3),
        (2, 0, 1, 4),
        (2, 0, 2, 0),
        (2, 0, 2, 1),
        (2, 0, 2, 4),
        (2, 0, 3, 0),
        (2, 0, 3, 1),
        (2, 0, 3, 2))
      cp.search {
        binaryStatic(starts)

      } onSolution {
        durs(0).value shouldBe 3
        durs(0).isBound shouldBe true
        val sol = (starts(0).value, starts(1).value, starts(2).value, starts(3).value)
        expectedSol.contains(sol) shouldBe true
      }

      start().nSols shouldBe 24
    }

    test("Test 4: 5 random activities"+strength) {
      val nActivities = 5
      val horizon = nActivities * 15
      for (i <- 1 to 5) {
        val randDurs = randomDurations(nActivities, i)

        val cp = new CPSolver()
        cp.silent = true
        val starts = Array.fill(nActivities)(CPIntVar(0 to horizon)(cp))
        val ends = Array.fill(nActivities)(CPIntVar(0 to horizon)(cp))
        val durs = randDurs.map(e => CPIntVar(e)(cp))
        val makespan = maximum(ends)

        for (i <- 0 until nActivities) {
          cp.add(starts(i) + durs(i) === ends(i))
        }
        cp.add(unaryResource(starts, durs, ends),strength)

        cp.minimize(makespan)

        cp.search {
          binaryStatic(starts)
        }



        val cp2 = new CPSolver()
        cp2.silent = true
        val starts2 = Array.fill(nActivities)(CPIntVar(0 to horizon)(cp2))
        val ends2 = Array.fill(nActivities)(CPIntVar(0 to horizon)(cp2))
        val durs2 = randDurs.map(e => CPIntVar(e)(cp2))
        val makespan2 = maximum(ends2)

        for (i <- 0 until nActivities) {
          cp2.add(starts2(i) + durs2(i) === ends2(i))
          for (j <- 0 until nActivities) {
            if (i != j) {
              cp2.add((ends2(i) ?<= starts2(j)) || (ends2(j) ?<= starts2(i)))
            }
          }
        }

        cp2.minimize(makespan2)

        cp2.search {
          binaryStatic(starts2)
        }

        val stats1 = cp.start()
        val stats2 = cp2.start()


        stats1.nSols shouldBe stats2.nSols
      }
    }

    test("Test 5: unary vs unary with optional"+strength) {
      val nActivities = 5
      val horizon = nActivities * 15
      for (i <- 1 to 10) {
        val randDurs = randomDurations(nActivities, i)

        val cp = new CPSolver()
        cp.silent = true
        val starts = Array.fill(nActivities)(CPIntVar(0 to horizon)(cp))
        val ends = Array.fill(nActivities)(CPIntVar(0 to horizon)(cp))
        val durs = randDurs.map(e => CPIntVar(e)(cp))
        val makespan = maximum(ends)

        for (i <- 0 until nActivities) {
          cp.add(starts(i) + durs(i) === ends(i))
        }
        cp.add(unaryResource(starts, durs, ends),strength)

        cp.minimize(makespan)

        cp.search {
          binaryFirstFail(starts)
        }

        val cp3 = new CPSolver()
        cp3.silent = true
        val starts3 = Array.fill(nActivities)(CPIntVar(0 to horizon)(cp3))
        val ends3 = Array.fill(nActivities)(CPIntVar(0 to horizon)(cp3))
        val durs3 = randDurs.map(e => CPIntVar(e)(cp3))
        val makespan3 = maximum(ends3)

        for (i <- 0 until nActivities) {
          cp3.add(starts3(i) + durs3(i) === ends3(i))
        }
        cp3.add(unaryResource(starts3, durs3, ends3),strength)

        cp3.minimize(makespan3)

        cp3.search {
          binaryStatic(starts3)
        }

        val stats1 = cp.start()
        val stats3 = cp3.start()

        stats1.nSols shouldBe stats3.nSols
      }
    }

  }

}
