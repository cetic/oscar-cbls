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
import oscar.cp.testUtils.TestSuite

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestUnaryResourceWithOptionalActivities extends TestSuite {

  // decomp without resource variables
  def decomp(cp: CPSolver, starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar]): Unit = {
    val n = starts.size
    for (i <- 0 until n; j <- i + 1 until n) {
      cp.add((ends(i) ?<= starts(j)) || (ends(j) ?<= starts(i)))
    }
  }
  
  // decomp with resource variables
  def decomp(cp: CPSolver, starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar],resources: Array[CPIntVar], id: Int ): Unit = {
    val n = starts.size
    for (i <- 0 until n; j <- i + 1 until n) {
      cp.add((ends(i) ?<= starts(j)) || (ends(j) ?<= starts(i)) || (resources(i) ?!== id) || (resources(j) ?!== id))
    }
  }  

  def unary(cp: CPSolver, starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar]): Unit = {
    cp.add(unaryResource(starts, durations, ends))
  }

  def randomDurations(n: Int, seed: Int = 0): Array[Int] = {
    val rand = new scala.util.Random(seed)
    Array.tabulate(n)(i => 1 + rand.nextInt(15))
  }
  
  def randomInstance(n: Int, seed: Int = 0) = {
    val rand = new scala.util.Random(seed)
    Array.tabulate(n)(i => (1 + rand.nextInt(3),rand.nextBoolean))
  }
  
  test("unary unit 1") {

      implicit val cp = CPSolver()
      cp.silent = true
      val starts = Array(CPIntVar(10), CPIntVar(7), CPIntVar(10))
      val durations = Array(3,1,3)
      val durs = durations.map(d => CPIntVar(d))
      val ends = Array.tabulate(starts.size)(i => starts(i) + durations(i))
      val required = Array(CPBoolVar(false),CPBoolVar(true),CPBoolVar(false))
      add(unaryResource(starts,durs,ends, required))
      cp.isFailed should be(false)
  } 
  
  test("unary unit 2") {
      // s:{2..4},{2..4} dur:1,1 ends:{3..5},{3..5}newMin:2,2
      implicit val cp = CPSolver()
      cp.silent = true
      val starts = Array(CPIntVar(2 to 4), CPIntVar(2 to 4))
      val durations = Array(1,1)
      val durs = durations.map(d => CPIntVar(d))
      val ends = Array.tabulate(starts.size)(i => starts(i) + durations(i))
      val required = Array(CPBoolVar(true),CPBoolVar(true))
      add(unaryResource(starts,durs,ends, required))
      cp.isFailed should be(false)
      //println(starts.mkString(","))
  }   
  
  
  
  test("unary vs cumul") {
    for (i <- 0 to 10) {
      val n = 4
      val inst = randomInstance(n,i)
      
      val durations = inst.map(_._1)
      val optional = inst.map(_._2)
      val horizon = durations.sum
      val cp = CPSolver()
      cp.silent = true
      val starts = Array.tabulate(n)(i => CPIntVar(0 until (horizon - durations(i) + 1))(cp))
      val durs = Array.tabulate(n)(i => CPIntVar(durations(i))(cp))
      val ends = Array.tabulate(n)(i => starts(i) + durations(i))
      val resources = Array.tabulate(n)(i => if (optional(i)) CPIntVar(0 to 1)(cp) else CPIntVar(0)(cp) )
      cp.search {
        binaryFirstFail(starts) ++ binaryFirstFail(resources)
      }
      
      val statCum = cp.startSubjectTo() {
        cp.add(maxCumulativeResource(starts, durs, ends,starts.map(_ => CPIntVar(1)(cp)), resources, CPIntVar(1)(cp),0))
      }
      val statUnary = cp.startSubjectTo() {
        cp.add(unaryResource(starts,durs,ends, resources,0))
      }
      
      val statUnaryDecomp = cp.startSubjectTo() {
        decomp(cp,starts,durs,ends, resources,0)
      }
      
      statCum.nSols should be (statUnary.nSols)
      statUnaryDecomp.nSols should be (statUnary.nSols)
    }

     
  }
  

  
 
  test("decomp vs global, permutations") {

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

      cp.onSolution {
        val p = (0 until n).sortBy(i => starts(i).value)
        //println("solution:" + p.map(i => "[" + starts(i).value + "," + ends(i).value + "]"))
      }

      cp.search {
        binaryFirstFail(starts)
      }

      val statDecomp = cp.startSubjectTo() {
        decomp(cp, starts, durs, ends)
      }
      
      val statGlobal = cp.startSubjectTo() {
        unary(cp, starts, durs, ends)
      }
      
      statDecomp.nSols should be(statGlobal.nSols)
      statDecomp.nSols should be(factorial(n))
    }
    for (i <- 0 until 100) {
      testPermutations(i)
    }

  }
  

	test("Test 1: packing") {	
		
		val horizon = 5
		implicit val cp = new CPSolver()
		cp.silent = true
        val starts = Array.fill(4)(CPIntVar(0 to 5))
        val ends = Array.fill(4)(CPIntVar(0 to 5))
        val durs = Array(CPIntVar(3),CPIntVar(1),CPIntVar(2),CPIntVar(2))
        for (i <- 0 until 4) {
          add(starts(i) + durs(i) === ends(i))
        }
		add(ends(0) <= starts(1)) // 0 precedes act 1
		add(ends(2) <= starts(3)) // 2 precedes act 3
		
		val r1 =  Array(0,3) // activity on r1
		val r2 = Array(1,2) // activity on r2
		
		add(unaryResource(r1.map(starts(_)),r1.map(durs(_)),r1.map(ends(_))))
		add(unaryResource(r2.map(starts(_)),r2.map(durs(_)),r2.map(ends(_))))
		
		var nSol = 0
		
		val expectedSol = Set((0, 3, 0, 3), (0, 4, 0, 3), (0, 3, 1, 3), (0, 4, 1, 3))
		cp.search {
			binaryStatic(starts)

		} onSolution {
		  	val sol = (starts(0).value,starts(1).value,starts(2).value,starts(3).value)
			expectedSol.contains(sol) should be(true)
		}
		
		start().nSols should be(4)
	}
	
	test("Test 2: durations") {	
		
		val horizon = 5
		implicit val cp = new CPSolver()
		cp.silent = true
        val starts = Array.fill(4)(CPIntVar(0 to 5))
        val ends = Array.fill(4)(CPIntVar(0 to 5))
        val durs = Array(CPIntVar(3 to 4),CPIntVar(1),CPIntVar(2),CPIntVar(2))
        for (i <- 0 until 4) {
          add(starts(i) + durs(i) === ends(i))
        }
		add(ends(0) <= starts(1)) // 0 precedes act 1
		add(ends(2) <= starts(3)) // 2 precedes act 3
		
		val r1 =  Array(0,3) // activity on r1
		val r2 = Array(1,2) // activity on r2
		
		add(unaryResource(r1.map(starts(_)),r1.map(durs(_)),r1.map(ends(_))))
		add(unaryResource(r2.map(starts(_)),r2.map(durs(_)),r2.map(ends(_))))
		
		var nSol = 0
		
		val expectedSol = Set((0, 3, 0, 3), (0, 4, 0, 3), (0, 3, 1, 3), (0, 4, 1, 3))
		cp.search {
			binaryStatic(starts)

		} onSolution {
		    durs(0).value should be(3)
		    durs(0).isBound should be(true)
		  	val sol = (starts(0).value,starts(1).value,starts(2).value,starts(3).value)
			expectedSol.contains(sol) should be(true)
		}
		
		start().nSols should be(4)
	}
	
	test("Test 3: durations") {	
		
		val horizon = 5
		implicit val cp = new CPSolver()
		cp.silent = true
        val starts = Array.fill(4)(CPIntVar(0 to 5))
        val ends = Array.fill(4)(CPIntVar(0 to 5))
        val durs = Array(CPIntVar(3 to 4),CPIntVar(2),CPIntVar(2),CPIntVar(1))
        for (i <- 0 until 4) {
          add(starts(i) + durs(i) === ends(i))
        }

		
		val r1 =  Array(0,1) // activity on r1
		val r2 = Array(2,3) // activity on r2
		
		add(unaryResource(r1.map(starts(_)),r1.map(durs(_)),r1.map(ends(_))))
		add(unaryResource(r2.map(starts(_)),r2.map(durs(_)),r2.map(ends(_))))
		
		var nSol = 0
		
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
		    durs(0).value should be(3)
		    durs(0).isBound should be(true)
		  	val sol = (starts(0).value,starts(1).value,starts(2).value,starts(3).value)
			expectedSol.contains(sol) should be(true)
		}
		
		start().nSols should be(24)
	}

  test("unary minimization with optional") {
    for (i <- 0 to 100) {
      var n = 6
      val inst = randomInstance(n / 2, i)
      val durations = inst.map(_._1) ++ inst.map(_._1)

      val horizon = durations.sum
      val expectedOpt = horizon / 2

      implicit val cp = CPSolver()
      cp.silent = true
      val starts = Array.tabulate(n)(i => CPIntVar(0 until (horizon - durations(i) + 1)))
      val durs = Array.tabulate(n)(i => CPIntVar(durations(i)))
      val ends = Array.tabulate(n)(i => starts(i) + durations(i))
      val resources = Array.tabulate(n)(i => CPIntVar(1 to 2))

      add(unaryResource(starts, durs, ends, resources, 1))
      add(unaryResource(starts, durs, ends, resources, 2))
      val obj = maximum(ends)
      var bestObj = Int.MaxValue
      minimize(obj)
      cp.search {
        binaryFirstFail(resources) ++ binaryFirstFail(starts)
      } onSolution {
        bestObj = obj.value
      }

      val stat = start()

      cp.search {
         binaryFirstFail(starts) ++ binaryFirstFail(resources)
      }
      bestObj should be(expectedOpt)
    }

  }
	


}
