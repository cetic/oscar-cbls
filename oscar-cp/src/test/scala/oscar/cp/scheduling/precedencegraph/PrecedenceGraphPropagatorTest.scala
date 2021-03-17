package oscar.cp.scheduling.precedencegraph

import org.scalatest.{Assertions, FunSuite, Matchers}
import oscar.cp._
import oscar.cp.scheduling.precedencegraph.branching.StaticPrecedenceGraphBranching
import oscar.util.instanceGenerators.RandomFamilyInstanceGenerator
import oscar.util.instanceGenerators.utils.Utils

import scala.collection.mutable.ArrayBuffer

/**
  * Created by saschavancauwelaert on 17/05/16.
  */

class PrecedenceGraphPropagatorTest extends FunSuite with Matchers with Assertions {

  val seed = 30
  val randGen = new scala.util.Random(seed)

  test("Random instance of 2 activities 1 family") {
    genericTest(2, 1, 1000)
  }

  test("Random instance of 2 activities 2 families") {
    genericTest(2, 2, 1000)
  }

  test("Random instance of 3 activities 1 family") {
    genericTest(3, 1, 50)
  }

  test("Random instance of 3 activities 3 families") {
    genericTest(3, 3, 1000, minimization=true)
  }

  test("Random instance of 4 activities") {
    genericTest(4, 1, 500, minimization=true)
  }

  test("Random instance of 4 activities 2 families") {
    genericTest(4, 2, 500, minimization=true)
  }

  test("Random instance of 4 activities 4 families") {
    genericTest(4, 4, 500, minimization=true)
  }

  test("Random instance of 5 activities") {
    genericTest(5, 1, 10, minimization=true)
  }

  test("Random instance of 5 activities 5 families") {
    genericTest(5, 5, 10, minimization=true)
  }

  test("Random instance of 6 activities") {
    genericTest(6, 1, 10, minimization=true)
  }

  test("Random instance of 6 activities 6 families") {
    genericTest(6, 6, 10, minimization=true)
  }

  test("Random instance of 6 activities 2 families") {
    genericTest(6, 2, 10, minimization=true)
  }

  test("Random instance of 6 activities 3 families") {
    genericTest(6, 3, 10, minimization=true)
  }

  def genericTest(nActivities: Int, nFamilies: Int, nRuns: Int, minimization: Boolean=false, printStats: Boolean=false, shouldRun: Boolean=true, testSols: Boolean=true, numSols: Int = Int.MaxValue) = {
    for(i <- 0 until nRuns) {
      val nMachines = 1
      val minTT = randGen.nextInt(20)
      val maxTT = minTT + randGen.nextInt(20)
      val minDuration = 5 + randGen.nextInt(30)
      val maxDuration = minDuration + randGen.nextInt(30)
      val (nActis, nJobs, nMach, machines, durations, ttMatrixWithSetup, _) =
        RandomFamilyInstanceGenerator.generateRandomJobShopInstance(nActivities, nMachines, nFamilies, minDuration, maxDuration, minTT, maxTT, familiesOfEqualSize=false, 0)
      val ttMatrix = ttMatrixWithSetup.map(_.drop(1)).drop(1)

      if (shouldRun) {
        if (printStats) {
          println(s"Run #${i}")
        }
        compareWithDecomp(nActivities, durations, ttMatrix, minimize = minimization, verbose = printStats, testSolutions = testSols, numSolutions = numSols)
      }
    }
  }

  def compareWithDecomp(nActivities : Int, durations : Array[Int], ttMatrix : Array[Array[Int]], minimize : Boolean = false, verbose: Boolean = false, numSolutions : Int = Int.MaxValue, testSolutions: Boolean=false) : Unit /*(SearchStatistics, SearchStatistics)*/ = {

    val horizon = durations.sum + ttMatrix.map(arr => arr.max).max * (nActivities - 1)
    val startMins = Array.fill(nActivities)(0)
    val endMaxs = Array.fill(nActivities)(horizon)
    val (families, familyTT) = Utils.getFamilies(ttMatrix)
    var solutions = ArrayBuffer[Array[Int]]()
    var solutionsAugmented = ArrayBuffer[Array[Int]]()

    /** ************************************************************************
      * ***********************     BINARY RESOLUTION     ***********************
      * *************************************************************************/
    val basicSolver = CPSolver()
    basicSolver.silent = true

    val durationVars = Array.tabulate(nActivities)(i => CPIntVar(durations(i))(basicSolver))
    val startVars = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) to endMaxs(i) - durations(i), s"start $i")(basicSolver))
    val endVars = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) + durations(i) to endMaxs(i), "end " + i)(basicSolver))

    for (i <- 0 until nActivities) {
      basicSolver.add(startVars(i) + durationVars(i) === endVars(i))
    }

    for {
      i <- 0 until nActivities
      j <- 0 until nActivities
      if i != j
    } {
      basicSolver.add((endVars(i) + ttMatrix(i)(j) ?<= startVars(j)) || (endVars(j) + ttMatrix(j)(i) ?<= startVars(i)))
    }

    basicSolver.search(new StaticPrecedenceGraphBranching(startVars, endVars, None))

    if (minimize) {
      val makespan = maximum(endVars)
      basicSolver.minimize(makespan)
    }

    if (testSolutions) {
      basicSolver.onSolution {
        solutions += startVars.map(sv => sv.min)
      }
    }

    val statsBasic = basicSolver.start(timeLimit = 1000)

    if (!statsBasic.completed) {
      println("DECOMP COULD NOT FINISH INSTANCE")
      return
    }

    if (verbose) {
      println("BINARY DECOMP")
      println(statsBasic)
    }

    /** ************************************************************************
      * ***********************     SMARTER RESOLUTION     **********************
      * *************************************************************************/

    val augmentedSolver = CPSolver()
    augmentedSolver.silent = true

    val durationVarsAugmented = Array.tabulate(nActivities)(i => CPIntVar(durations(i))(augmentedSolver))
    val startVarsAugmented = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) to endMaxs(i) - durations(i), "start " + i)(augmentedSolver))
    val endVarsAugmented = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) + durations(i) to endMaxs(i), "end " + i)(augmentedSolver))

    for (i <- 0 until nActivities) {
      augmentedSolver.add(startVarsAugmented(i) + durationVarsAugmented(i) === endVarsAugmented(i))
    }

    //TODO:check if we cannot we remove those binary constraints if the precedence graph is used
    for {
      i <- 0 until nActivities
      j <- 0 until nActivities
      if i != j
    } {
      augmentedSolver.add((endVarsAugmented(i) + ttMatrix(i)(j) ?<= startVarsAugmented(j)) || (endVarsAugmented(j) + ttMatrix(j)(i) ?<= startVarsAugmented(i)))
    }

    val pg = PrecedenceGraph(startVarsAugmented, durationVarsAugmented, endVarsAugmented, ttMatrix)
    augmentedSolver.add(new PrecedenceGraphPropagator(startVarsAugmented, durationVarsAugmented, endVarsAugmented, pg, ttMatrix))

    augmentedSolver.search(new StaticPrecedenceGraphBranching(startVarsAugmented, endVarsAugmented, Some(pg), ""))

    if (minimize) {
      val makespanAugmented = maximum(endVarsAugmented)
      augmentedSolver.minimize(makespanAugmented)
    }

    if (testSolutions) {
      augmentedSolver.onSolution {
        solutionsAugmented += startVarsAugmented.map(sv => sv.min)
      }
    }

    val statAugmented = augmentedSolver.start()

    if (verbose) {
      println("PRECEDENCE GRAPH")
      println(statAugmented)
    }
    solutionsAugmented.length shouldBe solutions.length

    checkSolutionsAreEqual(solutions.toArray, solutionsAugmented.toArray)

    assert(statAugmented.nFails <= statsBasic.nFails)
  }


  def checkSolutionsAreEqual(sols1: Array[Array[Int]], sols2: Array[Array[Int]]): Unit = {
    for(a <- 0 until sols1.length) {
      assert(areSolutionsEquivalent(sols1(a), sols2(a)))
    }
  }

  def testConstraints(solver: CPSolver, block: => Unit): Unit = {
    solver.pushState()
    block
    solver.pop()
  }
  def areSolutionsEquivalent(starts1: Array[Int], starts2: Array[Int]) : Boolean = (0 until starts1.length).forall(a => (starts1(a) == starts2(a)))
}
