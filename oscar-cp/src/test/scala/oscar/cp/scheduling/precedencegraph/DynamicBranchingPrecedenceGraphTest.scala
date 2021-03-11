package oscar.cp.scheduling.precedencegraph

import org.scalatest.{Assertions, FunSuite, Matchers}
import oscar.algo.search.DFSearch
import oscar.cp._
import oscar.cp.scheduling.precedencegraph.branching._
import oscar.cp.scheduling.precedencegraph.nogoods.NoGoodRecorder
import oscar.util.instanceGenerators.RandomFamilyInstanceGenerator
import oscar.util.instanceGenerators.utils.Utils

import scala.collection.mutable.ArrayBuffer

/**
  * Created by saschavancauwelaert on 17/05/16.
  */
class DynamicBranchingPrecedenceGraphTest extends FunSuite with Matchers with Assertions {

  test("Test 3 activities") {
    val nTests = 100
    testRandomInstances(nTests, 3, 3, 5, 15, 5, 20)
  }

  test("Test 4 activities") {
    val nTests = 50
    testRandomInstances(nTests, 4, 4, 5, 15, 5, 20)
  }

  test("Test 4 activities 2 families") {
    val nTests = 50
    testRandomInstances(nTests, 4, 2, 5, 15, 5, 20)
  }

  test("Test 5 activities") {
    val nTests = 20
    testRandomInstances(nTests, 5, 5, 5, 15, 5, 20)
  }

  test("Test 6 activities") {
    val nTests = 10
    testRandomInstances(nTests, 6, 6, 5, 15, 5, 20)
  }

  def testRandomInstances(nTests: Int, nActivities : Int, nFamilies : Int, minDuration: Int, maxDuration: Int, minTT : Int, maxTT : Int):Unit = {

    for(i <- 0 until nTests) {
      val (nActis, _, _, _, durations, ttMatrixWithSetup, _) = RandomFamilyInstanceGenerator.generateRandomJobShopInstance(nActivities, 1, nFamilies, minDuration, maxDuration, minTT, maxTT, true, 0)
      val ttMatrix = ttMatrixWithSetup.map(_.drop(1)).drop(1)
      val opt = optimumWithDecompAndBinaryStatic(nActis, durations, ttMatrix)
      val dynOpt = optimumWithDynamicPGBranching(nActis, durations, ttMatrix)
      val dynOptNg = optimumWithHebrardBranching(nActis, durations, ttMatrix)
      assert(opt == dynOptNg)
      assert(opt == dynOpt)
    }
  }

  def optimumWithDecompAndBinaryStatic(nActivities : Int, durations : Array[Int], ttMatrix : Array[Array[Int]]) : Int = {

    val horizon = durations.sum + ttMatrix.map(arr => arr.max).max * (nActivities - 1)
    val startMins = Array.fill(nActivities)(0)
    val endMaxs = Array.fill(nActivities)(horizon)

    val basicSolver = CPSolver()
    basicSolver.silent = true

    val resourceID = 0

    val durationVars = Array.tabulate(nActivities)(i => CPIntVar(durations(i))(basicSolver))
    val startVars = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) to endMaxs(i) - durations(i), s"start-$i")(basicSolver))
    val endVars = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) + durations(i) to endMaxs(i))(basicSolver))
    val runOnResource = Array.fill(nActivities)(CPIntVar(resourceID)(basicSolver))

    val resourceIDVar =  CPIntVar(resourceID)(basicSolver)

    for (i <- 0 until nActivities) {
      basicSolver.add(startVars(i) + durationVars(i) === endVars(i))
    }

    for {
      i <- 0 until nActivities
      j <- 0 until nActivities
      if i != j
    } {
      basicSolver.add((runOnResource(i) ?!== resourceIDVar) || (runOnResource(j) ?!== resourceIDVar) || (endVars(i) + ttMatrix(i)(j) ?<= startVars(j)) || (endVars(j) + ttMatrix(j)(i) ?<= startVars(i)))
    }

    basicSolver.search {conflictOrderingSearch(startVars, startVars(_).min, startVars(_).min)}

    val endVarsForMks = Array.tabulate(nActivities)(a => endVars(a) * (runOnResource(a) ?=== resourceID))
    val nonOptionalMakespan = maximum(endVarsForMks)
    basicSolver.minimize(nonOptionalMakespan)


    var mks = Int.MaxValue
    basicSolver.onSolution{
      mks = math.min(mks, nonOptionalMakespan.value)
    }

    val stats = basicSolver.start()
    mks
  }


  def optimumWithDynamicPGBranching(nActivities : Int, durations : Array[Int], ttMatrix : Array[Array[Int]]) : Int = {

    val resourceID = 0

    val horizon = durations.sum + ttMatrix.map(arr => arr.max).max * (nActivities - 1)
    val startMins = Array.fill(nActivities)(0)
    val endMaxs = Array.fill(nActivities)(horizon)
    val (families, familyTT) = Utils.getFamilies(ttMatrix)

    implicit val solver = CPSolver()
    solver.silent = true

    /** *******************************************************************
      * ************               VARIABLES                 **************
      * *******************************************************************/
    val startVars = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) to horizon - durations(i), "start-" + i))
    val durationVars = Array.tabulate(nActivities)(i => CPIntVar(durations(i)))
    val endVars = Array.tabulate(nActivities)(i => startVars(i) + durationVars(i))
    val runOnResourceVar: Array[CPIntVar] = Array.fill(nActivities)(CPIntVar(resourceID))
    val makespan = maximum(endVars)

    val precedenceGraph = PrecedenceGraph(startVars, durationVars, endVars, ttMatrix)
    val pgc = new PrecedenceGraphPropagator(startVars,durationVars,endVars, precedenceGraph, ttMatrix)

    add(pgc)

    /** *******************************************************************
      * ***********               CONSTRAINTS                 *************
      * *******************************************************************/

    for (i <- 0 until nActivities) {
      solver.add(startVars(i) + durationVars(i) === endVars(i))
    }

    /** *******************************************************************
      * ***********               SEARCH                 *************
      * *******************************************************************/

    val positionInJob = Array.tabulate(nActivities)(i => i)
    def valueOfPrec(task1: Int, task2: Int): Int = {
    positionInJob(task1) * positionInJob(task2) * startVars(task1).min * startVars(task2).min * startVars(task1).size * startVars(task2).size
    }

    def isPrecOrderPreferable(task1: Int, task2: Int) : Boolean = {
      if(endVars(task1).min + ttMatrix(task1)(task2) + durationVars(task2).min < endVars(task2).min + ttMatrix(task2)(task1) + durationVars(task1).min)
        true
      else if (endVars(task1).min + ttMatrix(task1)(task2) + durationVars(task2).min > endVars(task2).min + ttMatrix(task2)(task1) + durationVars(task1).min)
        false
      else if(endVars(task1).max < startVars(task2).min)
        true
      else if(endVars(task2).max < startVars(task1).min)
        false
      else if(ttMatrix(task1)(task2) < ttMatrix(task2)(task1))
        true
      else if(ttMatrix(task1)(task2) > ttMatrix(task2)(task1))
        false
      else
        task1 < task2
    }
    val brPrec = new DynamicPrecedenceGraphBranchingAllPrecedences(precedenceGraph, s"machine", valueOfPrec, isPrecOrderPreferable)

    val brStarts = new AssignAllVarsToMin(startVars)
    val heuristic = brPrec ++ brStarts
    solver.search(heuristic)

    val endVarsForMks = Array.tabulate(nActivities)(a => endVars(a) * (runOnResourceVar(a) ?=== resourceID))
    val nonOptionalMakespan = maximum(endVarsForMks)
    solver.minimize(nonOptionalMakespan)


    var mks = Int.MaxValue
    solver.onSolution {
      mks = math.min(mks, nonOptionalMakespan.value)
    }

    val stats = solver.start()
    mks
  }

  def optimumWithHebrardBranching(nActivities : Int, durations : Array[Int], ttMatrix : Array[Array[Int]]) : Int = {
    val horizon = durations.sum + ttMatrix.map(arr => arr.max).max * (nActivities - 1)
    val startMins = Array.fill(nActivities)(0)
    val endMaxs = Array.fill(nActivities)(horizon)
    val (families, familyTT) = Utils.getFamilies(ttMatrix)

    val resourceID = 0

    implicit val solver = CPSolver()
    solver.silent = true

    /** *******************************************************************
      * ************               VARIABLES                 **************
      * *******************************************************************/
    val startVars = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) to horizon - durations(i), "start-" + i))
    val durationVars = Array.tabulate(nActivities)(i => CPIntVar(durations(i)))
    val endVars = Array.tabulate(nActivities)(i => startVars(i) + durationVars(i))
    val runOnResourceVar: Array[CPIntVar] = Array.fill(nActivities)(CPIntVar(resourceID))
    val makespan = maximum(endVars)

    val precedenceGraph = new PrecedenceGraphWithNoGoods(startVars, durationVars, endVars, ttMatrix, Array(), null, -1)
    val pgc = new PrecedenceGraphPropagator(startVars,durationVars,endVars, precedenceGraph, ttMatrix)

    add(pgc)

    /** *******************************************************************
      * ***********               CONSTRAINTS                 *************
      * *******************************************************************/

    for (i <- 0 until nActivities) {
      solver.add(startVars(i) + durationVars(i) === endVars(i))
    }

    /** *******************************************************************
      * ***********               SEARCH                 *************
      * *******************************************************************/

    val machines = Array.fill(startVars.length)(0)

    val bestSolPrecs = Array.tabulate(1)(m => {
      val n = machines.count(_ == m)
      Array.fill(n,n)(false)
    })

    def isPrecOrderPreferableHebrard(machine: Int, task1: Int, task2: Int) : Boolean = {
      bestSolPrecs(machine)(task1)(task2)
    }

    val brPrec = new HebrardHeuristic(Array(precedenceGraph), Array(""), isPrecOrderPreferableHebrard, startVars, machines)

    val brStarts = new AssignAllVarsToMin(startVars)
    val heuristic = brPrec
    solver.search(heuristic ++ brStarts)

    val endVarsForMks = Array.tabulate(nActivities)(a => endVars(a) * (runOnResourceVar(a) ?=== resourceID))
    val nonOptionalMakespan = maximum(endVarsForMks)
    solver.minimize(nonOptionalMakespan)

    var mks = Int.MaxValue
    solver.onSolution {
      mks = math.min(mks, nonOptionalMakespan.value)
    }

    val precGraphs = heuristic.asInstanceOf[HebrardHeuristic].precGraphs
    val recorder = new NoGoodRecorder(1, nActivities, precGraphs)
    precGraphs(0).noGoodBase = recorder.base
    precGraphs(0).machineId=0

    def stopConditionNoGood(nBacks: Int) : DFSearch => Boolean = {
      search : DFSearch => {
        if(nBacks <= search.nBacktracks) { // will stop
          val reversedLastBranch = ArrayBuffer[PrecGraphDecisionForNoGood]()
          val stack = heuristic.asInstanceOf[HebrardHeuristic].decisionStackForNoGood
          while(!stack.isEmpty)
            reversedLastBranch += stack.pop()
          val lastBranch = reversedLastBranch.reverse
          recorder.storeNoGoods(lastBranch.toArray)
          true
        }
        else
          false
      }
    }

    var completed = false
    var r = 1.3
    while(!completed) {
      for(mandatoryLit <- recorder.unaryBase) {
        val pgs = heuristic.asInstanceOf[HebrardHeuristic].precGraphs
        pgs(mandatoryLit.machine).addNonDetectablePrecAndUpdateTransitiveClosure(mandatoryLit.to, mandatoryLit.from)
      }
      recorder.unaryBase.clear()
      val s = 64
      val computedLimit = s * r
      val limit = computedLimit.toInt
      val stats = solver.startSubjectTo(stopConditionNoGood(limit), Int.MaxValue, null){}
      if(stats.completed)
        completed = true
      r = r * 1.3
    }
    mks
  }

}
