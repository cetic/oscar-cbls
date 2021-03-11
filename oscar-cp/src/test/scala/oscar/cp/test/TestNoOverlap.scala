package oscar.cp.test

import org.scalatest.{Assertions, FunSuite, Matchers}
import oscar.cp._
import oscar.cp.constraints.nooverlap.{NoOverlap, NoOverlapTransitionTimes, NoOverlapTransitionTimesFamilies}
import oscar.cp.constraints.nooverlap.AlternativeResourcesTransitionTimes
import oscar.cp.scheduling.search.StaticBranchingWithOptionalActivities
import oscar.util.instanceGenerators.RandomFamilyInstanceGenerator
import oscar.util.instanceGenerators.utils.Utils

import scala.collection.mutable.ArrayBuffer

/**
  * Created by saschavancauwelaert on 17/05/16.
  */

class AbstractNoOverlapTest extends FunSuite with Matchers with Assertions {

  def unaryConstraint(startVars : Array[CPIntVar], durationVars : Array[CPIntVar], endVars : Array[CPIntVar], runOnResource : Array[CPIntVar], resourceId: Int, ttMatrix : Array[Array[Int]], familyMatrix: Array[Array[Int]], family: Array[Int]) : Option[Constraint] = None

  val seed = 30
  val randGen = new scala.util.Random(seed)

  val probOptional = 0.2F

  test("Random instance of 2 activities 1 family") {
    genericTest(2, 1, 10, shouldRun = true, testSols = true, printStats=false, probaOptional = probOptional)
  }

  test("Random instance of 2 activities 2 families") {
    genericTest(2, 2, 10, shouldRun = true, testSols = true, printStats=false, probaOptional = probOptional)
  }

  test("Random instance of 3 activities 1 family") {
    genericTest(3, 1, 10, shouldRun = true, testSols = true, printStats=false, probaOptional = probOptional)
  }

  test("Random instance of 3 activities 3 families") {
    genericTest(3, 3, 10, minimization=true, testSols = true, printStats=false, probaOptional = probOptional)
  }

  test("Random instance of 4 activities") {
    genericTest(4, 1, 10, minimization=true, printStats=false, probaOptional = probOptional)
  }

  test("Random instance of 4 activities 2 families") {
    genericTest(4, 2, 10, minimization=true, printStats=false, probaOptional = probOptional)
  }

  test("Random instance of 4 activities 4 families") {
    genericTest(4, 4, 10, minimization=true, printStats=false, probaOptional = probOptional)
  }

  def genericTest(nActivities: Int, nFamilies: Int, nRuns: Int, minimization: Boolean=false, printStats: Boolean=false, shouldRun: Boolean=true, testSols: Boolean=false, numSols: Int = Int.MaxValue, probaOptional: Float = 0) = {
    for(i <- 0 until nRuns) {
      val nMachines = 1
      val minTT = randGen.nextInt(20)
      val maxTT = minTT + randGen.nextInt(20)
      val minDuration = 5 + randGen.nextInt(30)
      val maxDuration = minDuration + randGen.nextInt(30)
      val (nActis, nJobs, nMach, machines, durations, ttMatrixWithSetup, isOptional) =
        RandomFamilyInstanceGenerator.generateRandomJobShopInstance(nActivities, nMachines, nFamilies, minDuration, maxDuration, minTT, maxTT, familiesOfEqualSize = false, probaOptional)
      val ttMatrix = ttMatrixWithSetup.map(_.drop(1)).drop(1)

      if (shouldRun) {
        if (printStats) {
          println(s"Run #${i}")
        }
        val nOptional: Int = isOptional.filter(_ == true).length
        if (nOptional < nActivities || !minimization)
          compareWithDecomp(nActivities, durations, ttMatrix, isOptional, minimize = minimization, verbose = printStats, testSolutions = testSols, numSolutions = numSols)
      }
    }
  }

  def compareWithDecomp(nActivities : Int, durations : Array[Int], ttMatrix : Array[Array[Int]], isOptional: Array[Boolean], minimize : Boolean = false, verbose: Boolean = false, numSolutions : Int = Int.MaxValue, testSolutions: Boolean=false) : Unit /*(SearchStatistics, SearchStatistics)*/ = {

    val resourceID = 0
    val otherResourceID = 1

    val horizon = durations.sum + ttMatrix.map(arr => arr.max).max * (nActivities - 1)
    val startMins = Array.fill(nActivities)(0)
    val endMaxs = Array.fill(nActivities)(horizon)
    val (families, familyTT) = Utils.getFamilies(ttMatrix)
    var solutions = ArrayBuffer[Array[Int]]()
    var solutions2 = ArrayBuffer[Array[Int]]()
    var isOptionalInSolution = ArrayBuffer[Array[Boolean]]()
    var isOptionalInSolution2 = ArrayBuffer[Array[Boolean]]()

    /** ************************************************************************
      * ***********************     BINARY RESOLUTION     ***********************
      * *************************************************************************/
    val basicSolver = CPSolver()
    basicSolver.silent = true

    val durationVars = Array.tabulate(nActivities)(i => CPIntVar(durations(i))(basicSolver))
    val startVars = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) to endMaxs(i) - durations(i), s"start $i")(basicSolver))
    val endVars = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) + durations(i) to endMaxs(i), "end " + i)(basicSolver))
    val runOnResource = Array.tabulate(nActivities)(i => if(isOptional(i)) CPIntVar(resourceID to otherResourceID, s"res $i")(basicSolver) else CPIntVar(resourceID, s"res $i")(basicSolver))

    val resourceIDVar = CPIntVar(resourceID)(basicSolver)

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

    basicSolver.search {
      new StaticBranchingWithOptionalActivities(startVars, runOnResource, resourceID)
    }
    if (minimize) {
      val endVarsForMks = Array.tabulate(nActivities)(a => endVars(a) * (runOnResource(a) ?=== resourceID))
      val nonOptionalMakespan = maximum(endVarsForMks)
      basicSolver.minimize(nonOptionalMakespan)
    }

    if (testSolutions) {
      basicSolver.onSolution {
        solutions += startVars.map(sv => sv.min)
        isOptionalInSolution += runOnResource.map(sv => sv.isBoundTo(resourceID))
      }
    }

    val stats = basicSolver.start(/*numSolutions,*/ timeLimit = 5)

    if (!stats.completed) {
      //println("DECOMP COULD NOT FINISH INSTANCE")
      return
    }


    if (verbose) {
      println("BINARY DECOMP")
      println(stats)
    }

    /** ************************************************************************
      * ***********************     SMARTER RESOLUTION     **********************
      * *************************************************************************/

    val augmentedSolver = CPSolver()
    augmentedSolver.silent = true

    val durationVarsExtended = Array.tabulate(nActivities)(i => CPIntVar(durations(i))(augmentedSolver))
    val startVarsExtended = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) to endMaxs(i) - durations(i), "start " + i)(augmentedSolver))
    val endVarsExtended = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) + durations(i) to endMaxs(i), "end " + i)(augmentedSolver))
    val runOnResourceExtended = Array.tabulate(nActivities)(i => if(isOptional(i)) CPIntVar(resourceID to otherResourceID, s"res $i")(augmentedSolver) else CPIntVar(resourceID, "res " + i)(augmentedSolver))

    for (i <- 0 until nActivities) {
      augmentedSolver.add(startVarsExtended(i) + durationVarsExtended(i) === endVarsExtended(i))
    }

    for {
      i <- 0 until nActivities
      j <- 0 until nActivities
      if i != j
    } {
      augmentedSolver.add((runOnResourceExtended(i) ?!== resourceIDVar) || (runOnResourceExtended(j) ?!== resourceIDVar) || (endVarsExtended(i) + ttMatrix(i)(j) ?<= startVarsExtended(j)) || (endVarsExtended(j) + ttMatrix(j)(i) ?<= startVarsExtended(i)))
    }

    val unary = unaryConstraint(startVarsExtended, durationVarsExtended, endVarsExtended, runOnResourceExtended, resourceID, ttMatrix, familyTT, families)
    unary match {case None => ; case Some(u) => augmentedSolver.add(u)}

    augmentedSolver.search {
      new StaticBranchingWithOptionalActivities(startVarsExtended, runOnResourceExtended, resourceID)
    }

    if (minimize) {
      val endVarsForMksExtended = Array.tabulate(nActivities)(a => endVarsExtended(a) * (runOnResourceExtended(a) ?=== resourceID))
      val nonOptionalMakespanExtended = maximum(endVarsForMksExtended)
      augmentedSolver.minimize(nonOptionalMakespanExtended)
    }


    if (testSolutions) {
      augmentedSolver.onSolution {
        solutions2 += startVarsExtended.map(sv => sv.min)
        isOptionalInSolution2 += runOnResourceExtended.map(sv => sv.hasValue(resourceID))
      }
    }

    val statAugmented = augmentedSolver.start(/*numSolutions*/)

    if (verbose) {
      println("UNARY GLOBAL")
      println(statAugmented)
    }


    val filteredSols1 = filterEquivalentSolutions(solutions.toArray, isOptionalInSolution.toArray)
    val filteredSols2 = filterEquivalentSolutions(solutions2.toArray, isOptionalInSolution2.toArray)

    assert(filteredSols1.length == solutions.length)
    assert(filteredSols2.length == solutions2.length)

    if (verbose) {
      printFilteredSol(filteredSols1, filteredSols2)
    }

    filteredSols1.length shouldBe filteredSols2.length
    checkSolutionsAreEqual(filteredSols1, filteredSols2)
    assert(statAugmented.nFails <= stats.nFails)
  }


  def printFilteredSol(sols1: Array[(Array[Int], Array[Boolean])], sols2: Array[(Array[Int], Array[Boolean])]): Unit ={
    val minNumSol = math.min(sols1.length, sols2.length)
    for(a <- 0 until minNumSol) {
      println("-"*80)
      println(sols1(a)._1.mkString(" "))
      println(sols1(a)._2.mkString(" "))
      println(sols2(a)._1.mkString(" "))
      println(sols2(a)._2.mkString(" "))
      println("-"*80)
    }

    for(a <- minNumSol until sols1.length) {
      println("-"*80)
      println("EXTRA SOL FOR DECOMP")
      println(sols1(a)._1.mkString(" "))
      println(sols1(a)._2.mkString(" "))
      println("-"*80)
    }

    for(a <- minNumSol until sols2.length) {
      println("-"*80)
      println("EXTRA SOL FOR GLOBAL")
      println(sols2(a)._1.mkString(" "))
      println(sols2(a)._2.mkString(" "))
      println("-"*80)
    }
  }


  def checkSolutionsAreEqual(sols1: Array[(Array[Int], Array[Boolean])], sols2: Array[(Array[Int], Array[Boolean])]): Unit = {
    for(a <- 0 until sols1.length) {
      assert(areSolutionsEquivalent(sols1(a)._1, sols2(a)._1, sols1(a)._2, sols2(a)._2))
    }
  }

  def areSolutionsEquivalent(starts1: Array[Int], starts2: Array[Int], runOnResource1: Array[Boolean], runOnResource2: Array[Boolean]) : Boolean = (0 until starts1.length).forall(a => runOnResource1(a) == runOnResource2(a) && (starts1(a) == starts2(a) || runOnResource1(a) == false ))

  def filterEquivalentSolutions(startsOfAllSols: Array[Array[Int]], runOnResourceOfAllSols: Array[Array[Boolean]], print: Boolean = false) : Array[(Array[Int], Array[Boolean])] = {

    if(print) {
      for(s <- 0 until startsOfAllSols.length) {
        println(startsOfAllSols(s).mkString(" ") + " " + runOnResourceOfAllSols(s).mkString(" "))
      }
    }

    val filteredSolutions = ArrayBuffer[(Array[Int], Array[Boolean])]()
    (0 until startsOfAllSols.length).foreach(sol => {
      val starts = startsOfAllSols(sol)
      val runOnRes = runOnResourceOfAllSols(sol)
      if(!(0 until filteredSolutions.length).exists(s => areSolutionsEquivalent(starts, filteredSolutions(s)._1, runOnRes, filteredSolutions(s)._2))){
        filteredSolutions += Tuple2(starts, runOnRes)
      }
    })

    if(print) {
      println("*"*80)

      for(s <- 0 until filteredSolutions.length) {
        println(filteredSolutions(s)._1.mkString(" ") + " " + filteredSolutions(s)._2.mkString(" "))
      }
    }

    filteredSolutions.toArray

  }
}

class NoOverlapTest extends AbstractNoOverlapTest {
  override def unaryConstraint(startVars : Array[CPIntVar], durationVars : Array[CPIntVar], endVars : Array[CPIntVar], runOnResource : Array[CPIntVar], resourceId: Int, ttMatrix : Array[Array[Int]], familyMatrix: Array[Array[Int]], family: Array[Int]) : Option[Constraint] = {
    Some(new NoOverlap(startVars, durationVars, endVars, runOnResource, resourceId))
  }
}

class NoOverlapTransitionTimesTest extends AbstractNoOverlapTest {
  override def unaryConstraint(startVars : Array[CPIntVar], durationVars : Array[CPIntVar], endVars : Array[CPIntVar], runOnResource : Array[CPIntVar], resourceId: Int, ttMatrix : Array[Array[Int]], familyMatrix: Array[Array[Int]], family: Array[Int]) : Option[Constraint] = {
    Some(new NoOverlapTransitionTimes(startVars, durationVars, endVars, ttMatrix, runOnResource, resourceId))
  }
}

class NoOverlapTransitionTimesFamilyTest extends AbstractNoOverlapTest {
  override def unaryConstraint(startVars : Array[CPIntVar], durationVars : Array[CPIntVar], endVars : Array[CPIntVar], runOnResource : Array[CPIntVar], resourceId: Int, ttMatrix : Array[Array[Int]], familyMatrix: Array[Array[Int]], family: Array[Int]) : Option[Constraint] = {
    Some(new NoOverlapTransitionTimesFamilies(startVars, durationVars, endVars, familyMatrix, family, runOnResource, resourceId))
  }
}

class SimpleNoOverlapTest extends FunSuite with Matchers with Assertions {
  test("The domain of variable is only updated once the activity is known to be running on the resource") {
    implicit val cp = CPSolver()
    val starts = Array(CPIntVar(0 to 10), CPIntVar(0 to 10), CPIntVar(0 to 30))
    val durations = Array(CPIntVar(5), CPIntVar(5), CPIntVar(5))
    val ends = Array(CPIntVar(5 to 10), CPIntVar(5 to 10), CPIntVar(5 to 50))
    val runOnResource = Array(CPIntVar(0), CPIntVar(0), CPIntVar(0 to 1))
    val resourceId = 0

    for(a <- 0 until starts.length)
      post(starts(a) + durations(a) === ends(a))

    post(new NoOverlap(starts, durations, ends, runOnResource, resourceId))

    starts(2).min shouldBe 0

    post(runOnResource(2) === resourceId)

    starts(2).min shouldBe 10

  }
}

class SmallNoOverlapTransitionTimeTest extends FunSuite with Matchers with Assertions {
  test("Small test to remove optional") {
    implicit val cp = CPSolver()
    val distances: Array[Array[Int]] = Array.tabulate(3, 3)((i,j) => if(i == j) 0 else 30)

    val starts: Array[CPIntVar] = Array.fill(3)(CPIntVar(0 until 50))
    val durations: Array[CPIntVar] = Array.fill(3)(CPIntVar(1))
    val ends: Array[CPIntVar] = Array.fill(3)(CPIntVar(1 to 50))
    val resources: Array[CPIntVar] = Array.fill(3)(CPIntVar(0, 1))

    for(a <- 0 until 3)
      post(starts(a) + durations(a) === ends(a))

    add(resources(0) === 1)
    add(resources(1) === 1)

    add(new NoOverlapTransitionTimes(starts, durations, ends, distances, resources, 1))

    //    add(starts(0) === 0)
    //    add(starts(1) === 31)
    //  add(resources(2) === 1)


    assert(!resources(2).hasValue(1))

    //println("starts:\n" + starts.mkString("\n"))
    //println("resources:\n" + resources.mkString("\n"))

  }
}

class SmallAlternativeResourcesTransitionTimeTest extends FunSuite with Matchers with Assertions {
  test("Small test for alternative resources interface") {
    implicit val cp = CPSolver()
    val distances: Array[Array[Int]] = Array.tabulate(4, 4)((i,j) => if(i == j) 0 else 30)

    val starts: Array[CPIntVar] = Array.fill(4)(CPIntVar(0 until 50))
    val durations: Array[CPIntVar] = Array.fill(4)(CPIntVar(1))
    val ends: Array[CPIntVar] = Array.fill(4)(CPIntVar(1 to 50))
    val resources: Array[CPIntVar] = Array.fill(4)(CPIntVar(0, 1))

    for(a <- 0 until 4)
      post(starts(a) + durations(a) === ends(a))

    add(resources(0) === 1)
    add(resources(1) === 1)
    add(starts(0) === 0)
    add(starts(2) === 0)

    add(new AlternativeResourcesTransitionTimes(starts, durations, ends, resources, Set(0, 1), distances))

    assert(!resources(2).hasValue(1))
    assert(!resources(3).hasValue(1))
    assertResult(31)(starts(1).min)
    assertResult(31)(starts(3).min)

  }
}