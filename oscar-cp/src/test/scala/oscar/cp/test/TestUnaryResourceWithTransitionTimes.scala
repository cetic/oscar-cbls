package oscar.cp.test

import org.scalatest.{Assertions, Matchers, FunSuite}
import oscar.cp._
import oscar.cp.constraints.UnaryResourceWithTransitionTimes
import oscar.cp.core.variables.CPIntVar

import scala.util.Random

/**
 * Created on 21/01/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 * @author Sascha Van Cauwelaert (sascha.vancauwelaert@gmail.com)
 */
class TestUnaryResourceWithTransitionTimes extends FunSuite with Matchers with Assertions {
  val seed = 42
  val randGen = new scala.util.Random(seed)

  test("Random instance of size 2") {
    for (i <- 0 until 10) {
      val nActivities = 2
      val minTT = randGen.nextInt(20)
      val maxTT = minTT + randGen.nextInt(20)
      val minDuration = randGen.nextInt(30)
      val maxDuration = minDuration + randGen.nextInt(30)
      val intervalMultiplicator = math.max(1, nActivities / 5)
      val intervalProba = 0.0
      val (durations, triMatrix, actiBounds) = randomInstance(nActivities, minTT, maxTT, minDuration, maxDuration, intervalMultiplicator, intervalProba)

      if (i > 0) {
        val (stat1, stat2) = compareWithDecomp(nActivities, durations, triMatrix, actiBounds)

        assert(stat2.nSols == stat1.nSols)
        assert(stat2.nFails <= stat1.nFails)
        assert(stat2.nNodes <= stat1.nNodes)
      }
    }
  }

  test("Random instance of size 3") {
    for (i <- 0 until 10) {
      val nActivities = 3
      val minTT = randGen.nextInt(20)
      val maxTT = minTT + randGen.nextInt(20)
      val minDuration = randGen.nextInt(30)
      val maxDuration = minDuration + randGen.nextInt(30)
      val intervalMultiplicator = math.max(1, nActivities / 5)
      val intervalProba = 0.0
      val (durations, triMatrix, actiBounds) = randomInstance(nActivities, minTT, maxTT, minDuration, maxDuration, intervalMultiplicator, intervalProba)

      val (stat1, stat2) = compareWithDecomp(nActivities, durations, triMatrix, actiBounds)

      assert(stat2.nSols == stat1.nSols)
      assert(stat2.nFails <= stat1.nFails)
      assert(stat2.nNodes <= stat1.nNodes)
    }
  }

  test("Random instance of size 4") {
    for(i <- 0 until 5) {
      val nActivities = 4
      val minTT = randGen.nextInt(20)
      val maxTT = minTT + randGen.nextInt(20)
      val minDuration = randGen.nextInt(30)
      val maxDuration = minDuration + randGen.nextInt(30)
      val intervalMultiplicator = math.max(1, nActivities / 5)
      val intervalProba = 0.0
      val (durations, triMatrix, actiBounds) = randomInstance(nActivities, minTT, maxTT, minDuration, maxDuration, intervalMultiplicator, intervalProba)

      val (stat1, stat2) = compareWithDecomp(nActivities, durations, triMatrix, actiBounds)

      assert(stat2.nSols == stat1.nSols)
      assert(stat2.nFails <= stat1.nFails)
      assert(stat2.nNodes <= stat1.nNodes)
    }
  }

  def compareWithDecomp(nActivities : Int, durations : Array[Int], ttMatrix : Array[Array[Int]], actiBounds: Array[(Int, Int)], numSolutions : Int = Int.MaxValue) = {
    val horizon = durations.sum + ttMatrix.map(arr => arr.max).max * (nActivities - 1)
    val startMins = actiBounds.map(_._1)
    val endMaxs = actiBounds.map(_._2)

    /**************************************************************************
      ************************     BINARY RESOLUTION     ***********************
      **************************************************************************/
    val basicSolver = CPSolver()
    basicSolver.silent = true

    val durationVars = Array.tabulate(nActivities)(i => CPIntVar(durations(i))(basicSolver))
    val startVars = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) to endMaxs(i) - durations(i))(basicSolver))
    val endVars = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) + durations(i) to endMaxs(i))(basicSolver))

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

    basicSolver.search {
      binaryStatic(startVars)
    }

    val stat1 = basicSolver.start(numSolutions)

    /**************************************************************************
      ************************     SMARTER RESOLUTION     **********************
      **************************************************************************/

    val augmentedSolver = CPSolver()
    augmentedSolver.silent = true

    val durationVars2 = Array.tabulate(nActivities)(i => CPIntVar(durations(i))(augmentedSolver))
    val startVars2 = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) to endMaxs(i) - durations(i))(augmentedSolver))
    val endVars2 = Array.tabulate(nActivities)(i => CPIntVar(startMins(i) + durations(i) to endMaxs(i))(augmentedSolver))

    for (i <- 0 until nActivities) {
      augmentedSolver.add(startVars2(i) + durationVars2(i) === endVars2(i))
    }

    augmentedSolver.add(new UnaryResourceWithTransitionTimes(startVars2, durationVars2, endVars2, ttMatrix))

    augmentedSolver.search {
      binaryStatic(startVars2)
    }
    val stat2 = augmentedSolver.start(numSolutions)

    (stat1, stat2)
  }

  def randomInstance(nActivities : Int, minTT : Int, maxTT : Int, minDuration : Int, maxDuration : Int, intervalMultiplicator: Int, intervalProba: Double) =  {

    val triMatrix = getMatrix(nActivities, minTT, maxTT,randGen)

    val durations = Array.tabulate(nActivities)(i => minDuration + randGen.nextInt(maxDuration - minDuration + 1))
    val maxTransitions = triMatrix.map(e => e.max)
    val horizon = durations.sum + maxTransitions.max * (nActivities - 1)
    val actiBounds = Array.tabulate(nActivities)(i => {
      if (intervalProba > randGen.nextDouble) {
        val intervalWidth = (durations(i) + maxTransitions(i)) * intervalMultiplicator
        val startMin = randGen.nextInt(horizon - intervalWidth)
        val endMax = startMin + intervalWidth
        (startMin, endMax)
      }
      else {
        (0, horizon)
      }
    })
    (durations, triMatrix, actiBounds)
  }

  def getMatrix(nState : Int, minTransition : Int, maxTransition : Int, randGen : Random, timeLimitToFindTransitionMatrix : Int = Int.MaxValue) :Array[Array[Int]] = {
    val transitionTimes = Array.tabulate(nState, nState)((i, j) => if (i != j) minTransition + randGen.nextInt(maxTransition - minTransition + 1) else 0)
    floydWarshall(transitionTimes)
  }

  /*
   * Floyd–Warshall algorithm
   *
   * Given a potentially non-triangular matrix, outputs a triangular version of it
   */
  def floydWarshall(matrix : Array[Array[Int]]) : Array[Array[Int]] = {
    val n = matrix.length
    assert(matrix.forall(_.length == n))

    val dist = matrix.map(_.clone)
    var i, j, k = 0
    while (k < n) {
      i = 0
      while (i < n) {
        j = 0
        while (j < n) {
          if (dist(i)(j) > dist(i)(k) + dist(k)(j)) {
            dist(i)(j) = dist(i)(k) + dist(k)(j)
          }
          j += 1
        }
        i += 1
      }
      k += 1
    }
    dist
  }
}
