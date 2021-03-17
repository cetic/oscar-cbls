package oscar.cp.examples

import oscar.cp.{CPIntVar, CPModel, Strong, add, allDifferent, branch, minimize, noAlternative, onSolution, post, start, startSubjectTo, sum}
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.PropagationGuidedRelax
import oscar.util.selectMin

import scala.io.Source

/**
 * Quadratic Assignment Problem:
 * There are a set of n facilities and a set of n locations.
 * For each pair of locations, a distance is specified and
 * for each pair of facilities a weight or flow is specified
 * (e.g., the amount of supplies transported between the two facilities).
 * The problem is to assign all facilities to different locations
 * with the goal of minimizing the sum of the distances multiplied by the corresponding flows.
 *
 * Uses a Propagation Guided Relaxation (see Propagation Guided Large Neighborhood Search - Perron 2004)
 *
 * @author Charles Thomas cftmthomas@gmail.com
 * @author Pierre Schaus pschaus@gmail.com
 */
object QuadraticAssignmentLNSPropGuided extends CPModel with App {

  // Read the data
  var lines = Source.fromFile("data/qap.txt").getLines.toList.filter(_ != "")
  val n = lines.head.toInt
  val N = 0 until n
  lines = lines.drop(1)
  var w: Array[Array[Int]] = Array() //weight matrix
  var d: Array[Array[Int]] = Array() //distance matrix
  for (i <- N) {
    w = w :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
    lines = lines.drop(1)
  }
  for (i <- N) {
    d = d :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
    lines = lines.drop(1)
  }


  // for each facilities, the location chosen for it
  val x = N map (v => CPIntVar(0 until n))

  solver.addDecisionVariables(x)

  add(allDifferent(x), Strong)

  val propGuided = new PropagationGuidedRelax(x) //Initialisation of the propagation guided

  minimize(sum(N, N)((i, j) => d(x(i))(x(j)) * w(i)(j))) search {
    selectMin(x.indices)(i => !x(i).isBound)(i => x(i).size) match {
      case None => noAlternative
      case Some(i) => {
        val y = x(i)
        val v = y.min
        branch{
          post(y === v)
          propGuided.updateCloseness(i) //Updating closeness metric when assigning variable (this is optional but allows to gain more information for the reverse PGLNS)
        }(post(y !== v))
      }
    }
  }

  var currentSol = new CPIntSol(Array(), Int.MaxValue, -1)

  onSolution {
    println("solution: " + x.mkString(","))
    println("objective: " + solver.objective.objs.head.best)
    currentSol = new CPIntSol(x.map(_.value).toArray, solver.objective.objs.head.best, -1)
  }

  // Search for an initial solution
  start(nSols = 1)

  val rand = new scala.util.Random(0)
  var limit = 100 // set the limit to 100 backtracks for LNS restarts
  val relaxSize = x.map(v => math.log(v.size)).sum * 0.5 //Desired size for the neighbourhood to relax
  val iters = 200 //Number of LNS iterations
  val reversePGLNSRatio = 0.5 //Probably to select the reverse PGLNS when used conjointly with PGLNS

  //LNS Search with random relaxation:
  for (r <- 1 to iters) {
    val stat = startSubjectTo(failureLimit = limit) {
//      add(N.filter(i => rand.nextInt(100) < 50).map(i => x(i) === currentSol.values(i))) // Random relaxation

//      propGuided.propagationGuidedRelax(solver, currentSol, relaxSize) // Propagation Guided Relaxation

//      propGuided.reversePropagationGuidedRelax(solver, currentSol, relaxSize) // reverse Propagation Guided Relaxation

      //PGLNS and reverse PGLNS used conjointly:
      if(rand.nextDouble() <= reversePGLNSRatio) propGuided.propagationGuidedRelax(solver, currentSol, relaxSize, updateCloseness = true)
      else propGuided.reversePropagationGuidedRelax(solver, currentSol, relaxSize)
    }
    // adapt the backtrack limit for next run *2 is previous run reached the limit /2 otherwise
    limit = if (stat.completed) limit / 2 else limit * 2
    println("set limit to " + limit)
  }
}
