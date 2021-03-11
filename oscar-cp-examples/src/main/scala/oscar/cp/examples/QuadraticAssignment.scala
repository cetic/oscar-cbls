package oscar.cp.examples

import oscar.cp.{CPIntVar, CPModel, Strong, add, allDifferent, branch, minimize, noAlternative, onSolution, post, start, sum}
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
 * @author Pierre Schaus pschaus@gmail.com
 */
object QuadraticAssignment extends CPModel with App {

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

  onSolution { println("solution" + x.mkString(",")) }

  // for each facilities, the location chosen for it
  val x = N map (v => CPIntVar(0 until n))

  add(allDifferent(x), Strong)

  minimize(sum(N, N)((i, j) => d(x(i))(x(j)) * w(i)(j))) search {
    selectMin(x)(y => !y.isBound)(y => y.size) match {
      case None => noAlternative
      case Some(y) => {
        val v = y.min
        branch(post(y === v))(post(y !== v))
      }
    }
  }

  val stats = start()
  println(stats)
}
