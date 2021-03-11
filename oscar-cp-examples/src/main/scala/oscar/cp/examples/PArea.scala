package oscar.cp.examples

import oscar.cp.{CPIntVar, CPModel, Weak, add, binPacking, binaryFirstFail, minimize, onSolution, start, startSubjectTo, sum}
import oscar.util.rand

import scala.io.Source

/**
 * Problem proposed in:
 *
 * Q.T. Bui, Q.P. Dung, Y. Deville.
 * Solving the Agricultural Land Allocation Problem by Constraint-Based Local Search,
 * 19th International Conference on Principles and Practice of Constraint Programming (CP 2013),
 * Uppsala, Sweden, October 2013. Lecture Notes in Computer Science, Springer, 2013.
 *
 *  PArea is the problem of computing the area of the parcel allocated to each household in each field.
 *  We do not consider any order between households.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object PArea extends CPModel with App {

  val lines = Source.fromFile("data/Parea/PArea_100_9.txt").getLines.reduceLeft(_ + " " + _)
  val vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
  var index = 0
  def next() = {
    index += 1
    vals(index - 1)
  }

  val nHouseholds = next()
  val nFields = next()

  val h = Array.fill(nHouseholds)(next())
  val c = Array.fill(nFields)(next())

  val x = Array.fill(nHouseholds)(CPIntVar(0 until nFields))

  val l = Array.fill(nFields)(CPIntVar(0 to h.sum))

  // record best current solution
  val xsol = Array.fill(nHouseholds)(0)

  val t0 = System.currentTimeMillis()

  onSolution {
    println("time:"+(System.currentTimeMillis()-t0))
    for (i <- 0 until nHouseholds) xsol(i) = x(i).min
  }

  val obj = sum(0 until nFields)(j => (l(j)-c(j)).abs)

  add(binPacking(x,h,l),Weak)

  // dominance rules (see PhD thesis of Q.T. Bui)
  for (j <- 0 until nFields) {


    add(l(j) > c(j)-h.max)


    add(l(j) < c(j)+h.max) // we would like to have c(j) + min_{h(i): x(i) = j} (need global constraint to express it efficiently)

  }

  // Search
  minimize(obj) search {
    binaryFirstFail(x,_.randomValue)
  }

  start(1) // find initial solution

  // LNS
  for (r <- 0 until 10000000) {
    if (r%10000 == 0) println(r)
    startSubjectTo(failureLimit = 100) {
      add(for (i <- 0 until nHouseholds; if rand.nextInt(100) < 70) yield (x(i) === xsol(i)))
    }
  }
}
