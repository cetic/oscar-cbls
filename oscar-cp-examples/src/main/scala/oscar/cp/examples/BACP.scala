package oscar.cp.examples

import scala.io.Source
import oscar.cp._
import oscar.util._

/**
 * Balanced Academic Curriculum Problem
 * The BACP is to design a balanced academic curriculum by assigning periods to courses in a way that
 * the academic load of each period is balanced, i.e., as similar as possible . The curriculum must obey the following administrative and academic regulations:
 * Academic curriculum: an academic curriculum is defined by a set of courses and a set of prerequisite relationships among them.
 * Number of periods: courses must be assigned within a maximum number of academic periods.
 * Academic load: each course has associated a number of credits or units that represent the academic effort required to successfully follow it.
 * Prerequisites: some courses can have other courses as prerequisites.
 * Minimum academic load: a minimum amount of academic credits per period is required to consider a student as full time.
 * Maximum academic load: a maximum amount of academic credits per period is allowed in order to avoid overload.
 * Minimum number of courses: a minimum number of courses per period is required to consider a student as full time.
 * Maximum number of courses: a maximum number of courses per period is allowed in order to avoid overload.
 * The goal is to assign a period to every course in a way that
 * - the minimum and maximum academic load for each period,
 * - the minimum and maximum number of courses for each period,
 * - and the prerequisite relationships are satisfied.
 * An optimal balanced curriculum balances academic load for all periods.
 * @author Pierre Schaus pschaus@gmail.com
 */
object BACP extends CPModel with App {

  val lines = Source.fromFile("data/bacp/instancecsplib/instance12.txt").getLines.reduceLeft(_ + " " + _)
  val vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
  var index = 0
  def next() = {
    index += 1
    vals(index - 1)
  }

  val nbCourses = next()
  val courses = 0 until nbCourses
  val nbPeriods = next()
  val periods = 0 until nbPeriods
  val mincredit = next()
  val maxcredit = next()
  val nbPre = next()
  val credits = Array.fill(nbCourses)(next())
  val prerequisites = Array.fill(nbPre)((next(), next()))

  val x = Array.fill(nbCourses)(CPIntVar(periods))
  val l = Array.fill(nbPeriods)(CPIntVar(0 to credits.sum))
  val vari = CPIntVar(0 to 10000000)

  add(spread(l, credits.sum, vari))
  add(binPacking(x, credits, l))
  for ((i, j) <- prerequisites) {
    add(x(i) < x(j)) // precedence constraint
  }


  class ValueHeuristicLearner(x: Array[CPIntVar], defaultValueHeuristic: (Int => Int)){
    private[this] val lastValues = Array.fill(x.length)(Int.MinValue)

    x(0).store.onPush {
      for (i <- 0 until x.length) {
        if (x(i).isBound) {
          lastValues(i) = x(i).min
        }
      }
    }
    def valueHeuristic(i: Int): Int = {
      if (x(i).hasValue(lastValues(i))) {
        lastValues(i)
      } else {
        defaultValueHeuristic(i)
      }
    }
  }
  def learnValueHeuristic(x: Array[CPIntVar], defaultValueHeuristic: (Int => Int)): (Int => Int) = {
    new ValueHeuristicLearner(x,defaultValueHeuristic).valueHeuristic
  }

  val valueHeuris: (Int => Int) = learnValueHeuristic(x,i => selectMin(periods)(x(i).hasValue(_))(l(_).min).get)

  solver.obj(vari).relax()

  // Search
  minimize(vari) search {
    binaryFirstFailIdx(x, valueHeuris)
  }

  // Execution
  println(start())
}
