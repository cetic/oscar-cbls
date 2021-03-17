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
import oscar.cp.testUtils._
import oscar.cp.constraints.{SoftGCCFWC, SoftGCCAC}

import scala.util.Random

/**
 * @author Victor Lecomte
 */
class TestSoftGCC extends TestSuite {

  val AC = 0
  val FWC = 1

  def nbSol(domX: Array[Set[Int]], values: Range, lower: Array[Int], upper: Array[Int], domViol: Set[Int],
            mode: Int): (Int, Long, Int, Int) = {
    val cp = CPSolver()

    val nVariables = domX.length
    val X = Array.tabulate(nVariables)(i => CPIntVar(domX(i))(cp))
    val viol = CPIntVar(domViol)(cp)

    try {
      if (mode == AC) {
        cp.add(new SoftGCCAC(X, values.min, lower, upper, viol))
      } else {
        cp.add(new SoftGCCFWC(X, values.min, lower, upper, viol))
      }
    } catch {
      case e: NoSolutionException => return (0, 0, 0, 0)
    }

    cp.search { binaryStatic(X) }

    val stat = cp.start(nSols = 10000000)
    (stat.nSols, stat.time, stat.nNodes, stat.nFails)
  }

  var rand: Random = null
  def randomDom(size: Int) = Array.fill(size)(rand.nextInt(size)).toSet

  // nSols on random domains
  test("Test #1: Random bounds") {
    var totalTime1 = 0L
    var totalTime2 = 0L
    var totalNodes1 = 0
    var totalNodes2 = 0

    for (i <- 1 to 10) {
      //println(s"test #$i")
      rand =  new scala.util.Random(i)

      val nVariables = 7

      val domVars = Array.fill(nVariables)(randomDom(size = 6))
      val domViol = (0 to rand.nextInt(6)).toSet
      /*for (i <- 0 until nVariables) {
        println(s"$i: ${domVars(i) mkString " "}")
      }*/

      val min = domVars.flatten.min
      val max = domVars.flatten.max

      val lower = (for (v <- min to max) yield rand.nextInt(3)).toArray
      val upper = lower.map(v => v+rand.nextInt(3))
      /*println(s"lower: ${lower.mkString(" ")}")
      println(s"upper: ${upper.mkString(" ")}")
      println(s"viol: $domViol")*/

      val (nSols1, time1, nNodes1, nFails1) = nbSol(domVars, min to max, lower, upper, domViol, AC)
      val (nSols2, time2, nNodes2, nFails2) = nbSol(domVars, min to max, lower, upper, domViol, FWC)
      totalTime1 += time1
      totalTime2 += time2
      totalNodes1 += nNodes1
      totalNodes2 += nNodes2
      //println(s"time: $time1, $time2")
      //println(s"nodes: $nNodes1, $nNodes2")

      nSols1 should be(nSols2)
    }
    //println(s"total time: ac $totalTime1, fwc $totalTime2")
    //println(s"total nodes: ac $totalNodes1, fwc $totalNodes2")
  }
}
