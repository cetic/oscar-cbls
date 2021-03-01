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

package oscar.cp.perf



import oscar.cp._


object RCPSP2 extends App {

    
  // data/rcpsp/j60/j60_17_8.rcp interesting instance since we close it as lcg
  // data/rcpsp/j60/j60_17_8.rcp
  val resourceid = 1
  val (nTasks, nRes, resourcesCapacities, taskDescriptions) = RCPReader.readInstance("data/rcpsp/j30/j30_1_1.rcp")

  
  val durationsData = taskDescriptions.map(_._1)
  val horizon = durationsData.sum
  val demandsData = taskDescriptions.map(_._2)
  val successorsData = taskDescriptions.map(_._3)

  val taskIds = 0 until nTasks
  val successors = Array.tabulate(nTasks)(t => successorsData(t))
  val resIds = 0 until nRes

  implicit val cp = CPSolver()
  //cp.silent=true

  //vars
  val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durations(t))
  val demands = Array.tabulate(nRes)(r => Array.tabulate(nTasks)(t => CPIntVar(demandsData(t)(r))))
  val resources = Array.tabulate(nTasks)(t => CPIntVar(resourceid))
  val capacities = Array.tabulate(nRes)(r => CPIntVar(resourcesCapacities(r)))

  // Constraints	
  // -----------------------------------------------------------------------
  val makespan = maximum(ends)

  // Consistency 
  for (t <- taskIds)
    cp.add(ends(t) === starts(t) + durations(t))

  var best = Int.MaxValue 
  
  onSolution {
    best = makespan.value
  }

  // Cumulative
  for (r <- resIds) {
    add(maxCumulativeResource(starts, durations, ends, demands(r), resources, capacities(r), 1), Medium)
  }

  //Precedence
  for (t <- taskIds; succ <- successors(t)) {
    println(t + " <= " + succ)
    cp.add(ends(t) <= starts(succ))
  }

  // Search
  // -----------------------------------------------------------------------

    
    
  minimize(makespan)
  search {
    setTimes(starts, durations, ends,-durations(_).min)
  }
  val stat = start()
  println(stat)
}

import scala.io.Source

/*
 * Reader used for .rcp instances 
 */

object RCPReader {

  /*nbAct nbRes
   *CapaList
   * Line : qctivity description (dur (requirementList) nbSuccessors (successorList))  
   */
  def readInstance(dataFile: String) = {

    var lines = Source.fromFile(dataFile).getLines.toList.filter(_ != "")

    //first line : nbAct and nbRes
    val firstLineNumbers = lines.head.split(" ")
    val (nbAct, nbRes) = (firstLineNumbers(0).toInt - 2, firstLineNumbers(1).toInt) //we remove the 2 dummy activities
    val (actIndices, resIndices) = (0 until nbAct, 0 until nbRes)

    lines = lines.drop(1)

    //second line : resources capacities
    val secondLineNumbers = lines.head.split(" ")
    val resourcesCapacities = new Array[Int](nbRes)
    for (i <- resIndices)
      resourcesCapacities(i) = secondLineNumbers(i).toInt

    lines = lines.drop(1)

    //next lines are activities descriptions

    //skip the first dummy activity
    lines = lines.drop(1)

    val activityDescriptions = new Array[Tuple3[Int, Array[Int], Array[Int]]](nbAct)
    for (i <- actIndices) {
      val lineNumbers = lines.head.split(" ")

      val requirementArray = new Array[Int](nbRes)
      for (r <- resIndices)
        requirementArray(r) = lineNumbers(1 + r).toInt

      val successors = lineNumbers.drop(nbRes + 2).map(_.toInt - 2).filter(_ != nbAct) //remove 2 from the successors id and get rid of the dummy last activity in the successors

      //(dur (requirementArray) (successorArray)
      activityDescriptions(i) = Tuple3(lineNumbers(0).toInt, requirementArray, successors)

      //next line
      lines = lines.drop(1)
    }

    (nbAct, nbRes, resourcesCapacities, activityDescriptions)
  }

}

