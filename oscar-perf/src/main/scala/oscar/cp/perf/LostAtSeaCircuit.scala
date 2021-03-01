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


/**
 * Searching for a lost ship at sea is a time sensitive task that requires skill and urgency.  
 * Finding a lost ship quickly means the difference between life and death.
 * The map in Figure 1 shows a section of ocean divided into 64 cells.  Somewhere in this grid, a ship has been lost.  
 * Each cell has a number that represents the probability of finding the lost ship when that cell is searched (based on last known position, ocean currents, and debris sightings).  For example, if you searched cell A1, you would have a 2% chance of finding the lost ship there.  
 * As the leader of the search and rescue team, your goal is to find the ship with all survivors.  
 * Unfortunately it takes you 1 day to search a cell and the lost sailors have only enough food and water to survive for 10 days.  
 * This allows you to search a total of 10 cells before the lost sailors perish.
 * You may start your search in any of the 64 cells.  
 * You are only allowed to move to adjacent cells (you cannot move diagonally) and you are not allowed to revisit any cells.  
 * Add up the percentages in the 10 cells you have searched to get the probability of finding the lost ship.
 * Question:  What is the greatest probability of finding the lost ship?
 * 
 * @author Elise Dupont & Pierre Schaus
 */
object LostAtSeaCircuit  {
	def main(args: Array[String]): Unit = {
		
      // input data with the probabilities
      
    val proba = Array(Array(3,0,0,3,2,2,2,3),
                        Array(3,3,3,1,2,2,1,2),
                        Array(0,2,0,1,2,3,2,0),
                        Array(1,1,0,3,2,1,1,0),
                        Array(1,1,3,3,1,2,2,2),
                        Array(0,2,3,3,3,0,2,3),
                        Array(2,3,2,2,2,2,1,1),
                        Array(2,1,2,2,2,2,1,3))
    def getLineCol(i: Int) = (i / 8, i % 8)

    def neighbors(i: Int) = {
      val (l, c) = getLineCol(i)
      def toInt(lc: (Int, Int)) = lc._1 * 8 + lc._2
      Set((l + 1, c), (l - 1, c), (l, c + 1), (l, c - 1)).filter { case (l, c) => (l >= 0 && l < 8 && c >= 0 && c < 8) }.map(toInt(_))
    }

    // --------------- model -------------------

    val cp = CPSolver()
    //cp.silent = true
    val succ = Array.tabulate(64)(i => CPIntVar(cp, neighbors(i)))

    val path = Array.fill(10)(CPIntVar(cp, 0 until 64)) // represent the path of length ten which is the solution

    val sol = Array.fill(10)(0)
    val prob = proba.flatten
    
    val obj = sum(0 until 10)(i => element(prob, path(i)))
    
    cp.add(obj >= 20)

    //cp.maximize(sum(0 until 10)(i => element(prob, path(i))))

    for (i <- 0 until 9) {
      cp.add(elementVar(succ, path(i), path(i + 1)), Strong)
    }
    cp.add(circuit(succ), Strong)
    
    
    
    cp.search {
      binaryStatic(path)
    } onSolution {
      //println(path.mkString(","))
      //(0 until 10).foreach(i => sol(i) = path(i).value) // record the best solution
    }
    println(cp.start())

      
      
	}
}
