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
/******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Gaël Thouvenin
  ******************************************************************************/

package oscar.examples.cbls.old
/*
import oscar.cbls._
import oscar.cbls.lib.constraint.{EQ, Exactly}
import oscar.cbls.lib.invariant.logic.SelectLESetQueue
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.modeling.CBLSModel
import oscar.cbls.util.StopWatch

import scala.collection.immutable.SortedMap

object MagicSeries extends CBLSModel with App with StopWatch{

  //The size of the serie
  val size:Int = 20

  val range: Range = Range(0, size)

  val magicSeries = Array.tabulate(size)(w => CBLSIntVar(0, 0 until size, " n° of " + w ))

  //exactly constraint
  val bounds = SortedMap[Int, CBLSIntVar]((for(v <- range) yield v -> magicSeries(v)):_*)
  c.post(Exactly(magicSeries, bounds))

  //redundant constraint, to make the search procedure faster
  c.post(EQ(Sum( for(i <- range) yield i * magicSeries(i) ), size))

  var it: CBLSIntVar = CBLSIntVar(0, fullRange, "it")

  val tabuArray: Array[CBLSIntVar] = Array.tabulate(size)(w => CBLSIntVar(-1, fullRange, "tabu_of_" + w + " "))

  val nonTabuPositions = SelectLESetQueue(tabuArray, it).setName("non tabu positions")

  val maxs = argMax(Array.tabulate(size)(i => c.violation(magicSeries(i))), nonTabuPositions)

  val maxVal = maxs.setName("most violated positions")

  close()

  val tabuLength = 3

  val restartFreq = 300

  startWatch()

  //Search Procedure
  while(violation.value > 0)
  {
    val index = maxVal.value.firstKey

    var bestOccurrences = selectMin(range)(r => assignVal(magicSeries(index), r), r => magicSeries(index).value != r)

    magicSeries(index) := bestOccurrences
    tabuArray(index) := it.value + tabuLength
    it :+= 1
  }

  print("Solution found : \n(")
  for(i <- range) yield print(magicSeries(i).value + ", ")
  print(")\nin " + it.value + " iterations and " + getWatch + " ms")
}
*/