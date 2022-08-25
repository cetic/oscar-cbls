/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cbls.core.search

import oscar.cbls.core.distrib.{RemoteTask, Supervisor}

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class NeighborhoodCombinator(nbs: Neighborhood*) extends Neighborhood {
  //this resets the internal state of the move combinators
  override def reset(): Unit = {
    for (n <- nbs) n.reset()
  }

  override def resetStatistics(): Unit ={
    for (n <- nbs) n.resetStatistics()
  }

  override def verbose_=(i: Int): Unit = {
    for (n <- nbs) n.verbose = i
    super.verbose_=(i)
  }

  override def toString: String = this.getClass.getSimpleName + "(" + nbs.mkString(",") + ")"

  override def collectProfilingStatistics: List[Array[String]] = nbs.flatMap(_.collectProfilingStatistics).toList

  override def labelAndExtractRemoteTasks(supervisor: Supervisor,
                                          currentID: Int,
                                          nbDistributedCombinators: Int = 0,
                                          acc: List[RemoteTask]): (Int, Int, List[RemoteTask]) = {
    var currentIDNow: Int = currentID
    var nbDistributedCombinatorsNow: Int = nbDistributedCombinators
    var accNow: List[RemoteTask] = acc
    for(nb <- nbs){
      val (currId, nbDC, accN) = nb.labelAndExtractRemoteTasks(supervisor, currentIDNow, nbDistributedCombinatorsNow, accNow)
      currentIDNow = currId
      nbDistributedCombinatorsNow = nbDC
      accNow = accN
    }
    (currentIDNow,nbDistributedCombinatorsNow,accNow)
  }
}



