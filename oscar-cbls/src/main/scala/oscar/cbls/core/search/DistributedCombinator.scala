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

import oscar.cbls.core.distributed._

abstract class DistributedCombinator(neighborhoods:Array[Neighborhood],
                                     remoteTasks:Array[Int => RemoteTask] = Array()) extends Neighborhood {

  var remoteNeighborhoodIdentifications:Array[RemoteTaskIdentification] = null
  var remoteTaskIdentification:Array[RemoteTaskIdentification] = null
  var supervisor:Supervisor = null

  override val profiler: EmptyProfiler = new EmptyProfiler(this)

  override def labelAndExtractRemoteTasks(supervisor: Supervisor,
                                          currentID: Int,
                                          nbDistributedCombinators:Int = 0,
                                          acc: List[RemoteTask]): (Int, Int, List[RemoteTask]) = {
    this.supervisor = supervisor
    val (newID,newAcc,neighborhoods2) = labelAndExtractRemoteNeighborhoodsOutOf(currentID, acc, neighborhoods)
    remoteNeighborhoodIdentifications = neighborhoods2.map(_.remoteIdentification)

    val (newID2,newAcc2,neighborhoods3) = labelAndExtractRemoteTasksOutOf(
      currentID,
      acc = newAcc,
      remoteTaskGenerators = remoteTasks
    )
    remoteTaskIdentification = neighborhoods3.map(_.remoteIdentification)

    (newID2,nbDistributedCombinators+1,newAcc2)
  }

  private def labelAndExtractRemoteNeighborhoodsOutOf(currentID: Int,
                                                      acc: List[RemoteTask],
                                                      neighborhoods: Array[Neighborhood]):
  (Int, List[RemoteTask], Array[RemoteTask]) = {
    var currentIDNow: Int = currentID
    var accNow: List[RemoteTask] = acc
    val toReturnArray = neighborhoods.map(n => {
      val r:RemoteTask = new RemoteNeighborhood(currentIDNow, n)
      currentIDNow += 1
      accNow = r :: accNow
      r
    })
    (currentIDNow, accNow,toReturnArray)
  }

  private def labelAndExtractRemoteTasksOutOf(currentID:Int,
                                              acc:List[RemoteTask],
                                              remoteTaskGenerators:Array[Int => RemoteTask]):
  (Int, List[RemoteTask], Array[RemoteTask]) = {
    var currentIDNow: Int = currentID
    var accNow: List[RemoteTask] = acc
    val toReturnArray = remoteTaskGenerators.map(remoteTaskGenerator => {
      val r = remoteTaskGenerator(currentIDNow)
      currentIDNow += 1
      accNow = r :: accNow
      r
    })
    (currentIDNow, accNow,toReturnArray)
  }

  override def collectProfilingStatistics: List[Array[String]] = {
    remoteNeighborhoodIdentifications.flatMap(i => supervisor.getRemoteStatisticsFor(i)).toList
  }

  override def toString: String = "Distributed Combinator"
}
