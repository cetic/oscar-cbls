package oscar.cbls.core.search

import oscar.cbls.core.distrib._

abstract class DistributedCombinator(neighborhoods:Array[Neighborhood],
                                     remoteTasks:Array[Int => RemoteTask] = Array()) extends Neighborhood {

  var remoteNeighborhoodIdentifications:Array[RemoteTaskIdentification] = null
  var remoteTaskIdentification:Array[RemoteTaskIdentification] = null
  var supervisor:Supervisor = null

  override def labelAndExtractRemoteTasks(supervisor: Supervisor,
                                          currentID: Int,
                                          nbDistributedCombinators:Int = 0,
                                          acc: List[RemoteTask]): (Int, Int, List[RemoteTask]) = {
    this.supervisor = supervisor
    val (newID,newAcc,neighborhoods2) = labelAndExtractRemoteNeighborhoodsOutOf(currentID, acc, neighborhoods)
    remoteNeighborhoodIdentifications = neighborhoods2.map(_.remoteIdentification)

    val (newID2,newAcc2,neighborhoods3) = labelAndExtractRemoteTasksOutOf(currentID:Int,
      acc = newAcc,
      remoteTaskGenerators = remoteTasks)
    remoteTaskIdentification = neighborhoods3.map(_.remoteIdentification)

    (newID2,nbDistributedCombinators+1,newAcc2)
  }

  private def labelAndExtractRemoteNeighborhoodsOutOf(currentID:Int,
                                                      acc:List[RemoteTask],
                                                      neighborhoods:Array[Neighborhood]):
  (Int,List[RemoteTask],Array[RemoteTask]) = {
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
  (Int,List[RemoteTask],Array[RemoteTask]) = {
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
}
