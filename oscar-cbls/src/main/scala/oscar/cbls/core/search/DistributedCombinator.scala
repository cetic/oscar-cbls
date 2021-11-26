package oscar.cbls.core.search

import oscar.cbls.core.distrib._

abstract class DistributedCombinator(neighborhoods:Array[Neighborhood]) extends Neighborhood {

  var remoteNeighborhoodIdentifications:Array[RemoteTaskIdentification] = null
  var supervisor:Supervisor = null

  override def labelAndExtractRemoteTasks(supervisor: Supervisor,
                                          currentID: Int,
                                          nbDistributedCombinators:Int = 0,
                                          acc: List[RemoteNeighborhood]): (Int, Int, List[RemoteNeighborhood]) = {
    this.supervisor = supervisor
    val (newID,newAcc,neighborhoods2) = labelAndExtractRemoteNeighborhoodsOutOf(currentID, acc, neighborhoods)
    remoteNeighborhoodIdentifications = neighborhoods2.map(_.remoteIdentification)
    (newID,nbDistributedCombinators+1,newAcc)
  }

  private def labelAndExtractRemoteNeighborhoodsOutOf(currentID:Int,
                                                      acc:List[RemoteNeighborhood],
                                                      neighborhoods:Array[Neighborhood]):
  (Int,List[RemoteNeighborhood],Array[RemoteNeighborhood]) = {
    var currentIDNow: Int = currentID
    var accNow: List[RemoteNeighborhood] = acc
    val toReturnArray = neighborhoods.map(n => {
      val r = new RemoteNeighborhood(currentIDNow, n)
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
