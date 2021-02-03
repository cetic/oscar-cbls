package oscar.cbls.lib.search.combinators

import oscar.cbls.core.distrib._
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{Move, MoveFound, Neighborhood, NoMoveFound, SearchResult}

import scala.util.Random

abstract class DistributedCombinator(neighborhoods:Array[List[Long] => Neighborhood]) extends Neighborhood {

  var remoteNeighborhoods:Array[RemoteNeighborhood] = null
  var supervisor:Supervisor = null

  def delegateSearchesStopAtFirst(searchRequests:Array[SearchRequest]):WorkGiver =
    supervisor.delegateSearchesStopAtFirst(searchRequests)

  def delegateSearch(searchRequest:SearchRequest):WorkGiver =
    supervisor.delegateSearch(searchRequest)

  def delegateWithAction(searchRequest:SearchRequest,onComplete:SearchEnded => Unit):WorkGiver =
    supervisor.delegateWithAction(searchRequest,onComplete)

  override def labelAndExtractRemoteNeighborhoods(supervisor: Supervisor,
                                                  currentID: Int,
                                                  acc: List[RemoteNeighborhood]): (Int, List[RemoteNeighborhood]) = {
    this.supervisor = supervisor
    val (newID,newAcc,neighborhoods2) = labelAndExtractRemoteNeighborhoodsOutOf(currentID, acc, neighborhoods)
    remoteNeighborhoods = neighborhoods2
    (newID,newAcc)
  }

  private def labelAndExtractRemoteNeighborhoodsOutOf(currentID:Int,
                                                      acc:List[RemoteNeighborhood],
                                                      neighborhoods:Array[List[Long] => Neighborhood]):
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
    super.collectProfilingStatistics
  }
}

class Remote(neighborhoods:Neighborhood)
  extends DistributedCombinator(Array(_ => neighborhoods)) {

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val startSol = IndependentSolution(obj.model.solution())

    val move = delegateSearch(
      SearchRequest(
        remoteNeighborhoods(0).getRemoteIdentification(Nil),
        acceptanceCriteria,
        independentObj,
        startSol))

    move.getResult
  }
}

class DistributedBest(neighborhoods:Array[Neighborhood])
  extends DistributedCombinator(neighborhoods.map(x => (y:List[Long]) => x)) {

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val startSol = IndependentSolution(obj.model.solution())

    val workGivers =
      remoteNeighborhoods.map(l =>
        delegateSearch(SearchRequest(
          l.getRemoteIdentification(Nil),
          acceptanceCriteria,
          independentObj,
          startSol)))

    val foundMoves = workGivers.flatMap(
      _.getResultWaitIfNeeded().get match{
      case NoMoveFound => None
      case m: MoveFound => Some(m)
    })

    if (foundMoves.isEmpty) NoMoveFound
    else foundMoves.minBy(_.objAfter)
  }
}


class DistributedFirst(neighborhoods:Array[Neighborhood], randomOrder:Boolean = true)
  extends DistributedCombinator(neighborhoods.map(x => (y:List[Long]) => x)) {

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val startSol = IndependentSolution(obj.model.solution())

    val searchRequests = remoteNeighborhoods.map(l =>
      SearchRequest(
        l.getRemoteIdentification(Nil),
        acceptanceCriteria,
        independentObj,
        startSol))

    val searchRequests2 = if (randomOrder) Random.shuffle(searchRequests.toList).toArray else searchRequests
    val move = delegateSearchesStopAtFirst(searchRequests2)

    move.getResult
  }
}
