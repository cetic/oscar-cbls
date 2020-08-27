package oscar.cbls.lib.search.combinators

import oscar.cbls.core.computation.Store
import oscar.cbls.core.distrib.{AndWorkGiver, RemoteNeighborhood, SearchRequest, SingleWorkGiver, Supervisor, WorkGiver, WorkerActor}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{MoveFound, Neighborhood, NoMoveFound, SearchResult}


abstract class DistributedCombinator(neighborhoods:Array[List[Long] => Neighborhood]) extends Neighborhood {

  var labeledRemoteNeighborhoods:Array[RemoteNeighborhood] = null
  var supervisor:Supervisor = null

  def delegateSearches(searchRequests:Array[SearchRequest]):AndWorkGiver =
    supervisor.delegateSearches(searchRequests)

  def delegateSearchesStopAtFirst(searchRequests:Array[SearchRequest]):SingleWorkGiver =
    supervisor.delegateSearchesStopAtFirst(searchRequests)

  def delegateSearch(searchRequest:SearchRequest):SingleWorkGiver =
    supervisor.delegateSearch(searchRequest)

  override def labelAndExtractRemoteNeighborhoods(supervisor: Supervisor, currentID: Int, acc: List[RemoteNeighborhood]): (Int, List[RemoteNeighborhood]) = {
    this.supervisor = supervisor
    val (newID,newAcc,neighborhoods2) = labelAndExtractRemoteNeighborhoodsOutOf(currentID, acc, neighborhoods)
    labeledRemoteNeighborhoods = neighborhoods2
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
}


class DistributedBest(n:Array[Neighborhood]) extends DistributedCombinator(n.map(x => (y:List[Long]) => x)) {

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val startSol = obj.model.solution().independentSolution

    val moves = delegateSearches(
      labeledRemoteNeighborhoods.map(l =>
        SearchRequest(
          l.getRemoteIdentification(Nil),
          acceptanceCriteria,
          independentObj,
          startSol)))

    val answers = moves.getResultWaitIfNeeded()

    val foundMoves = answers.get.flatMap({
      case NoMoveFound => None
      case m: MoveFound => Some(m)
    })

    if (foundMoves.isEmpty) NoMoveFound
    else foundMoves.minBy(_.objAfter)
  }
}


class DistributedFirst(n:Array[Neighborhood]) extends DistributedCombinator(n.map(x => (y:List[Long]) => x)) {

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val startSol = obj.model.solution().independentSolution

    val move = delegateSearchesStopAtFirst(
      labeledRemoteNeighborhoods.map(l =>
        SearchRequest(
          l.getRemoteIdentification(Nil),
          acceptanceCriteria,
          independentObj,
          startSol)))

    move.getResultWaitIfNeeded().get
  }
}

object MultiCoreOptimizingWithOscaRcbls{
  def createSearchProcedure():(Store,Neighborhood,Objective) = {

    //sreate the store, variables invariants, obj

    //m.close

    //the search procedure, using combinators and some distributed combinators (to be developed)

    //(m,search,obj)
    ???
  }

  //supervisor side
  val (store,search,obj) = createSearchProcedure()
  val supervisor:Supervisor = Supervisor.startSupervisorAndActorSystem(store,search)

  for(workerID <- (0 until WorkerActor.nbCores/2).par) {
    //worker side
    val (store2, search2, _) = createSearchProcedure()
    supervisor.createLocalWorker(store2,search2)
  }

  search.doAllMoves(obj = obj)
}

