package oscar.cbls.lib.search.combinators

import oscar.cbls.core.computation.Store
import oscar.cbls.core.distrib.{RemoteNeighborhood, SearchRequest, Supervisor, WorkGiverWrapper}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{MoveFound, Neighborhood, NoMoveFound, SearchResult}


abstract class DistributedCombinator(neighborhoods:Array[List[Long] => Neighborhood]) extends Neighborhood {

  var labeledRemoteNeighborhoods:Array[RemoteNeighborhood] = null
  var supervisor:Supervisor = null

  override def labelAndExtractRemoteNeighborhoods(supervisor: Supervisor, currentID: Int, acc: List[RemoteNeighborhood]): (Int, List[RemoteNeighborhood]) = {
    this.supervisor = supervisor
    val (newID,newAcc,neighborhoods2) = labelAndExtractRemoteNeighborhoodsOutOf(currentID, acc, neighborhoods)
    labeledRemoteNeighborhoods = neighborhoods2
    (newID,newAcc)
  }

  def labelAndExtractRemoteNeighborhoodsOutOf(currentID:Int,
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

    val moves = WorkGiverWrapper.andWrap(
      labeledRemoteNeighborhoods.map(l =>
        supervisor.delegateSearch(
          SearchRequest(
            l.getRemoteIdentification(Nil),
            acceptanceCriteria,
            independentObj,
            startSol))),
        obj.model,
        supervisor)

    val answers = moves.getResultWaitIfNeeded()
    
    val foundMoves = answers.get.flatMap({
      case NoMoveFound => None
      case m: MoveFound => Some(m)
    })

    if (foundMoves.isEmpty) NoMoveFound
    else foundMoves.minBy(_.objAfter)
  }
}


object Test{

  //supervisor side
  val m:Store = ???
  val n:Neighborhood = ???
  val supervisor:Supervisor = ???

  n.labelNeighborhoodsForRemoteOperation(supervisor)

  //worker side
  val m2:Store = ???
  val n2:Neighborhood = ???
  supervisor.createLocalWorker(n2.identifyNeighborhoodForWorker, m2)



  def createSearchProcedure():(Store,Neighborhood,Objective) = ???


  //supervisor side
  val (store,search,obj) = createSearchProcedure()
  val supervisor:Supervisor = ???
  search.labelNeighborhoodsForRemoteOperation(supervisor)

  //worker side
  val (store2,search2,_) = createSearchProcedure()
  supervisor.createLocalWorker(search2.identifyNeighborhoodForWorker, store2)

  search.doAllMoves(obj)

}