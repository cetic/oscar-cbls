package oscar.cbls.core.distrib

import oscar.cbls.core.computation.{AbstractVariableSnapShot, Solution, Store}
import oscar.cbls.core.objective.{AbortableObjective, Objective}
import oscar.cbls.core.search.{Move, MoveFound, Neighborhood, NoMoveFound, SearchResult}

// ////////////////////////////////////////////////////////////

abstract class IndependentOBj{
  def convertToOBj(m:Store):Objective
}

// ////////////////////////////////////////////////////////////

//le truc qu'on envoie au worker
case class RemoteNeighborhoodIdentification(neighborhoodID:Int, parameters:List[Long], neighborhoodName:String)

class RemoteNeighborhood(val neighborhoodID:Int, neighborhood:List[Long] => Neighborhood, neighborhoodName:String = ""){
  def explore(parameters:List[Long], obj:Objective, acc:(Long,Long) => Boolean, shouldAbort:() => Boolean):SearchResult = {
    neighborhood(parameters).getMoveAbortable(obj,obj.value,acc,shouldAbort)
  }

  def getRemoteIdentification(parameters:List[Long]=Nil):RemoteNeighborhoodIdentification =
    RemoteNeighborhoodIdentification(neighborhoodID,parameters,neighborhoodName)
}

// ////////////////////////////////////////////////////////////

abstract class IndependentSearchResult{
  def getLocalResult(m:Store):SearchResult
}
case class IndependentMoveFound(i:IndependentMove) extends IndependentSearchResult{
  override def getLocalResult(m: Store): SearchResult = MoveFound(i.makeLocal(m))
}
case class IndependentNoMoveFound() extends IndependentSearchResult{
  override def getLocalResult(m: Store): SearchResult = NoMoveFound
}

// ////////////////////////////////////////////////////////////


object IndependentSolution{
  def apply(solution:Solution):IndependentSolution = {
    new IndependentSolution(solution.saves)
  }
}

class IndependentSolution(saves:Iterable[AbstractVariableSnapShot]){
  def makeLocal(s:Store):Solution = {
    new Solution(saves,s)
  }
}
// ////////////////////////////////////////////////////////////

trait IndependentMove{
  def commit(m:Store): Unit
  def objAfter:Long
  def neighborhoodName:String
  def makeLocal(m:Store):Move = new MoveWrapper(this,m)
}

case class LoadIndependentSolutionMove(objAfter:Long, neighborhoodName: String, s:IndependentSolution)
  extends IndependentMove{
  def commit(m:Store): Unit = {
    s.makeLocal(m).restoreDecisionVariables()
  }
}

class MoveWrapper(i:IndependentMove,m:Store)
  extends Move(i.objAfter,i.neighborhoodName){
  override def commit(): Unit = i.commit(m)
  override def toString: String = s"MoveWrapper($i)"
}
