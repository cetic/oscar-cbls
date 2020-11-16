package oscar.cbls.core.distrib

import oscar.cbls.core.computation.{AbstractVariableSnapShot, Solution, Store}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search._

// ////////////////////////////////////////////////////////////

abstract class IndependentOBj{
  def convertToOBj(m:Store):Objective
}

// ////////////////////////////////////////////////////////////

//le truc qu'on envoie au worker
case class RemoteNeighborhoodIdentification(neighborhoodID:Int, parameters:List[Long], neighborhoodName:String)

class RemoteNeighborhood(val neighborhoodID:Int, neighborhood:List[Long] => Neighborhood, neighborhoodName:String = ""){
  def explore(parameters:List[Long], obj:Objective, acc:(Long,Long) => Boolean, shouldAbort:() => Boolean):IndependentSearchResult = {
    neighborhood(parameters).getMoveAbortable(obj,obj.value,acc,shouldAbort) match{
      case NoMoveFound => IndependentNoMoveFound()
      case MoveFound(m) => IndependentMoveFound(m.getIndependentMove(obj.model))
    }
  }

  def getRemoteIdentification(parameters:List[Long]=Nil):RemoteNeighborhoodIdentification =
    RemoteNeighborhoodIdentification(neighborhoodID,parameters,neighborhoodName)
}

// ////////////////////////////////////////////////////////////

abstract class IndependentSearchResult {
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
  def makeLocal(s:Store):Solution =
    Solution(saves,s)
}
// ////////////////////////////////////////////////////////////

trait IndependentMove {
  def objAfter:Long
  def neighborhoodName:String
  def makeLocal(m:Store):Move
}

case class LoadIndependentSolutionMove(objAfter:Long, neighborhoodName: String, s:IndependentSolution)
  extends IndependentMove {
  override def makeLocal(m:Store):Move =
    LoadSolutionMove(
      s.makeLocal(m),
      objAfter = objAfter,
    neighborhoodName = neighborhoodName)
}
