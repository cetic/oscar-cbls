package oscar.cbls.core.distrib

import oscar.cbls.core.computation.{Solution, Store}
import oscar.cbls.core.objective.Objective
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
    neighborhood(parameters).getMove(new AbortableObjective(shouldAbort,obj),obj.value,acc)
  }

  def getRemoteIdentification(parameters:List[Long]=Nil):RemoteNeighborhoodIdentification =
    RemoteNeighborhoodIdentification(neighborhoodID,parameters,neighborhoodName)
}

// ////////////////////////////////////////////////////////////

abstract class IndependentSearchResult{
  def getLocalResult(m:Store):SearchResult
}
case class IndependentMoveFound(i:IndependentMove) extends IndependentSearchResult{
  override def getLocalResult(m: Store): SearchResult = MoveFound(new MoveWrapper(i,m))
}
case class IndependentNoMoveFound() extends IndependentSearchResult{
  override def getLocalResult(m: Store): SearchResult = NoMoveFound
}

// ////////////////////////////////////////////////////////////

trait IndependentMove{
  def commit(m:Store): Unit
  def makeIntoLocal(m:Store):Move = new MoveWrapper(this,m)
  def objAfter:Long
  def neighborhoodName:String
}

case class LoadIndependentSolutionMove(objAfter:Long, neighborhoodName: String, s:Solution)
  extends IndependentMove{
  def commit(m:Store): Unit = {
    s.restoreDecisionVariables(m)
  }
}

class MoveWrapper(i:IndependentMove,m:Store)
  extends Move(i.objAfter,i.neighborhoodName){
  override def commit(): Unit = i.commit(m)
  override def toString: String = s"MoveWrapper($i)"
}

class AbortException extends Exception("")
class AbortableObjective(shouldAbort:()=>Boolean, baseObj:Objective) extends Objective{
  override def detailedString(short: Boolean, indent: Long): String = baseObj.detailedString(short,indent)

  override def model: Store = baseObj.model

  override def value: Long = {
    if(shouldAbort()) throw new AbortException()
    baseObj.value
  }
}
