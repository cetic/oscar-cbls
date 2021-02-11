package oscar.cbls.core.distrib

import akka.actor.typed.ActorRef
import oscar.cbls.core.computation.{AbstractVariableSnapShot, Solution, Store}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search._

// ////////////////////////////////////////////////////////////

//le truc qu'on envoie au worker
case class RemoteNeighborhoodIdentification(neighborhoodID: Int, parameters: List[Long], neighborhoodName: String)

class RemoteNeighborhood(val neighborhoodID: Int, neighborhood: List[Long] => Neighborhood, neighborhoodName: String = "") {

  @volatile
  var bestObjSoFar:Long = Long.MaxValue

  def getMove(parameters: List[Long],
              obj: Objective,
              acc: (Long, Long) => Boolean,
              shouldAbort: () => Boolean,
              sendFullSolution:Boolean): IndependentSearchResult = {
    neighborhood(parameters).getMoveAbortable(obj, obj.value, acc, shouldAbort) match {
      case NoMoveFound => IndependentNoMoveFound()
      case MoveFound(m) =>
        if(sendFullSolution) {
          m.commit()
          IndependentMoveFound(LoadIndependentSolutionMove(
            objAfter = m.objAfter,
            neighborhoodName = m.neighborhoodName,
            IndependentSolution(obj.model.solution())))
        }else {
          IndependentMoveFound(m.getIndependentMove(obj.model))
        }
    }
  }

  def doAllMoves(parameters: List[Long],
                 obj: Objective,
                 acc: (Long, Long) => Boolean,
                 shouldAbort: () => Boolean,
                 searchId:Long,
                 sendProgressTo:Option[ActorRef[SearchProgress]]): IndependentSearchResult = {

    bestObjSoFar = obj.value
    var anyMoveFound = false
    var name:String = ""
    val delayForNextFeedbackMS = 100 // 0.1 second
    var nextTimeForFeedbackMS = System.currentTimeMillis() + delayForNextFeedbackMS

    val theNeighborhood = neighborhood(parameters)
    while(!shouldAbort() && (theNeighborhood.getMoveAbortable(obj, obj.value, acc, shouldAbort) match {
      case NoMoveFound => false
      case MoveFound(m) =>
        m.commit();
        if(!anyMoveFound){
          name = m.neighborhoodName
          anyMoveFound = true;
        }
        if(m.objAfter < bestObjSoFar){
          bestObjSoFar = m.objAfter
        }
        true
    })) {
      sendProgressTo match{
        case None => ;
        case Some(target) =>
          val currentTimeMs = System.currentTimeMillis()
          if (nextTimeForFeedbackMS <= currentTimeMs){
            target ! SearchProgress(searchId, obj.value)
            nextTimeForFeedbackMS = currentTimeMs + delayForNextFeedbackMS
          }
      }
    }

    if(anyMoveFound && !shouldAbort()){
      sendProgressTo match {
        case None => ;
        case Some(target) =>
          target ! SearchProgress(searchId, obj.value)
      }
      IndependentMoveFound(LoadIndependentSolutionMove(
        objAfter = obj.value,
        neighborhoodName = name,
        IndependentSolution(obj.model.solution())))
    }else {
      IndependentNoMoveFound()
    }
  }

  def getRemoteIdentification(parameters: List[Long] = Nil): RemoteNeighborhoodIdentification =
    RemoteNeighborhoodIdentification(neighborhoodID, parameters, neighborhoodName)
}

// ////////////////////////////////////////////////////////////

abstract class IndependentSearchResult {
  def getLocalResult(m: Store): SearchResult
}

case class IndependentMoveFound(move: IndependentMove) extends IndependentSearchResult {
  override def getLocalResult(m: Store): MoveFound = MoveFound(move.makeLocal(m))
  def objAfter:Long = move.objAfter
}

case class IndependentNoMoveFound() extends IndependentSearchResult {
  override def getLocalResult(m: Store): NoMoveFound.type = NoMoveFound
}

// ////////////////////////////////////////////////////////////

object IndependentSolution {
  def apply(solution: Solution): IndependentSolution = {
    new IndependentSolution(solution.saves)
  }
}

class IndependentSolution(saves: Iterable[AbstractVariableSnapShot]) {
  def makeLocal(s: Store): Solution =
    Solution(saves, s)
}

// ////////////////////////////////////////////////////////////

trait IndependentMove {
  def objAfter: Long

  def neighborhoodName: String

  def makeLocal(m: Store): Move
}

case class LoadIndependentSolutionMove(objAfter: Long, neighborhoodName: String, s: IndependentSolution)
  extends IndependentMove {
  override def makeLocal(m: Store): Move =
    LoadSolutionMove(
      s.makeLocal(m),
      objAfter = objAfter,
      neighborhoodName = neighborhoodName)
}
