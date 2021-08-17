package oscar.cbls.core.distrib

import akka.actor.typed.ActorRef
import oscar.cbls.core.computation.{IndependentSerializableAbstractVariableSnapshot, Solution, Store}
import oscar.cbls.core.objective.{IndependentObjective, Objective}
import oscar.cbls.core.search._


// ////////////////////////////////////////////////////////////

case class SearchProgress(searchId:Long, obj:Long, timeMs:Long, aborted:Boolean = false)

case class SearchRequest(neighborhoodID: RemoteNeighborhoodIdentification,
                         acc: (Long, Long) => Boolean,
                         obj: IndependentObjective,
                         startSolutionOpt: Option[IndependentSolution],
                         sendFullSolution:Boolean = false,
                         doAllMoves:Boolean = false,
                         sendProgressTo:Option[ActorRef[SearchProgress]] = None) {
  override def toString: String = s"SearchRequest($neighborhoodID,$acc,$obj,sendFullSolution:$sendFullSolution)"
}

case class SearchTask(request: SearchRequest,
                      searchId: Long,
                      sendResultTo: ActorRef[SearchEnded]) {
  override def toString: String = s"SearchTask($request,$searchId,${sendResultTo.path})"
}
// ////////////////////////////////////////////////////////////

//le truc qu'on envoie au worker
case class RemoteNeighborhoodIdentification(neighborhoodID: Int, parameters: List[Long])

class RemoteNeighborhood(val neighborhoodID: Int, neighborhood: List[Long] => Neighborhood) {

  @volatile
  var bestObjSoFar:Long = Long.MaxValue

  def getMove(parameters: List[Long],
              obj: Objective,
              acc: (Long, Long) => Boolean,
              shouldAbort: () => Boolean,
              initSolutionOpt:Option[Solution],
              sendFullSolution:Boolean,
              searchId:Long,
              sendProgressTo:Option[ActorRef[SearchProgress]]): IndependentSearchResult = {
    neighborhood(parameters).getMoveAbortable(obj, obj.value, acc, shouldAbort,initSolutionOpt) match {
      case NoMoveFound => IndependentNoMoveFound()
      case MoveFound(m) =>
        sendProgressTo match {
          case None =>
          case Some(target) =>
            target ! SearchProgress(searchId, m.objAfter, System.currentTimeMillis())
        }
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
    var nextTimeForFeedbackMS = System.currentTimeMillis() - 2* delayForNextFeedbackMS

    var lastProgressOBj:Long = bestObjSoFar

    val theNeighborhood = neighborhood(parameters)
    while(!shouldAbort() && (theNeighborhood.getMoveAbortable(obj, obj.value, acc, shouldAbort) match {
      case NoMoveFound => false
      case MoveFound(m) =>
        m.commit()
        if(!anyMoveFound){
          name = m.neighborhoodName
          anyMoveFound = true
        }
        if(m.objAfter < bestObjSoFar){
          bestObjSoFar = m.objAfter
        }
        true
    })) {
      sendProgressTo match{
        case None =>
        case Some(target) =>
          val currentTimeMs = System.currentTimeMillis()
          if (nextTimeForFeedbackMS <= currentTimeMs){
            lastProgressOBj = obj.value
            target ! SearchProgress(searchId, lastProgressOBj, currentTimeMs)
            nextTimeForFeedbackMS = currentTimeMs + delayForNextFeedbackMS
          }
      }
    }

    if(anyMoveFound && !shouldAbort()){
      sendProgressTo match {
        case None =>
        case Some(target) =>
          target ! SearchProgress(searchId, obj.value, System.currentTimeMillis())
      }
      IndependentMoveFound(LoadIndependentSolutionMove(
        objAfter = obj.value,
        neighborhoodName = name,
        IndependentSolution(obj.model.solution())))
    }else {
      sendProgressTo match {
        case None =>
        case Some(target) =>
          target ! SearchProgress(searchId, lastProgressOBj, System.currentTimeMillis(), aborted=true)
      }
      IndependentNoMoveFound()
    }
  }

  def getRemoteIdentification(parameters: List[Long] = Nil): RemoteNeighborhoodIdentification =
    RemoteNeighborhoodIdentification(neighborhoodID, parameters)
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
    new IndependentSolution(solution.saves.map(_.makeIndependentSerializable),solution.saveNr)
  }
}

class IndependentSolution(saves: Iterable[IndependentSerializableAbstractVariableSnapshot], val solutionId:Int) {
  def makeLocal(s: Store): Solution = {
    require(saves.nonEmpty)
    Solution(saves.map(_.makeLocal), s, solutionId)
  }

  def isEmpty:Boolean = saves.isEmpty
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