package oscar.cbls.core.distrib

import akka.actor.typed.ActorRef
import oscar.cbls.core.computation.{IndependentSerializableAbstractVariableSnapshot, Solution, Store}
import oscar.cbls.core.objective.{IndependentObjective, Objective}
import oscar.cbls.core.search._


// ////////////////////////////////////////////////////////////

case class SearchProgress(searchId:Long, obj:Long, timeMs:Long, aborted:Boolean = false)

abstract sealed class SearchRequest{
  def startSolutionOpt: Option[IndependentSolution]
  def dropStartSolution:SearchRequest
  def neighborhoodIdOpt:Option[RemoteNeighborhoodIdentification]
}

case class SingleMoveSearch(neighborhoodID: RemoteNeighborhoodIdentification,
                            acc: (Long, Long) => Boolean,
                            obj: IndependentObjective,
                            sendFullSolution:Boolean = false,
                            startSolutionOpt: Option[IndependentSolution]) extends SearchRequest{
  override def toString: String = s"SingleMoveSearch($neighborhoodID,$acc,$obj,sendFullSolution:$sendFullSolution)"

  override def dropStartSolution: SearchRequest = this.copy(startSolutionOpt = None)

  override def neighborhoodIdOpt: Option[RemoteNeighborhoodIdentification] = Some(neighborhoodID)
}

case class DoAllMoveSearch(neighborhoodID: RemoteNeighborhoodIdentification,
                           acc: (Long, Long) => Boolean,
                           obj: IndependentObjective,
                           startSolutionOpt: Option[IndependentSolution],
                           sendFullSolution:Boolean = false,
                           sendProgressTo:Option[ActorRef[SearchProgress]] = None) extends SearchRequest {
  override def toString: String = s"DoAllMoveSearch($neighborhoodID,$acc,$obj,sendFullSolution:$sendFullSolution)"

  override def dropStartSolution: SearchRequest = this.copy(startSolutionOpt = None)

  override def neighborhoodIdOpt: Option[RemoteNeighborhoodIdentification] = Some(neighborhoodID)
}


case class SearchTask(request: SearchRequest,
                      searchId: Long,
                      sendResultTo: ActorRef[SearchEnded]) {
  override def toString: String = s"SearchTask($request,$searchId,${sendResultTo.path})"
}
// ////////////////////////////////////////////////////////////

//le truc qu'on envoie au worker
case class RemoteTaskIdentification(taskId: Int, description:String)

abstract class RemoteTask[TaskMessage](val taskId: Int, description:String) {

  def doTask(taskMessage:Any): Unit ={
     internalDoTask(taskMessage.asInstanceOf[TaskMessage])
  }
  def internalDoTask(taskMessage:TaskMessage):Unit

  def getRemoteIdentification: RemoteTaskIdentification =
    RemoteTaskIdentification(taskId,description)

}

class RemoteNeighborhood(val neighborhoodID: Int, val neighborhood:Neighborhood)
  extends RemoteTask[SearchRequest](neighborhoodID: Int,neighborhood.toString){

  @volatile
  var bestObjSoFar:Long = Long.MaxValue

  private def internalDoTask(searchRequest: SearchRequest): (IndependentSearchResult,Int) = {
    searchRequest match{
      case s:SingleMoveSearch => doSingleMoveSearch(s,searchId)
      case d:DoAllMoveSearch =>doDoAllMoveSearch(d,searchId)
      //TODO: more search tasks should ne supported, bu I do not know yet how to do it cleanly
    }
  }


  private def loadSolutionOpt(startSolOpt:Option[IndependentSolution]) : Option[Solution] = {
    startSolOpt match {
      case None =>
        require(currentModelNr.isDefined)
        None
      case Some(startSolution) =>
        if (this.currentModelNr.isEmpty || startSolution.solutionId != this.currentModelNr.get) {
          val s = startSolution.makeLocal(m)
          s.restoreDecisionVariables(withoutCheckpoints = true) //TODO: we should only transmit the delta, eventually
          currentModelNr = Some(startSolution.solutionId)
          Some(s)
        } else {
          None
        }
    }
  }

  private def doSingleMoveSearch(searchRequest: SingleMoveSearch,searchId:Long) : (IndependentSearchResult,Int) = {
    val initLocalSolutionOpt:Option[Solution] = loadSolutionOpt(searchRequest.startSolutionOpt)

    shouldAbortComputation = false

    val neighborhood = neighborhoods(searchRequest.neighborhoodID.neighborhoodID)
    currentNeighborhood = neighborhood

    val startTime = System.currentTimeMillis()
    val result = neighborhood.getMove(
      searchRequest.obj.convertToObjective(m),
      searchRequest.acc,
      shouldAbort = () => shouldAbortComputation,
      initSolutionOpt = initLocalSolutionOpt,
      sendFullSolution = searchRequest.sendFullSolution,
      searchId = searchId,
      sendProgressTo = None)

    (result,(System.currentTimeMillis() - startTime).toInt)
  }

  private def doDoAllMoveSearch(searchRequest:DoAllMoveSearch, searchId:Long) : (IndependentSearchResult,Int) = {

    loadSolutionOpt(searchRequest.startSolutionOpt)

    shouldAbortComputation = false

    val neighborhood = neighborhoods(searchRequest.neighborhoodID.neighborhoodID)
    currentNeighborhood = neighborhood

    val startTime = System.currentTimeMillis()
    val result = neighborhood.doAllMoves(
      searchRequest.obj.convertToObjective(m),
      searchRequest.acc,
      shouldAbort = () => shouldAbortComputation,
      searchId = searchId,
      sendProgressTo = searchRequest.sendProgressTo)


    (result,(System.currentTimeMillis() - startTime).toInt)
  }


  def getMove(obj: Objective,
              acc: (Long, Long) => Boolean,
              shouldAbort: () => Boolean,
              initSolutionOpt:Option[Solution],
              sendFullSolution:Boolean,
              searchId:Long,
              sendProgressTo:Option[ActorRef[SearchProgress]]): IndependentSearchResult = {
    bestObjSoFar = Long.MaxValue
    neighborhood.getMoveAbortable(obj, obj.value, acc, shouldAbort,initSolutionOpt) match {
      case NoMoveFound => IndependentNoMoveFound()
      case MoveFound(m) =>
        sendProgressTo match {
          case None =>
          case Some(target) =>
            target ! SearchProgress(searchId, m.objAfter, System.currentTimeMillis())
        }
        if(sendFullSolution) {
          val startSol = initSolutionOpt.getOrElse(obj.model.solution())
          m.commit()
          bestObjSoFar = Long.MaxValue
          val toReturn = IndependentMoveFound(LoadIndependentSolutionMove(
            objAfter = m.objAfter,
            neighborhoodName = m.neighborhoodName,
            IndependentSolution(obj.model.solution())))
          startSol.restoreDecisionVariables()
          toReturn
        }else {
          bestObjSoFar = Long.MaxValue
          IndependentMoveFound(m.getIndependentMove(obj.model))
        }
    }
  }

  def doAllMoves(obj: Objective,
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

    while(!shouldAbort() && (neighborhood.getMoveAbortable(obj, obj.value, acc, shouldAbort) match {
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
      val toReturn = IndependentMoveFound(LoadIndependentSolutionMove(
        objAfter = obj.value,
        neighborhoodName = name,
        IndependentSolution(obj.model.solution())))

      toReturn
    }else {
      sendProgressTo match {
        case None =>
        case Some(target) =>
          target ! SearchProgress(searchId, lastProgressOBj, System.currentTimeMillis(), aborted=true)
      }
      IndependentNoMoveFound()
    }
  }
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
