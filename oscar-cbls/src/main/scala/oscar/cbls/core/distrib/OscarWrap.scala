package oscar.cbls.core.distrib

import akka.actor.typed.ActorRef
import oscar.cbls.core.computation.{IndependentSerializableAbstractVariableSnapshot, Solution, Store}
import oscar.cbls.core.objective.IndependentObjective
import oscar.cbls.core.search._
///////////////////////////////////////////////////////////////

trait SearchRequest {
  // Unique identifier of the search request
  val uniqueSearchId: Long
  // Identifier of the remote task
  val remoteTaskId: RemoteTaskIdentification
  // Actor reference where the final result of the search should be sent
  val sendResultTo: ActorRef[SearchEnded]

  /**
   * The supervisor checks that the worker already has this solution loaded.
   * If it is the case, the solution is removed from the SearchRequest by calling dropStartSolution hereBelow
   * In case there is no startSolution, just return None
   * @return
   */
  def startSolutionOpt: Option[IndependentSolution]

  /**
   * the Supervisor can drop the startSolution in case the worker already has this solution loaded,
   * to spare on transmission and loading costs
   * @return
   */
  def dropStartSolution:SearchRequest

  /**
   * in case this is a neighborhood, the supervisor tries to run it on the same worker as last time
   * to try and benefit from te hotRestart of the neighborhood
   * only for neighborhoods, not for other purely stateless tasks
   * @return
   */
  def neighborhoodIdOpt:Option[Int]
}

case class SingleMoveSearch(uniqueSearchId: Long = -1,
                            remoteTaskId: RemoteTaskIdentification,
                            acceptanceCriterion: AcceptanceCriterion,
                            obj: IndependentObjective,
                            sendFullSolution: Boolean = false,
                            startSolutionOpt: Option[IndependentSolution],
                            sendResultTo: ActorRef[SearchEnded])
  extends SearchRequest {
  override def toString: String = s"SingleMoveSearch($remoteTaskId,$acceptanceCriterion,$obj,sendFullSolution:$sendFullSolution)"

  override def dropStartSolution: SearchRequest = this.copy(startSolutionOpt = None)

  override def neighborhoodIdOpt: Option[Int] = Some(remoteTaskId.taskId)
}

case class SearchProgress(searchId: Long, obj: Long, timeMs: Long, aborted: Boolean = false)

case class DoAllMoveSearch(uniqueSearchId: Long = -1,
                           remoteTaskId:RemoteTaskIdentification,
                           acceptanceCriterion: AcceptanceCriterion,
                           obj: IndependentObjective,
                           startSolutionOpt: Option[IndependentSolution],
                           sendProgressTo: Option[ActorRef[SearchProgress]] = None,
                           sendResultTo: ActorRef[SearchEnded])
  extends SearchRequest {
  override def toString: String = s"DoAllMoveSearch($remoteTaskId,$acceptanceCriterion,$obj)"

  override def dropStartSolution: SearchRequest = this.copy(startSolutionOpt = None)

  override def neighborhoodIdOpt: Option[Int] = Some(remoteTaskId.taskId)
}

//////////////////////////////////////////////////////////////
// This is what is sent to the worker
case class RemoteTaskIdentification(taskId: Int, description: String)

abstract class RemoteTask(val taskId: Int, description: String) {
  val remoteIdentification: RemoteTaskIdentification = RemoteTaskIdentification(taskId,description)

  def abort():Unit

  def loadSolution(startSolOpt:Option[IndependentSolution],
                   model:Store,
                   currentSolOpt:Option[(Solution,SolutionID)]) : (Solution,Option[SolutionID]) = {
    (startSolOpt,currentSolOpt) match {
      case (None, Some(cur)) =>
        (cur._1, Some(cur._2))
      case (Some(startSolution), Some(cur))
        if startSolution.solutionId.isDefined
          && cur._2 == startSolution.solutionId.get =>
        //no need to load the new solution
        (cur._1, Some(cur._2))
      case (Some(startSolution), None) =>
        val s = startSolution.makeLocal(model)
        //TODO: we should only transmit the delta, eventually
        s.restoreDecisionVariables(withoutCheckpoints = true)
        (s, startSolution.solutionId)
      case _ =>
        //TODO This case should never be reached; Throw exception ?
        (null, None)
    }
  }

  def doTask(taskMessage: SearchRequest,
             model: Store,
             currentSolOpt: Option[(Solution, SolutionID)],
             workerID: Option[String]) : Option[(Solution,SolutionID)]
}

case class RemoteNeighborhood(neighborhoodID: Int, neighborhood: Neighborhood)
  extends RemoteTask(neighborhoodID, neighborhood.toString) {

  @volatile
  var bestObjSoFar: Long = Long.MaxValue

  @volatile
  private var shouldAbortComputation:Boolean  = false

  override def abort(): Unit = {
    shouldAbortComputation = true
  }

  override def doTask(taskMessage: SearchRequest,
                      model: Store,
                      currentSolOpt: Option[(Solution, SolutionID)],
                      workerID: Option[String]): Option[(Solution, SolutionID)] = {
    // Loading solution from message
    val (startSol, solId): (Solution, Option[SolutionID]) = loadSolution(
      taskMessage.startSolutionOpt,
      model,
      currentSolOpt
    )
    shouldAbortComputation = false
    neighborhood.reset()
    // Actual perform of task from message
    taskMessage match {
      case s:SingleMoveSearch => doSingleMoveSearch(s, model, startSol, workerID)
      case d:DoAllMoveSearch => doAllMoveSearch(d, model, startSol, workerID)
    }
    shouldAbortComputation = false
    // Linking start solution to this solution Id.
    solId.map(x => (startSol, x))
  }

  private def doSingleMoveSearch(searchRequest: SingleMoveSearch,
                                 model: Store,
                                 startSol: Solution,
                                 workerID: Option[String]) : Unit = {
    // To compute processing time
    val startTime = System.currentTimeMillis()
    // Initialize objective value
    bestObjSoFar = Long.MaxValue
    val obj = searchRequest.obj.convertToObjective(model)

    neighborhood.getMoveAbortable(
      obj,
      obj.value,
      searchRequest.acceptanceCriterion,
      () => shouldAbortComputation,
      Some(startSol)) match {
      case NoMoveFound =>
        if (shouldAbortComputation) {
          searchRequest.sendResultTo ! SearchAborted(searchRequest.uniqueSearchId)
        } else {
          searchRequest.sendResultTo ! SearchCompleted(
            searchRequest.uniqueSearchId,
            IndependentNoMoveFound,
            (System.currentTimeMillis() - startTime).toInt
          )
        }
      case MoveFound(m) =>
        if (searchRequest.sendFullSolution) {
          val endTime = System.currentTimeMillis()
          m.commit()
          searchRequest.sendResultTo ! SearchCompleted(
            searchRequest.uniqueSearchId,
            IndependentMoveFound(IndependentLoadSolutionMove(
              IndependentSolution(obj.model.solution(), workerID),
              m.objAfter,
              m.neighborhoodName
            )),
            (endTime - startTime).toInt
          )
          startSol.restoreDecisionVariables()
        } else {
          searchRequest.sendResultTo ! SearchCompleted(
            searchRequest.uniqueSearchId,
            IndependentMoveFound(m.getIndependentMove(obj.model)),
            (System.currentTimeMillis() - startTime).toInt
          )
        }
    }
  }

  private def doAllMoveSearch(searchRequest: DoAllMoveSearch,
                              model: Store,
                              startSol: Solution,
                              workerID: Option[String]) : Unit = {

    val startTime = System.currentTimeMillis()
    val obj = searchRequest.obj.convertToObjective(model)

    bestObjSoFar = obj.value
    var anyMoveFound = false
    var name:String = ""
    val delayForNextFeedbackMS = 100 // 0.1 second
    var nextTimeForFeedbackMS:Long = System.currentTimeMillis() - 2* delayForNextFeedbackMS

    var lastProgressOBj:Long = bestObjSoFar

    while (
      !shouldAbortComputation &&
      (
        neighborhood.getMoveAbortable(
          obj,
          obj.value,
          searchRequest.acceptanceCriterion,
          () => shouldAbortComputation
        ) match {
          case NoMoveFound => false
          case MoveFound(m) =>
            m.commit()
            if (!anyMoveFound) {
              name = m.neighborhoodName
              anyMoveFound = true
            }
            if (m.objAfter < bestObjSoFar) {
              bestObjSoFar = m.objAfter
            }
            true
        }
      )
    ) {
      searchRequest.sendProgressTo match {
        case None =>
        case Some(target) =>
          val currentTimeMs:Long = System.currentTimeMillis()
          if (nextTimeForFeedbackMS <= currentTimeMs) {
            lastProgressOBj = obj.value
            target ! SearchProgress(searchRequest.uniqueSearchId, lastProgressOBj, currentTimeMs)
            nextTimeForFeedbackMS = currentTimeMs + delayForNextFeedbackMS
          }
      }
    }

    val endTime = System.currentTimeMillis()

    if (shouldAbortComputation) {
      searchRequest.sendResultTo ! SearchAborted(searchRequest.uniqueSearchId)
      searchRequest.sendProgressTo match {
        case None =>
        case Some(target) =>
          target ! SearchProgress(searchRequest.uniqueSearchId, lastProgressOBj, endTime, aborted=true)
      }

    } else if (anyMoveFound) {

      searchRequest.sendProgressTo match {
        case None =>
        case Some(target) =>
          target ! SearchProgress(searchRequest.uniqueSearchId, obj.value, endTime)
      }

      searchRequest.sendResultTo ! SearchCompleted(
        searchRequest.uniqueSearchId,
        IndependentMoveFound(IndependentLoadSolutionMove(
          IndependentSolution(obj.model.solution(),workerID),
          objAfter = obj.value,
          neighborhoodName = name
        )),
        (endTime - startTime).toInt
      )

    } else {
      searchRequest.sendProgressTo match {
        case None =>
        case Some(target) =>
          target ! SearchProgress(searchRequest.uniqueSearchId, obj.value, System.currentTimeMillis())
      }
      searchRequest.sendResultTo ! SearchCompleted(
        searchRequest.uniqueSearchId,
        IndependentNoMoveFound,
        (endTime - startTime).toInt
      )
    }
    startSol.restoreDecisionVariables()
  }
}

// ////////////////////////////////////////////////////////////

sealed trait IndependentSearchResult {
  def getLocalResult(m: Store): SearchResult
}

case class IndependentMoveFound(move: IndependentMove) extends IndependentSearchResult {
  override def getLocalResult(m: Store): MoveFound = MoveFound(move.makeLocal(m))
  def objAfter:Long = move.objAfter
}

case object IndependentNoMoveFound extends IndependentSearchResult {
  override def getLocalResult(m: Store): NoMoveFound.type = NoMoveFound
}

// ////////////////////////////////////////////////////////////

case class SolutionID(workerID:String, solutionID:Int)

object IndependentSolution {
  def apply(solution: Solution, workerID: Option[String]=None): IndependentSolution = {
    val solID = (solution.saveNr, workerID) match{
      case (Some(id), Some(worker)) => Some(SolutionID(worker,id))
      case _ => None
    }
    new IndependentSolution(solution.saves.map(_.makeIndependentSerializable), solID)
  }
}

case class IndependentSolution(saves: Iterable[IndependentSerializableAbstractVariableSnapshot],
                               solutionId: Option[SolutionID]) {
  def makeLocal(s: Store): Solution = {
    require(saves.nonEmpty)
    Solution(saves.map(_.makeLocal), s, None)
  }

  def isEmpty:Boolean = saves.isEmpty
}

// ////////////////////////////////////////////////////////////

trait IndependentMove {
  def objAfter: Long

  def neighborhoodName: String

  def makeLocal(m: Store): Move
}

case class IndependentLoadSolutionMove(s: IndependentSolution,
                                       objAfter: Long,
                                       neighborhoodName: String)
  extends IndependentMove {
  override def makeLocal(m: Store): Move =
    LoadSolutionMove(
      s.makeLocal(m),
      objAfter = objAfter,
      neighborhoodName = neighborhoodName)
}
