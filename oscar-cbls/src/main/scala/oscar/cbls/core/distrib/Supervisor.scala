package oscar.cbls.core.distrib

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import org.slf4j.{Logger, LoggerFactory}
import oscar.cbls.core.computation.Store
import oscar.cbls.core.search.Neighborhood

import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.duration.Duration.Infinite
import scala.concurrent.{Await, Future, TimeoutException}
import scala.util.{Failure, Success}

sealed trait MessagesToSupervisor

final case class NewWorkerEnrolled(workerRef: ActorRef[MessageToWorker]) extends MessagesToSupervisor

final case class ReadyForWork(workerRef: ActorRef[MessageToWorker],
                              completedSearchIDOpt: Option[Long],
                              currentModelId:Option[SolutionID]) extends MessagesToSupervisor

final case class CancelSearchToSupervisor(searchID: Long,
                                          keepAliveIfOjBelow: Option[Long]=None) extends MessagesToSupervisor

final case class SearchStarted(searchID: Long,
                               startID: Long,
                               worker: ActorRef[MessageToWorker]) extends MessagesToSupervisor

final case class SearchNotStarted(searchID: Long,
                                  startID: Long,
                                  worker: ActorRef[MessageToWorker]) extends MessagesToSupervisor

final case class Crash(worker: ActorRef[MessageToWorker]) extends MessagesToSupervisor

final case class DelegateSearch(searchRequest: SearchRequest,
                                waitForMoreSearch: Boolean = false) extends MessagesToSupervisor

case object StartSomeSearch extends MessagesToSupervisor

final case class GetNewUniqueID(replyTo: ActorRef[Long]) extends MessagesToSupervisor

final case class ShutDown(replyTo: Option[ActorRef[Unit]]) extends MessagesToSupervisor

final case class SpawnWorker(workerBehavior: Behavior[MessageToWorker]) extends MessagesToSupervisor

final case class NbWorkers(replyTo: ActorRef[Int],
                           waitForAtLeastOneWorker:Boolean) extends MessagesToSupervisor

final case class RemoteStatisticsFor(replyTo: ActorRef[List[Array[String]]],
                                     remoteNeighborhood: RemoteTaskIdentification) extends MessagesToSupervisor

final case class SpawnNewActor[T](behavior:Behavior[T],
                                  behaviorName:String,
                                  replyTo:ActorRef[ActorRef[T]]) extends MessagesToSupervisor

object Supervisor {
  // Number of available cores on this JVM
  val nbCores: Int = Runtime.getRuntime.availableProcessors()

  def startSupervisorAndActorSystem(search: Neighborhood,
                                    verbose: Boolean = false,
                                    hotRestart: Boolean = true,
                                    tic: Duration = Duration.Inf): Supervisor = {
    val supervisorActorSystem = internalStartSupervisorAndActorSystem(verbose, hotRestart, tic)
    val supervisor = wrapSupervisor(supervisorActorSystem, verbose)(system = supervisorActorSystem)
    val (nbNRemoteNeighborhood,nbDistributedCombinator,_) = search.labelAndExtractRemoteTasks(supervisor: Supervisor)
    val startLogger: Logger = LoggerFactory.getLogger("SupervisorObject")
    startLogger.info(s"Analyzed search; Nb Distributed Combinators:$nbDistributedCombinator; Nb Remote Neighborhoods:$nbNRemoteNeighborhood")
    supervisor
  }

  def internalStartSupervisorAndActorSystem(verbose: Boolean = false, hotRestart:Boolean, tic: Duration = Duration.Inf): ActorSystem[MessagesToSupervisor] = {
    val startLogger: Logger = LoggerFactory.getLogger("SupervisorObject")
    startLogger.info("Starting actor system and supervisor")
    //We prioritize some messages to try and maximize the hit on hotRestart
    ActorSystem(createSupervisorBehavior(verbose, hotRestart, tic), "supervisor")
  }

  def wrapSupervisor(supervisorRef: ActorRef[MessagesToSupervisor], verbose: Boolean)
                    (implicit system: ActorSystem[_]): Supervisor = {
    new Supervisor(supervisorRef, verbose, system)
  }

  def spawnSupervisor(context: ActorContext[_], verbose: Boolean, hotRestart:Boolean): ActorRef[MessagesToSupervisor] = {
    context.spawn(createSupervisorBehavior(verbose, hotRestart), "supervisor")
  }

  def createSupervisorBehavior(verbose: Boolean = false, hotRestart:Boolean = true, tic: Duration = Duration.Inf): Behavior[MessagesToSupervisor] =
    Behaviors.setup { context: ActorContext[MessagesToSupervisor] => new SupervisorActor(context, verbose, hotRestart, tic) }
}

class Supervisor(val supervisorActor: ActorRef[MessagesToSupervisor],
                 verbose: Boolean,
                 implicit val system: ActorSystem[_]) {
  //TODO look for an adequate timeout or stopping mechanism
  implicit val timeout: Timeout = 1.hour

  import akka.actor.typed.scaladsl.AskPattern._

  //TODO: for the distributed version, regularly check that workers performing some search are still alive and working,
  // otherwise, search must be restarted at another worker.

  @volatile
  private var nbLocalWorkers:Int = 0
  def createLocalWorker(m: Store, search: Neighborhood, workerName: String = null): Unit = {
    this.synchronized {
      nbLocalWorkers += 1
      val workerBehavior = WorkerActor.createWorkerBehavior(
        search.identifyRemotelySearchableNeighborhoods,
        m,
        this.supervisorActor,
        verbose,
        if (workerName == null) "localWorker" + nbLocalWorkers else workerName
      )
      supervisorActor ! SpawnWorker(workerBehavior)
    }
  }

  def spawnNewActor[T](behavior:Behavior[T],behaviorName:String): Future[ActorRef[T]] = {
    supervisorActor.ask[ActorRef[T]](ref => SpawnNewActor(behavior:Behavior[T],behaviorName:String, ref))
  }

  def nbWorkers: Int = {
    val ongoingRequest: Future[Int] = supervisorActor.ask[Int](ref => NbWorkers(ref, waitForAtLeastOneWorker = false))
    Await.result(ongoingRequest, atMost = 30.seconds)
  }

  /**
   * Waits for at least one worker to be available, with a timeout
   *
   * @param waitFor after the timeout, returns zero
   * @return the number of available workers, which is zero if timeout is reached
   */
  def waitForAtLestOneWorker(waitFor:Duration = 5.minutes):Int = {
    val ongoingRequest: Future[Int] = supervisorActor.ask[Int](ref => NbWorkers(ref,waitForAtLeastOneWorker = true))
    try{
      Await.result(ongoingRequest, atMost = waitFor)
    }catch {
      case _: TimeoutException => 0
    }
  }

  def getRemoteStatisticsFor(remoteNeighborhood:RemoteTaskIdentification,waitFor:Duration = 5.minutes):List[Array[String]] = {
    val ongoingRequest: Future[List[Array[String]]] = supervisorActor.ask[List[Array[String]]](ref => RemoteStatisticsFor(ref,remoteNeighborhood))
    Await.result(ongoingRequest, atMost = waitFor)
  }

  def shutdown(): Unit = {
    val ongoingRequest: Future[Unit] = supervisorActor.ask[Unit](ref => ShutDown(Some(ref)))
    Await.result(ongoingRequest, 30.seconds)
    supervisorActor match {
      case a: ActorSystem[_] =>
        a.terminate()
        val startLogger: Logger = LoggerFactory.getLogger("SupervisorObject")
        startLogger.info("terminating actor system")
      case _ => ;
    }
  }

  def throwRemoteExceptionAndShutDown(searchCrashed:SearchCrashed): Unit ={
    val e = new Exception(s"Crash happened at worker:${searchCrashed.worker}: \n${searchCrashed.exception.getMessage}\nwhen performing neighborhood:${searchCrashed.searchTask}")
    e.setStackTrace(
      //This trims the stack trace to hide the intermediary calls to threads, futures and the like.
      (searchCrashed.exception.getStackTrace.toList.reverse.dropWhile(!_.getClassName.contains("oscar.cbls")).reverse
        //c.exception.getStackTrace.toList
        ::: e.getStackTrace.toList).toArray)
    shutdown()
    throw e
  }
}

class SupervisorActor(context: ActorContext[MessagesToSupervisor],
                      verbose: Boolean,
                      hotRestart:Boolean,
                      tic: Duration)
  extends AbstractBehavior[MessagesToSupervisor](context) {
  // Message to self
  private case object Tic extends MessagesToSupervisor
  // This one cannot be a control message.

  private val waitingSearches = mutable.Queue[SearchRequest]()
  var nbLocalWorker: Int = 0
  var nbCustomSearchActor:Int = 0
  var neighborhoodToPreferredWorker: SortedMap[Int, ActorRef[MessageToWorker]] = SortedMap.empty
  private var allKnownWorkers: List[ActorRef[MessageToWorker]] = Nil
  private var idleWorkersAndTheirCurentModelID: List[(ActorRef[MessageToWorker],Option[SolutionID])] = Nil
  //this one is a list, because the most common operations are add and takeFirst
  private var startingSearches: SortedMap[Long, (SearchRequest, Long, ActorRef[MessageToWorker])] = SortedMap.empty
  //need to add, and remove regularly, based on ID
  private var ongoingSearches: SortedMap[Long, (SearchRequest, ActorRef[MessageToWorker])] = SortedMap.empty
  private var totalStartedSearches = 0
  private var nextSearchID: Long = 0
  private var nextStartID: Long = 0 //search+worker
  private var notifyForAvailableWorkers:List[NbWorkers] = Nil
  private var statisticCollectorID:Int = 0

  override def onMessage(msg: MessagesToSupervisor): Behavior[MessagesToSupervisor] = {
    msg match {

      case Tic =>
        context.log.info(status)
        tic match {
          case _: Infinite => ;
          case f: FiniteDuration => context.scheduleOnce(f, context.self, Tic)
        }

      case SpawnNewActor(behavior, behaviorName, replyTo) =>
        replyTo ! context.spawn(behavior, s"customSearchActor$nbCustomSearchActor$behaviorName")
        nbCustomSearchActor += 1

      case SpawnWorker(workerBehavior) =>
        context.spawn(workerBehavior, s"localWorker$nbLocalWorker")
        nbLocalWorker += 1

      case NbWorkers(replyTo, waitForAtLeastOneWorker) if !waitForAtLeastOneWorker =>
        replyTo ! this.allKnownWorkers.size

      case n@NbWorkers(replyTo, waitForAtLeastOneWorker) if waitForAtLeastOneWorker =>
        if (this.allKnownWorkers.nonEmpty) {
          replyTo ! this.allKnownWorkers.size
        } else {
          //we must wait for some workers...
          notifyForAvailableWorkers = n :: notifyForAvailableWorkers
        }

      case RemoteStatisticsFor(replyTo, remoteNeighborhood) =>
        statisticCollectorID += 1
        /////
        def statisticsCollector1(context:ActorContext[(Int,List[Array[String]])]): Behavior[(Int,List[Array[String]])] ={
          val workerArray = allKnownWorkers.reverse.toArray
          //println("workers:\n\t" + workerArray.mkString("\n\t"))
          for( i <- workerArray.indices) {
            workerArray(i) ! GetStatisticsFor(remoteNeighborhood, i, context.self)
          }
          val statisticsArray:Array[List[Array[String]]] = Array.fill(workerArray.length)(null)
          statisticsCollector2(statisticsArray,statisticsArray.length)
        }
        /////
        def statisticsCollector2(statisticsArray:Array[List[Array[String]]],
                                 waitedAnswers:Int): Behavior[(Int,List[Array[String]])] = {
          Behaviors.receiveMessage {
            case (i, statistics) =>
              statisticsArray(i) = statistics
              if (waitedAnswers == 1) {
                replyTo ! statisticsArray.toList.flatten
                Behaviors.stopped
              } else {
                statisticsCollector2(statisticsArray, waitedAnswers - 1)
              }
          }
        }
        context.spawn(
          Behaviors.setup { context: ActorContext[(Int,List[Array[String]])] =>
            statisticsCollector1(context)
          },
          "statisticsCollector" + statisticCollectorID
        )

      case NewWorkerEnrolled(workerRef) =>
        allKnownWorkers = workerRef :: allKnownWorkers
        idleWorkersAndTheirCurentModelID = (workerRef,None) :: idleWorkersAndTheirCurentModelID
        context.self ! StartSomeSearch
        context.log.info("new worker enrolled:" + workerRef.path)
        for(nbWorker <- notifyForAvailableWorkers){
          nbWorker.replyTo ! this.allKnownWorkers.size
        }
        notifyForAvailableWorkers = Nil

      case StartSomeSearch =>
        //context.log.info("StartSomeSearch")
        (waitingSearches.isEmpty, idleWorkersAndTheirCurentModelID) match {
          case (true, idleWorkers) if idleWorkers.nonEmpty =>
            // No waiting searches
            if (verbose) context.log.info(status)

          case (_, Nil) =>
            // No idle workers
            if (verbose) context.log.info(status)

          case (false, _ :: _) =>
            // There are waiting searches with idle workers ; a search can be started
            // The following function starts the search in a worker
            def startSearch(search: SearchRequest,
                            worker: ActorRef[MessageToWorker],
                            currentSolutionAtWorker:Option[SolutionID]): Unit = {
              if (verbose) context.log.info(s"assigning search:${search.uniqueSearchId} to worker:${worker.path}")
              val startID = nextStartID
              nextStartID = nextStartID + 1
              totalStartedSearches += 1
              val solutionForThisSearch = search.startSolutionOpt
              val simplifiedSearch = (solutionForThisSearch,currentSolutionAtWorker) match {
                case (Some(x),Some(y)) if x.solutionId.isDefined && x.solutionId.get == y => search.dropStartSolution
                case _ => search
              }
              implicit val responseTimeout: Timeout = 3.seconds
              // Ask worker to start the search, and get the response
              context.ask[MessageToWorker, MessagesToSupervisor](worker, res => StartSearch(simplifiedSearch, startID, res)) {
                case Success(_: SearchStarted) => SearchStarted(simplifiedSearch.uniqueSearchId, startID, worker)
                case Success(_: SearchNotStarted) => SearchNotStarted(simplifiedSearch.uniqueSearchId, startID, worker)
                case Failure(_) => SearchNotStarted(simplifiedSearch.uniqueSearchId, startID, worker)
                case _ => SearchNotStarted(simplifiedSearch.uniqueSearchId, startID, worker) // Default case
              }
              startingSearches = startingSearches + (startID -> (search, startID, worker))
            }
            //////////
            val nbIdleWorkers = idleWorkersAndTheirCurentModelID.size
            val nbAvailableSearches = waitingSearches.size
            var nbSearchToStart = nbIdleWorkers min nbAvailableSearches
            var couldDequeue = true
            while (hotRestart && couldDequeue && nbSearchToStart != 0) {
              couldDequeue = false
              waitingSearches.dequeueFirst(searchTask => {
                val nID = searchTask.neighborhoodIdOpt.getOrElse(-1)
                val preferredWorkerOpt = neighborhoodToPreferredWorker.get(nID)
                preferredWorkerOpt match {
                  case Some(preferredWorker) =>
                    val newIdle = idleWorkersAndTheirCurentModelID.filter(_._1.path != preferredWorker.path)
                    if (newIdle.size != idleWorkersAndTheirCurentModelID.size) {
                      //start this one
                      //println("hotRestart" + searchTask.request.neighborhoodID)
                      val modelAtWorkerSide = idleWorkersAndTheirCurentModelID.filter(_._1.path == preferredWorker.path) match {
                        case (_,modelOpt) :: Nil => modelOpt
                        case _ => None
                      }
                      startSearch(searchTask, preferredWorker, modelAtWorkerSide)
                      idleWorkersAndTheirCurentModelID = newIdle
                      nbSearchToStart -= 1
                      couldDequeue = true
                      true
                    } else false
                  case None => false
                }
              })
            }
            while (nbSearchToStart != 0) {
              val searchToStart = waitingSearches.dequeue()
              val worker = idleWorkersAndTheirCurentModelID.head
              idleWorkersAndTheirCurentModelID = idleWorkersAndTheirCurentModelID.tail
              //println("coldRestart " + searchToStart.request.neighborhoodID)
              startSearch(searchToStart, worker._1,worker._2)

              if(hotRestart) {
                searchToStart.neighborhoodIdOpt match{
                  case None => ;
                  case Some(n) =>
                    neighborhoodToPreferredWorker = neighborhoodToPreferredWorker + (n -> worker._1)
                }
              }
              nbSearchToStart -= 1
            }
            //take the first searches, one per available worker
            //double loop on these searches; perform worker assignment as they come (no smart optimization here, first fit)
            //for the remaining searches, make it anyhow
            if (idleWorkersAndTheirCurentModelID.isEmpty) {
              if (verbose) context.log.info(status)
            }

          case _ =>
            // Default case
            if (verbose) context.log.info(status)
        }

      case SearchStarted(searchID, startID, worker) =>
        startingSearches.get(startID) match {
          case Some((search2, startID2, worker2)) if startID2 == startID =>
            require(searchID == search2.uniqueSearchId)
            if (verbose) context.log.info(s"search:$searchID start confirmed by worker:${worker.path}")
            ongoingSearches = ongoingSearches + (searchID -> (search2, worker2))
            startingSearches = startingSearches.-(startID)
          case _ =>
            if (verbose) context.log.warn(s"unexpected search:$searchID start confirmed to Supervisor by worker:${worker.path}; asking for abort")
            worker ! AbortSearch(searchID)
        }

      case SearchNotStarted(searchID, startID, worker) =>
        startingSearches.get(startID) match {
          case Some((search2, startID2, _)) if startID2 == startID =>
            require(searchID == search2.uniqueSearchId)
            /////
            if (verbose) context.log.info(s"search:$searchID could not be started by worker:${worker.path}")
            waitingSearches.enqueue(search2)
            startingSearches = startingSearches.-(startID)
            context.self ! StartSomeSearch

          case _ =>
            if (verbose) context.log.warn(s"unexpected search:$searchID could not be started; ignoring")
        }
      //we do not register the worker as available here because it will register itself through another call,
      // at least to show it is not completely crashed.

      case ReadyForWork(worker, completedSearchID, currentModelId) =>
        require(allKnownWorkers contains worker)
        /////
        if (verbose) context.log.info(s"got a worker ready:${worker.path}; finished search:$completedSearchID")
        completedSearchID match {
          case Some(s) => ongoingSearches = ongoingSearches.-(s)
          case None => ;
        }
        idleWorkersAndTheirCurentModelID = (worker,currentModelId) :: idleWorkersAndTheirCurentModelID
        context.self ! StartSomeSearch

      case GetNewUniqueID(replyTo) =>
        replyTo ! nextSearchID
        nextSearchID += 1

      case DelegateSearch(searchRequest, waitForMoreSearches) =>
        val searchId = searchRequest.uniqueSearchId
        if (verbose) context.log.info(s"got new waiting search:$searchId for :${searchRequest.sendResultTo.path}")
        //now, we search for an available Worker or put this request on a waiting list.
        waitingSearches.enqueue(searchRequest)
        if (!waitForMoreSearches) {
          context.self ! StartSomeSearch
        }

      case CancelSearchToSupervisor(searchID, keepAliveIfOjBelow) =>
        require(searchID != -1)
        /////
        waitingSearches.dequeueFirst(_.uniqueSearchId == searchID) match {
          case None =>
            //Search was already ongoing on some worker
            //the search is already being processed by some search worker.
            ongoingSearches.get(searchID) match {
              case Some((search, worker)) =>
                if (verbose) context.log.info(s"got cancel request for ongoing search:$searchID; forward to worker:${worker.path}")
                worker ! AbortSearch(search.uniqueSearchId,keepAliveIfOjBelow)

              case None =>
                startingSearches.find(_._2._1.uniqueSearchId == searchID) match {
                  case Some((startID2, (search, _, worker))) =>
                    startingSearches = startingSearches.-(startID2)
                    if (verbose) context.log.info(s"got cancel request for starting search:$searchID forward to worker:${worker.path}")
                    worker ! AbortSearch(search.uniqueSearchId,keepAliveIfOjBelow)
                  case None =>
                    if (verbose) context.log.info(s"got cancel request for unknown search:$searchID; ignored; search was already completed")
                }
            }

          case Some(_) =>
            //we just forget this one search from the list of waiting searches
            if (verbose) context.log.info(s"got cancel request for waiting search:$searchID; search removed from waiting list")
        }

      case Crash(worker) =>
        context.log.info(s"got crash report from $worker; waiting for shutdown command")
        //waitingSearches.dequeueAll(_ => true)

      case ShutDown(replyTo) =>
        //ask for a coordinated shutdown of all workers
        context.log.info(s"got Shutdown command; forwarding to workers")
        for (worker <- allKnownWorkers) {
          worker ! ShutDownWorker
        }
        replyTo match {
          case Some(x) => x ! ()
          case None => ;
        }
        tic match {
          case _: Infinite => ;
          case _: FiniteDuration =>
            context.log.info(status)
        }
        if (verbose) context.log.info(s"Supervisor shutdown")
        Behaviors.stopped

      case msg =>
        if (verbose) context.log.info(s"Supervisor got wrong message: $msg")
        Behaviors.same
    }
    this
  }

  tic match {
    case _: Infinite => ;
    case f: FiniteDuration => context.scheduleOnce(f, context.self, Tic)
  }

  def status: String = {
    s"workers(total:${allKnownWorkers.size} busy:${allKnownWorkers.size - idleWorkersAndTheirCurentModelID.size}) searches(waiting:${waitingSearches.size} starting:${startingSearches.size} running:${ongoingSearches.size} totalStarted:$totalStartedSearches)"
  }
}
