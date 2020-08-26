package oscar.cbls.core.distrib

import akka.actor.{TimerScheduler, Timers}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.immutable.SortedMap
import scala.concurrent.Future
import scala.concurrent.duration.Duration.Infinite
import scala.util.{Failure, Success}
//#imports

import scala.concurrent.Await

sealed trait MessagesToSupervisor
final case class NewWorkerEnrolled(workerRef: ActorRef[MessageToWorker]) extends MessagesToSupervisor
final case class ReadyForWork(workerRef: ActorRef[MessageToWorker],completedSearchIDOpt:Option[Long]) extends MessagesToSupervisor
final case class CancelSearchToSupervisor(searchID:Long) extends MessagesToSupervisor

final case class SearchStarted(search:SearchTask, startID:Long, worker:ActorRef[MessageToWorker]) extends MessagesToSupervisor
final case class SearchNotStarted(search:SearchTask, worker:ActorRef[MessageToWorker]) extends MessagesToSupervisor
final case class Crash(worker:ActorRef[MessageToWorker]) extends MessagesToSupervisor

final case class Tic() extends MessagesToSupervisor

case class WorkGiverActorCreated(workGiverActor:ActorRef[MessageToWorkGiver])

import scala.concurrent.duration._

object Supervisor{
  def startSupervisorAndActorSystem(verbose:Boolean):ActorSystem[MessagesToSupervisor] = {
    val  startLogger:Logger = LoggerFactory.getLogger("SupervisorObject")
    startLogger.info("Starting actor system and supervisor")
    ActorSystem(createSupervisorBehavior(verbose),"supervisor")
  }

  def spawnSupervisor(context:ActorContext[_],verbose:Boolean):ActorRef[MessagesToSupervisor] = {
    context.spawn(createSupervisorBehavior(verbose),"supervisor")
  }

  def wrapSupervisor(supervisorRef:ActorRef[MessagesToSupervisor])(implicit system:ActorSystem[_]):SupervisorWrapper = {
    new SupervisorWrapper(supervisorRef,system)
  }

  def createSupervisorBehavior(verbose:Boolean=false,tic:Duration = Duration.Inf):Behavior[MessagesToSupervisor] =
    Behaviors.setup {context:ActorContext[MessagesToSupervisor] => new SupervisorActor(context,verbose,tic)}


}

final case class DelegateSearch(searchRequest:SearchRequest, replyTo:ActorRef[WorkGiverActorCreated]) extends MessagesToSupervisor
final case class DelegateSearches(searchRequest:Array[SearchRequest], replyTo:ActorRef[WorkGiverActorCreated]) extends MessagesToSupervisor
final case class ShutDown(replyTo:Option[ActorRef[Unit]]) extends MessagesToSupervisor

class SupervisorWrapper(supervisorActor:ActorRef[MessagesToSupervisor],implicit val system: ActorSystem[_]){
  implicit val timeout: Timeout = 3.seconds
  import akka.actor.typed.scaladsl.AskPattern._

  def delegateSearch(searchRequest:SearchRequest):ActorRef[MessageToWorkGiver] = {
    val ongoingRequest:Future[WorkGiverActorCreated] = supervisorActor.ask[WorkGiverActorCreated] (ref => DelegateSearch(searchRequest, ref))
    Await.result(ongoingRequest,atMost = 30.seconds).workGiverActor
  }

  def shutdown(): Unit = {
    val ongoingRequest:Future[Unit] = supervisorActor.ask[Unit] (ref => ShutDown(Some(ref)))
    Await.result(ongoingRequest, 30.seconds)
    supervisorActor match{
      case a:ActorSystem[_] =>
        a.terminate()
        val  startLogger:Logger = LoggerFactory.getLogger("SupervisorWrapper")
        startLogger.info("terminating actor system")
      case _ => ;
    }
  }

  def delegateORSearches(searchRequests:Array[SearchRequest]):ActorRef[MessageToWorkGiver] = {
    val ongoingRequest:Future[WorkGiverActorCreated] = supervisorActor.ask[WorkGiverActorCreated] (ref => DelegateSearches(searchRequests, ref))
    Await.result(ongoingRequest,atMost = 30.seconds).workGiverActor
  }
}

class SupervisorActor(context: ActorContext[MessagesToSupervisor],verbose:Boolean, tic:Duration)
  extends AbstractBehavior[MessagesToSupervisor](context){

  private final case class StartSomeSearch() extends MessagesToSupervisor

  private var allKnownWorkers: List[ActorRef[MessageToWorker]] = Nil
  private var idleWorkers: List[ActorRef[MessageToWorker]] = Nil

  //this one is a list, because the most common opretions are add, and takeAny
  private val waitingSearches = scala.collection.mutable.Queue[SearchTask]()

  private var runningSearches:SortedMap[Long,(SearchTask,ActorRef[MessageToWorker])] = SortedMap.empty

  //need to add, and remove regularly, based on ID, indexed by startID
  private var startingSearches:SortedMap[Long,(SearchTask,Long,ActorRef[MessageToWorker])] = SortedMap.empty
  //need to add, and remove regularly, based on ID
  private var ongoingSearches:SortedMap[Long,(SearchTask,ActorRef[MessageToWorker])] = SortedMap.empty

  private var totalStartedSearches = 0

  private var nextSearchID:Long = 0
  private var nextStartID:Long = 0 //search+worker

  def status:String = {
    s"workers(total:${allKnownWorkers.size} busy:${allKnownWorkers.size - idleWorkers.size}) searches(waiting:${waitingSearches.size} starting:${startingSearches.size} running:${runningSearches.size} totalStarted:$totalStartedSearches)"
  }

  tic match{
    case _:Infinite => ;
    case f:FiniteDuration => context.scheduleOnce(f, context.self, Tic())
  }


  override def onMessage(msg: MessagesToSupervisor): Behavior[MessagesToSupervisor] = {
    msg match {
      case Tic() =>
        context.log.info(status)

        tic match{
          case _:Infinite => ;
          case f:FiniteDuration => context.scheduleOnce(f, context.self, Tic())
        }

      case NewWorkerEnrolled(workerRef: ActorRef[MessageToWorker]) =>
        allKnownWorkers = workerRef :: allKnownWorkers
        idleWorkers = workerRef :: idleWorkers
        context.self ! StartSomeSearch()
        context.log.info("new worker enrolled:" + workerRef.path)
      case StartSomeSearch() =>
        (waitingSearches.isEmpty, idleWorkers) match {
          case (true, idleWorkers) if idleWorkers.nonEmpty => ;
            if(verbose) context.log.info(status)

          case (_, Nil) => ;
            if(verbose) context.log.info(status)

          case (false, worker :: remainingWorkers) =>
            val search = waitingSearches.dequeue
            if(verbose) context.log.info(s"assigning search:${search.searchId} to worker:${worker.path}")
            val startID = nextStartID
            nextStartID = nextStartID + 1
            totalStartedSearches += 1

            implicit val responseTimeout: Timeout = 3.seconds
            context.ask[MessageToWorker, MessagesToSupervisor](worker, res => StartSearch(search, startID, res)) {
              case Success(_: SearchStarted) => SearchStarted(search, startID, worker)
              case Success(_: SearchNotStarted) => SearchNotStarted(search, worker)
              case Failure(_) => SearchNotStarted(search, worker)
            }

            startingSearches = startingSearches + (startID -> (search, startID, worker))

            idleWorkers = remainingWorkers
            if(idleWorkers.isEmpty){
              if(verbose) context.log.info(status)
            }
        }

      case SearchStarted(search, startID, worker) =>
        startingSearches.get(startID) match{
          case Some((search2,startID2,worker2)) if startID2 == startID =>
            require(search.searchId == search2.searchId)
            if(verbose) context.log.info(s"search:${search.searchId} start confirmed by worker:${worker.path}")
            ongoingSearches = ongoingSearches + (search.searchId -> (search2, worker2))
            startingSearches = startingSearches.-(startID)
            runningSearches = runningSearches + (search.searchId -> (search,worker))
          case _ =>
            if(verbose) context.log.warn(s"unexpected search:${search.searchId} start confirmed to Supervisor by worker:${worker.path}; asking for abort")
            worker ! AbortSearch(search.searchId)
        }

      case SearchNotStarted(search, worker) =>
        if(verbose) context.log.info(s"search:${search.searchId} could not be started by worker:${worker.path}")
        waitingSearches.enqueue(search)
        context.self ! StartSomeSearch()
      //we do not register the worker as available here because it will register itself through another call,
      // at least to show it is not completely crashed.

      case ReadyForWork(worker: ActorRef[MessageToWorker],completedSearchID:Option[Long]) =>

        require(allKnownWorkers contains worker)
        completedSearchID match{
          case None => ;
            if(verbose) context.log.info(s"got a worker ready:${worker.path}")
          case Some(s) =>
            if(verbose) context.log.info(s"got a worker ready:${worker.path}; finished search:$s")
            ongoingSearches = ongoingSearches.-(s)
            runningSearches = runningSearches - s
        }

        idleWorkers = worker :: idleWorkers
        context.self ! StartSomeSearch()

      case DelegateSearch(searchRequest:SearchRequest, replyTo: ActorRef[WorkGiverActorCreated]) =>
        val searchId = nextSearchID
        nextSearchID += 1

        val workGiverActorClass = WorkGiver(context.self, searchId)
        val workGiverActorRef = context.spawn(workGiverActorClass, "workGiver_search_" + searchId)

        if(verbose) context.log.info(s"got new waiting search:$searchId created workGiver:${workGiverActorRef.path}")

        replyTo ! WorkGiverActorCreated(workGiverActorRef)

        //now, we have a workGiverActor, we search for an available worker or put this request on a waiting list.
        val theSearch = SearchTask(searchRequest, searchId, workGiverActorRef)
        waitingSearches.enqueue(theSearch)
        context.self ! StartSomeSearch()

      case DelegateSearches(searchRequests:Array[SearchRequest], replyTo: ActorRef[WorkGiverActorCreated]) =>

        val minSearchID = nextSearchID

        val requestsAndIDs = searchRequests.map(r => {
          val searchId = nextSearchID
          nextSearchID += 1
          (r,searchId)
        })
        val maxSearchID = nextSearchID-1

        val orWorkGiverActorClass = ORWorkGiver(context.self, requestsAndIDs.map(_._2))
        val orWorkGiverActorRef = context.spawn(orWorkGiverActorClass, s"orWorkGiver_search_${minSearchID}_to_${maxSearchID}")

        for((searchRequest,searchId) <- requestsAndIDs){
          val workGiverActorClass = WorkGiver(context.self, searchId,Some(orWorkGiverActorRef))
          val workGiverActorRef = context.spawn(workGiverActorClass, "workGiver_search_" + searchId)

          if(verbose) context.log.info(s"got new waiting search:$searchId created workGiver:${workGiverActorRef.path}")

          //now, we have a workGiverActor, we search for an available worker or put this request on a waiting list.
          val theSearch = SearchTask(searchRequest, searchId, workGiverActorRef)
          waitingSearches.enqueue(theSearch)
          context.self ! StartSomeSearch()
          workGiverActorRef
        }

        replyTo ! WorkGiverActorCreated(orWorkGiverActorRef)

      case CancelSearchToSupervisor(searchID: Long) =>

        waitingSearches.dequeueFirst(_.searchId == searchID) match {
          case None =>
            //Search was already ongoing on some worker
            //the search is already being processed by some search worker.

            ongoingSearches.get(searchID) match {
              case Some((search, worker)) =>
                if(verbose) context.log.info(s"got cancel request for ongoing search:$searchID; forward to worker:${worker.path}")
                worker ! AbortSearch(search.searchId)
              case None =>

                startingSearches.find(_._2._1.searchId == searchID) match{
                  case Some((startID2,(search,startID3,worker)))  =>
                    startingSearches = startingSearches.-(startID2)
                    if(verbose) context.log.info(s"got cancel request for starting search:$searchID forward to worker:${worker.path}")
                    worker ! AbortSearch(search.searchId)
                  case None =>
                    if(verbose) context.log.info(s"got cancel request for unknown search:$searchID; ignored; search was already completed")
                }
            }
          case Some(_) =>
            //we just forget this one search from the list of waiting searches
            if(verbose) context.log.info(s"got cancel request for waiting search:$searchID; search removed from waiting list")
        }

      case Crash(worker) =>
        context.log.info(s"got crash report from $worker; waiting for shutdown command")
      //waitingSearches.dequeueAll(_ => true)

      case ShutDown(replyTo: Option[ActorRef[Unit]]) =>
        //ask for a coordinated shutdown of all workers

        context.log.info(s"got Shutdown command; forwarding to workers")
        for (worker <- allKnownWorkers) {
          worker ! ShutDownWorker()
        }
        replyTo match{
          case Some(x) => x ! Unit
          case None => ;
        }

        if(verbose) context.log.info(s"Supervisor shutdown")
        return Behaviors.stopped
    }
    this
  }
}


