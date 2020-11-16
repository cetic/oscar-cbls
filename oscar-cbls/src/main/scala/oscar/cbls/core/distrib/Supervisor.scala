package oscar.cbls.core.distrib

import akka.actor.BootstrapSetup
import akka.actor.setup.ActorSystemSetup
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, MailboxSelector}
import akka.dispatch.ControlMessage
import akka.util.Timeout
import org.slf4j.{Logger, LoggerFactory}
import oscar.cbls.core.computation.Store
import oscar.cbls.core.search.{Neighborhood, SearchResult}

import scala.collection.immutable.SortedMap
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration.Infinite
import scala.util.{Failure, Success}

sealed trait MessagesToSupervisor
final case class NewWorkerEnrolled(workerRef: ActorRef[MessageToWorker]) extends MessagesToSupervisor with ControlMessage
final case class ReadyForWork(workerRef: ActorRef[MessageToWorker], completedSearchIDOpt:Option[Long], completedNeighborhoodID:Option[Int]) extends MessagesToSupervisor with ControlMessage
final case class CancelSearchToSupervisor(searchID:Long) extends MessagesToSupervisor with ControlMessage

final case class SearchStarted(search:SearchTask, startID:Long, worker:ActorRef[MessageToWorker]) extends MessagesToSupervisor
final case class SearchNotStarted(search:SearchTask, worker:ActorRef[MessageToWorker]) extends MessagesToSupervisor
final case class Crash(worker:ActorRef[MessageToWorker]) extends MessagesToSupervisor
final case class Tic() extends MessagesToSupervisor

case class WorkGiverActorCreated(workGiverActor:ActorRef[MessageToWorkGiver])

import scala.concurrent.duration._

object Supervisor{

  def startSupervisorAndActorSystem(store:Store, search:Neighborhood, verbose:Boolean = false, tic:Duration = Duration.Inf):Supervisor = {
    val supervisorActorSystem = internalStartSupervisorAndActorSystem(verbose, tic)
    val supervisor = wrapSupervisor(supervisorActorSystem, store:Store, verbose)(system = supervisorActorSystem)
    search.labelNeighborhoodsForRemoteOperation(supervisor)
    supervisor
  }

  def internalStartSupervisorAndActorSystem(verbose:Boolean = false, tic:Duration = Duration.Inf):ActorSystem[MessagesToSupervisor] = {
    val  startLogger:Logger = LoggerFactory.getLogger("SupervisorObject")
    startLogger.info("Starting actor system and supervisor")


    //We prioritize some messages to try and maximize the hit on hotRestart
    val a = ActorSystem(createSupervisorBehavior(verbose, tic),"supervisor",
      setup=ActorSystemSetup.create(BootstrapSetup()),
      guardianProps = MailboxSelector.fromConfig("akka.actor.mailbox.unbounded-control-aware-queue-based")) //"akka.dispatch.UnboundedControlAwareMailbox"))




    //println(a.settings)

    a
  }

  def spawnSupervisor(context:ActorContext[_],verbose:Boolean):ActorRef[MessagesToSupervisor] = {
    context.spawn(createSupervisorBehavior(verbose),"supervisor")
  }

  def wrapSupervisor(supervisorRef:ActorRef[MessagesToSupervisor], store:Store, verbose:Boolean)
                    (implicit system:ActorSystem[_]):Supervisor = {
    new Supervisor(supervisorRef,store, verbose, system)
  }

  def createSupervisorBehavior(verbose:Boolean=false,tic:Duration = Duration.Inf):Behavior[MessagesToSupervisor] =
    Behaviors.setup {context:ActorContext[MessagesToSupervisor] => new SupervisorActor(context,verbose,tic)}
}


final case class DelegateSearchWithAction(searchRequest:SearchRequest, action:SearchResult=>Unit) extends MessagesToSupervisor with ControlMessage
final case class DelegateSearch(searchRequest:SearchRequest, replyTo:ActorRef[WorkGiverActorCreated], action:Option[SearchEnded => Unit]) extends MessagesToSupervisor with ControlMessage
final case class DelegateSearches(searchRequest:Array[SearchRequest], replyTo:ActorRef[WorkGiverActorCreated]) extends MessagesToSupervisor with ControlMessage
final case class ShutDown(replyTo:Option[ActorRef[Unit]]) extends MessagesToSupervisor
final case class SpawnWorker(workerBehavior:Behavior[MessageToWorker], replyTo:ActorRef[Unit]) extends MessagesToSupervisor
final case class NbWorkers(replyTo:ActorRef[Int]) extends MessagesToSupervisor

class Supervisor(supervisorActor:ActorRef[MessagesToSupervisor], m:Store, verbose:Boolean, implicit val system: ActorSystem[_]){
  implicit val timeout: Timeout = 3.seconds
  import akka.actor.typed.scaladsl.AskPattern._

  def createLocalWorker(m:Store,search:Neighborhood) : Unit ={
    createLocalWorker(m,search.identifyNeighborhoodForWorker)
  }

  def createLocalWorker(m:Store,neighborhoods:SortedMap[Int,RemoteNeighborhood]):Unit = {
    val workerBehavior = WorkerActor.createWorkerBehavior(neighborhoods,m,this.supervisorActor,verbose)
    val ongoingRequest:Future[Unit] = supervisorActor.ask[Unit] (ref => SpawnWorker(workerBehavior,ref))
    Await.result(ongoingRequest,atMost = 30.seconds)
  }

  def nbWorkers: Int = {
    val ongoingRequest:Future[Int] = supervisorActor.ask[Int] (ref => NbWorkers(ref))
    Await.result(ongoingRequest,atMost = 30.seconds)
  }

  def delegateSearch(searchRequest:SearchRequest): WorkGiver = {
    WorkGiver.wrap(internalDelegateSearch(searchRequest),m,this)
  }

  def delegateSearches(searchRequests:Array[SearchRequest]): AndWorkGiver = {
    WorkGiver.andWrap(searchRequests.map(searchRequest => this.internalDelegateSearch(searchRequest)),m,this)
  }

  def createWorkStream(): WorkStream = {
    new WorkStream(m, this)
  }

  def delegateSearchesStopAtFirst(searchRequests:Array[SearchRequest]): WorkGiver = {
    WorkGiver.wrap(delegateORSearches(searchRequests),m,this)
  }

  def delegateWithAction(searchRequest:SearchRequest,action:SearchEnded=>Unit): Unit = {
    supervisorActor.ask[WorkGiverActorCreated] (ref => DelegateSearch(searchRequest, ref, Some(action)))
  }

  private def internalDelegateSearch(searchRequest:SearchRequest): ActorRef[MessageToWorkGiver] = {
    val ongoingRequest:Future[WorkGiverActorCreated] = supervisorActor.ask[WorkGiverActorCreated] (ref => DelegateSearch(searchRequest, ref, None))
    Await.result(ongoingRequest,atMost = 30.seconds).workGiverActor
  }

  private def delegateORSearches(searchRequests:Array[SearchRequest]): ActorRef[MessageToWorkGiver] = {
    val ongoingRequest:Future[WorkGiverActorCreated] = supervisorActor.ask[WorkGiverActorCreated] (ref => DelegateSearches(searchRequests, ref))
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
}

class SupervisorActor(context: ActorContext[MessagesToSupervisor],
                      verbose: Boolean,
                      tic: Duration)
  extends AbstractBehavior[MessagesToSupervisor](context){

  private final case class StartSomeSearch() extends MessagesToSupervisor

  private var allKnownWorkers: List[ActorRef[MessageToWorker]] = Nil
  private var idleWorkers: List[ActorRef[MessageToWorker]] = Nil

  //this one is a list, because the most common operations are add and takeFirst
  private var runningSearches: SortedMap[Long,(SearchTask,ActorRef[MessageToWorker])] = SortedMap.empty

  private val waitingSearches = scala.collection.mutable.Queue[SearchTask]()

  //need to add, and remove regularly, based on ID, indexed by startID
  private var startingSearches: SortedMap[Long,(SearchTask,Long,ActorRef[MessageToWorker])] = SortedMap.empty
  //need to add, and remove regularly, based on ID
  private var ongoingSearches: SortedMap[Long,(SearchTask,ActorRef[MessageToWorker])] = SortedMap.empty

  private var totalStartedSearches = 0
  private var nextSearchID: Long = 0
  private var nextStartID: Long = 0 //search+worker

  var nbLocalWorker:Int = 0

  def status: String = {
    s"workers(total:${allKnownWorkers.size} busy:${allKnownWorkers.size - idleWorkers.size}) searches(waiting:${waitingSearches.size} starting:${startingSearches.size} running:${runningSearches.size} totalStarted:$totalStartedSearches)"
  }

  tic match {
    case _:Infinite => ;
    case f:FiniteDuration => context.scheduleOnce(f, context.self, Tic())
  }

  var neighborhoodToPreferredWorker:SortedMap[Int,ActorRef[MessageToWorker]] = SortedMap.empty

  override def onMessage(msg: MessagesToSupervisor): Behavior[MessagesToSupervisor] = {
    msg match {

      case Tic() =>
        context.log.info(status)

        tic match{
          case _:Infinite => ;
          case f:FiniteDuration => context.scheduleOnce(f, context.self, Tic())
        }
      //TODO: check that worker is still up re schedule associated search in case of worker crash/not responding

      case SpawnWorker(workerBehavior,ref) =>
        val worker = context.spawn(workerBehavior,s"localWorker$nbLocalWorker")
        nbLocalWorker += 1
        ref!Unit

      case NbWorkers(replyTo) =>
        replyTo ! this.allKnownWorkers.size

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

          case (false, _ :: _) =>

            def startSearch(search:SearchTask,worker:ActorRef[MessageToWorker]): Unit ={
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
            }

            val nbIdleWorkers = idleWorkers.size
            val nbAvailableSearches = waitingSearches.size
            val nbSearchToStart = nbIdleWorkers min nbAvailableSearches

            //println(s"nbSearchToStart:$nbSearchToStart")
            //println(s"idleWorkers:$idleWorkers")

            val searchesToStart = (0 until nbSearchToStart).toArray.map(_ => waitingSearches.dequeue())

            for(searchI <- searchesToStart.indices){
              val nID = searchesToStart(searchI).request.neighborhoodID.neighborhoodID
              val preferredWorkerOpt = neighborhoodToPreferredWorker.get(nID)
              preferredWorkerOpt match{
                case Some(preferredWorker) =>
                 // println(s"preferredWorker:$preferredWorker")
                 // println(s"preferredWorker.path:${preferredWorker.path}")
                 // println(s"idleWorkers:$idleWorkers")
                  val newIdle = idleWorkers.filter(_.path != preferredWorker.path)
                 // println(s"newIdle:$newIdle")
                  if(newIdle.size != idleWorkers.size){
                    //start this one
                    startSearch(searchesToStart(searchI),preferredWorker)
                    searchesToStart(searchI) = null
                    idleWorkers = newIdle
                    //println("hotRestart")
                  }
                case None => ;
              }
            }

            for(searchI <- searchesToStart.indices if searchesToStart(searchI) != null) {
              //println("coldRestart " + searchesToStart(searchI).request.neighborhoodID)
              val worker = idleWorkers.head
              idleWorkers = idleWorkers.tail
              startSearch(searchesToStart(searchI), worker)
              neighborhoodToPreferredWorker = neighborhoodToPreferredWorker + ((searchesToStart(searchI).request.neighborhoodID.neighborhoodID -> worker))
              searchesToStart(searchI) = null
            }

            //// take the first searches, one per available worker
            // double loop on these searches; perform worker assignment as they come (no smart optimization here, first fit)
            //for the remaining searches, make it anyhow

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

      case ReadyForWork(worker: ActorRef[MessageToWorker],completedSearchID:Option[Long],completedNeighborhoodID:Option[Int]) =>

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

      case DelegateSearch(searchRequest:SearchRequest, replyTo: ActorRef[WorkGiverActorCreated], action:Option[SearchEnded => Unit]) =>
        val searchId = nextSearchID
        nextSearchID += 1

        val workGiverActorClass = WorkGiverActor(context.self, searchId, action)
        val workGiverActorRef = context.spawn(workGiverActorClass, "workGiver_search_" + searchId)

        if(verbose) context.log.info(s"got new waiting search:$searchId created workGiver:${workGiverActorRef.path}")

        replyTo ! WorkGiverActorCreated(workGiverActorRef)

        //now, we have a WorkGiver actor, we search for an available Worker or put this request on a waiting list.
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

        val orWorkGiverActorClass = ORWorkGiverActor(context.self, requestsAndIDs.map(_._2))
        val orWorkGiverActorRef = context.spawn(orWorkGiverActorClass, s"orWorkGiver_search_${minSearchID}_to_${maxSearchID}")

        for((searchRequest,searchId) <- requestsAndIDs){
          val workGiverActorClass = WorkGiverActor(context.self, searchId, None, Some(orWorkGiverActorRef))
          val workGiverActorRef = context.spawn(workGiverActorClass, "workGiver_search_" + searchId)

          if(verbose) context.log.info(s"got new waiting search:$searchId created workGiver:${workGiverActorRef.path}")

          //now, we have a workGiverActor, we search for an available worker or put this request on a waiting list.
          val theSearch = SearchTask(searchRequest, searchId, workGiverActorRef)
          waitingSearches.enqueue(theSearch)
          workGiverActorRef
        }

        context.self ! StartSomeSearch()
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

        tic match{
          case _:Infinite => ;
          case f:FiniteDuration =>
            context.log.info(status)
        }

        if(verbose) context.log.info(s"Supervisor shutdown")
        return Behaviors.stopped
    }
    this
  }
}
