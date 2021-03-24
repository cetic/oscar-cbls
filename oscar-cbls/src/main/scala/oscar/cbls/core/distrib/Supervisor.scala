package oscar.cbls.core.distrib

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, MailboxSelector}
import akka.dispatch.ControlMessage
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.slf4j.{Logger, LoggerFactory}
import oscar.cbls.core.computation.Store
import oscar.cbls.core.search.Neighborhood

import scala.collection.immutable.SortedMap
import scala.concurrent.duration.Duration.Infinite
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

sealed trait MessagesToSupervisor

final case class NewWorkerEnrolled(workerRef: ActorRef[MessageToWorker]) extends MessagesToSupervisor with ControlMessage

final case class ReadyForWork(workerRef: ActorRef[MessageToWorker], completedSearchIDOpt: Option[Long], completedNeighborhoodIDAndMoveFound: Option[(Int,Boolean)], currentModelId:Option[Int]) extends MessagesToSupervisor with ControlMessage

final case class CancelSearchToSupervisor(searchID: Long,keepAliveIfOjBelow:Option[Long]=None) extends MessagesToSupervisor with ControlMessage

final case class SearchStarted(search: SearchTask, searchID: Long, worker: ActorRef[MessageToWorker]) extends MessagesToSupervisor

final case class SearchNotStarted(search: SearchTask, worker: ActorRef[MessageToWorker]) extends MessagesToSupervisor

final case class Crash(worker: ActorRef[MessageToWorker]) extends MessagesToSupervisor

final case class DelegateSearch(searchRequest: SearchRequest,
                                sendSearchResultTo:ActorRef[SearchEnded],
                                uniqueSearchID:Long = -1) extends MessagesToSupervisor with ControlMessage

final case class GetNewUniqueID(replyTo:ActorRef[Long]) extends MessagesToSupervisor with ControlMessage

final case class ShutDown(replyTo: Option[ActorRef[Unit]]) extends MessagesToSupervisor

final case class SpawnWorker(workerBehavior: Behavior[MessageToWorker]) extends MessagesToSupervisor

final case class NbWorkers(replyTo: ActorRef[Int]) extends MessagesToSupervisor

final case class SpawnNewActor[T](behavior:Behavior[T],behaviorName:String, replyTo:ActorRef[ActorRef[T]]) extends MessagesToSupervisor

import scala.concurrent.duration._

object Supervisor {

  def startSupervisorAndActorSystem(store: Store, search: Neighborhood, verbose: Boolean = false, tic: Duration = Duration.Inf): Supervisor = {
    val supervisorActorSystem = internalStartSupervisorAndActorSystem(verbose, tic)
    val supervisor = wrapSupervisor(supervisorActorSystem, store: Store, verbose)(system = supervisorActorSystem)
    val (nbNRemoteNeighborhood,nbRemoteCombinator,neighborhoods) = search.labelAndExtractRemoteNeighborhoods(supervisor: Supervisor)

    val startLogger: Logger = LoggerFactory.getLogger("SupervisorObject")
    startLogger.info(s"analyzed search; nbRemoteNeighborhood:$nbNRemoteNeighborhood nbRemoteCombinator:$nbRemoteCombinator")

    supervisor
  }

  def internalStartSupervisorAndActorSystem(verbose: Boolean = false, tic: Duration = Duration.Inf): ActorSystem[MessagesToSupervisor] = {
    val startLogger: Logger = LoggerFactory.getLogger("SupervisorObject")
    startLogger.info("Starting actor system and supervisor")

    //We prioritize some messages to try and maximize the hit on hotRestart
    val a = ActorSystem(
      createSupervisorBehavior(verbose, tic), "supervisor",
      config = ConfigFactory.parseString("""
                                           |oscarcbls.supervisormailbox.mailbox-type = "akka.dispatch.UnboundedControlAwareMailbox"
                                           |akka.version = 2.6.5
                                           |akka.home = ""
                                           |akka.actor.allow-java-serialization = on
                                           |akka.actor.creation-timeout = 20s
                                           |akka.actor.debug.receive = off
                                           |akka.actor.debug.autoreceive = off
                                           |akka.actor.debug.lifecycle = off
                                           |akka.actor.debug.fsm = off
                                           |akka.actor.debug.event-stream = off
                                           |akka.actor.debug.unhandled = off
                                           |akka.actor.debug.router-misconfiguration = off
                                           |akka.actor.default-dispatcher.type = "Dispatcher"
                                           |akka.actor.default-dispatcher.executor = "default-executor"
                                           |akka.actor.default-dispatcher.default-executor.fallback = "fork-join-executor"
                                           |akka.actor.default-dispatcher.fork-join-executor.parallelism-min = 8
                                           |akka.actor.default-dispatcher.fork-join-executor.parallelism-factor = 1.0
                                           |akka.actor.default-dispatcher.fork-join-executor.parallelism-max = 64
                                           |akka.actor.default-dispatcher.fork-join-executor.task-peeking-mode = "FIFO"
                                           |akka.actor.default-dispatcher.shutdown-timeout = 1s
                                           |akka.actor.default-dispatcher.throughput = 1
                                           |akka.actor.default-dispatcher.throughput-deadline-time = 0ms
                                           |akka.actor.default-dispatcher.attempt-teamwork = on
                                           |akka.actor.default-dispatcher.mailbox-requirement = ""
                                           |akka.actor.default-mailbox.mailbox-type = "akka.dispatch.UnboundedMailbox"
                                           |akka.actor.default-mailbox.mailbox-capacity = 1000
                                           |akka.actor.default-mailbox.mailbox-push-timeout-time = 10s
                                           |akka.actor.default-mailbox.stash-capacity = -1
                                           |akka.actor.deployment.default.dispatcher = ""
                                           |akka.actor.deployment.default.remote = ""
                                           |akka.actor.deployment.default.router = "from-code"
                                           |akka.actor.deployment.default.virtual-nodes-factor = 10
                                           |akka.actor.guardian-supervisor-strategy = "akka.actor.DefaultSupervisorStrategy"
                                           |akka.actor.internal-dispatcher.type = "Dispatcher"
                                           |akka.actor.internal-dispatcher.executor = "fork-join-executor"
                                           |akka.actor.internal-dispatcher.throughput = 5
                                           |akka.actor.internal-dispatcher.fork-join-executor.parallelism-min = 4
                                           |akka.actor.internal-dispatcher.fork-join-executor.parallelism-factor = 1.0
                                           |akka.actor.internal-dispatcher.fork-join-executor.parallelism-max = 64
                                           |akka.actor.mailbox.unbounded-queue-based.mailbox-type = "akka.dispatch.UnboundedMailbox"
                                           |akka.actor.mailbox.bounded-queue-based.mailbox-type = "akka.dispatch.BoundedMailbox"
                                           |akka.actor.mailbox.unbounded-deque-based.mailbox-type = "akka.dispatch.UnboundedDequeBasedMailbox"
                                           |akka.actor.mailbox.bounded-deque-based.mailbox-type = "akka.dispatch.BoundedDequeBasedMailbox"
                                           |akka.actor.mailbox.unbounded-control-aware-queue-based.mailbox-type = "akka.dispatch.UnboundedControlAwareMailbox"
                                           |akka.actor.mailbox.bounded-control-aware-queue-based.mailbox-type = "akka.dispatch.BoundedControlAwareMailbox"
                                           |akka.actor.mailbox.logger-queue.mailbox-type = "akka.event.LoggerMailboxType"
                                           |akka.actor.mailbox.requirements."akka.dispatch.UnboundedMessageQueueSemantics" = akka.actor.mailbox.unbounded-queue-based
                                           |akka.actor.mailbox.requirements."akka.dispatch.BoundedMessageQueueSemantics" = akka.actor.mailbox.bounded-queue-based
                                           |akka.actor.mailbox.requirements."akka.dispatch.DequeBasedMessageQueueSemantics" = akka.actor.mailbox.unbounded-deque-based
                                           |akka.actor.mailbox.requirements."akka.dispatch.UnboundedDequeBasedMessageQueueSemantics" = akka.actor.mailbox.unbounded-deque-based
                                           |akka.actor.mailbox.requirements."akka.dispatch.BoundedDequeBasedMessageQueueSemantics" = akka.actor.mailbox.bounded-deque-based
                                           |akka.actor.mailbox.requirements."akka.dispatch.MultipleConsumerSemantics" = akka.actor.mailbox.unbounded-queue-based
                                           |akka.actor.mailbox.requirements."akka.dispatch.ControlAwareMessageQueueSemantics" = akka.actor.mailbox.unbounded-control-aware-queue-based
                                           |akka.actor.mailbox.requirements."akka.dispatch.UnboundedControlAwareMessageQueueSemantics" = akka.actor.mailbox.unbounded-control-aware-queue-based
                                           |akka.actor.mailbox.requirements."akka.dispatch.BoundedControlAwareMessageQueueSemantics" = akka.actor.mailbox.bounded-control-aware-queue-based
                                           |akka.actor.mailbox.requirements."akka.event.LoggerMessageQueueSemantics" = akka.actor.mailbox.logger-queue
                                           |akka.actor.no-serialization-verification-needed-class-prefix = ["akka."]
                                           |akka.actor.provider = "local"
                                           |akka.actor.router.type-mapping.from-code = "akka.routing.NoRouter"
                                           |akka.actor.router.type-mapping.round-robin-pool = "akka.routing.RoundRobinPool"
                                           |akka.actor.router.type-mapping.round-robin-group = "akka.routing.RoundRobinGroup"
                                           |akka.actor.router.type-mapping.random-pool = "akka.routing.RandomPool"
                                           |akka.actor.router.type-mapping.random-group = "akka.routing.RandomGroup"
                                           |akka.actor.router.type-mapping.balancing-pool = "akka.routing.BalancingPool"
                                           |akka.actor.router.type-mapping.smallest-mailbox-pool = "akka.routing.SmallestMailboxPool"
                                           |akka.actor.router.type-mapping.broadcast-pool = "akka.routing.BroadcastPool"
                                           |akka.actor.router.type-mapping.broadcast-group = "akka.routing.BroadcastGroup"
                                           |akka.actor.router.type-mapping.scatter-gather-pool = "akka.routing.ScatterGatherFirstCompletedPool"
                                           |akka.actor.router.type-mapping.scatter-gather-group = "akka.routing.ScatterGatherFirstCompletedGroup"
                                           |akka.actor.router.type-mapping.tail-chopping-pool = "akka.routing.TailChoppingPool"
                                           |akka.actor.router.type-mapping.tail-chopping-group = "akka.routing.TailChoppingGroup"
                                           |akka.actor.router.type-mapping.consistent-hashing-pool = "akka.routing.ConsistentHashingPool"
                                           |akka.actor.router.type-mapping.consistent-hashing-group = "akka.routing.ConsistentHashingGroup"
                                           |akka.actor.serialize-creators = off
                                           |akka.actor.serialize-messages = off
                                           |akka.actor.unstarted-push-timeout = 10s
                                           |akka.coordinated-shutdown.default-phase-timeout = 5s
                                           |akka.coordinated-shutdown.terminate-actor-system = on
                                           |akka.coordinated-shutdown.exit-jvm = off
                                           |akka.coordinated-shutdown.run-by-jvm-shutdown-hook = on
                                           |akka.coordinated-shutdown.run-by-actor-system-terminate = on
                                           |akka.coordinated-shutdown.exit-code = 0
                                           |akka.coordinated-shutdown.phases.before-service-unbind = {}
                                           |akka.coordinated-shutdown.phases.service-unbind.depends-on = [before-service-unbind]
                                           |akka.coordinated-shutdown.phases.service-requests-done.depends-on = [service-unbind]
                                           |akka.coordinated-shutdown.phases.service-stop.depends-on = [service-requests-done]
                                           |akka.coordinated-shutdown.phases.before-cluster-shutdown.depends-on = [service-stop]
                                           |akka.coordinated-shutdown.phases.cluster-sharding-shutdown-region.timeout = 10s
                                           |akka.coordinated-shutdown.phases.cluster-sharding-shutdown-region.depends-on = [before-cluster-shutdown]
                                           |akka.coordinated-shutdown.phases.cluster-leave.depends-on = [cluster-sharding-shutdown-region]
                                           |akka.coordinated-shutdown.phases.cluster-exiting.timeout = 10s
                                           |akka.coordinated-shutdown.phases.cluster-exiting.depends-on = [cluster-leave]
                                           |akka.coordinated-shutdown.phases.cluster-exiting-done.depends-on = [cluster-exiting]
                                           |akka.coordinated-shutdown.phases.cluster-shutdown.depends-on = [cluster-exiting-done]
                                           |akka.coordinated-shutdown.phases.before-actor-system-terminate.depends-on = [cluster-shutdown]
                                           |akka.coordinated-shutdown.phases.actor-system-terminate.timeout = 10s
                                           |akka.coordinated-shutdown.phases.actor-system-terminate.depends-on = [before-actor-system-terminate]
                                           |akka.daemonic = off
                                           |akka.extensions = []
                                           |akka.fail-mixed-versions = on
                                           |akka.jvm-exit-on-fatal-error = on
                                           |akka.jvm-shutdown-hooks = on
                                           |akka.log-config-on-start = off
                                           |akka.log-dead-letters = off
                                           |akka.log-dead-letters-during-shutdown = off
                                           |akka.log-dead-letters-suspend-duration = infinite
                                           |akka.logger-startup-timeout = 5s
                                           |akka.loggers = ["akka.event.slf4j.Slf4jLogger"]
                                           |akka.loggers-dispatcher = "akka.actor.default-dispatcher"
                                           |akka.logging-filter = "akka.event.slf4j.Slf4jLoggingFilter"
                                           |akka.loglevel = "OFF"
                                           |akka.scheduler.tick-duration = 10ms
                                           |akka.scheduler.ticks-per-wheel = 512
                                           |akka.scheduler.implementation = "akka.actor.LightArrayRevolverScheduler"
                                           |akka.scheduler.shutdown-timeout = 5s
                                           |akka.stdout-loglevel = "OFF"
                                           |""".stripMargin),
      guardianProps = MailboxSelector.fromConfig("oscarcbls.supervisormailbox"))

    a
  }

  def wrapSupervisor(supervisorRef: ActorRef[MessagesToSupervisor], store: Store, verbose: Boolean)
                    (implicit system: ActorSystem[_]): Supervisor = {
    new Supervisor(supervisorRef, store, verbose, system)
  }

  def spawnSupervisor(context: ActorContext[_], verbose: Boolean): ActorRef[MessagesToSupervisor] = {
    context.spawn(createSupervisorBehavior(verbose), "supervisor")
  }

  def createSupervisorBehavior(verbose: Boolean = false, tic: Duration = Duration.Inf): Behavior[MessagesToSupervisor] =
    Behaviors.setup { context: ActorContext[MessagesToSupervisor] => new SupervisorActor(context, verbose, tic) }
}

class Supervisor(val supervisorActor: ActorRef[MessagesToSupervisor], m: Store, verbose: Boolean, implicit val system: ActorSystem[_]) {
  //TODO look for an adequate timeout or stopping mechanism
  implicit val timeout: Timeout = 1.hour

  import akka.actor.typed.scaladsl.AskPattern._

  //TODO: for the distributed version, regularly check that workers performing some wearch are still alive and working, otherwise, search must be restarted at another worker.

  def createLocalWorker(m: Store, search: Neighborhood): Unit = {
    val workerBehavior = WorkerActor.createWorkerBehavior(search.identifyRemotelySearcheableNeighbrhoods, m, this.supervisorActor, verbose)
    supervisorActor ! SpawnWorker(workerBehavior)
  }

  def spawnNewActor[T](behavior:Behavior[T],behaviorName:String): Future[ActorRef[T]] = {
    supervisorActor.ask[ActorRef[T]](ref => SpawnNewActor(behavior:Behavior[T],behaviorName:String, ref))
  }

  def nbWorkers: Int = {
    val ongoingRequest: Future[Int] = supervisorActor.ask[Int](ref => NbWorkers(ref))
    Await.result(ongoingRequest, atMost = 30.seconds)
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
    val e = new Exception(s"Crash happened at worker:${searchCrashed.worker}: \n${searchCrashed.exception.getMessage}\nwhen performing neighborhood:${searchCrashed.neighborhood}")
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
                      tic: Duration)
  extends AbstractBehavior[MessagesToSupervisor](context) {

  //message to self

  private case class Tic() extends MessagesToSupervisor
  //this one cannot be a control message.
  private case class StartSomeSearch() extends MessagesToSupervisor

  private val waitingSearches = scala.collection.mutable.Queue[SearchTask]()
  var nbLocalWorker: Int = 0
  var nbCustomSearchActor:Int = 0

  var neighborhoodToPreferredWorker: SortedMap[Int, ActorRef[MessageToWorker]] = SortedMap.empty
  private var allKnownWorkers: List[ActorRef[MessageToWorker]] = Nil
  private var idleWorkersAndTheirCurentModelID: List[(ActorRef[MessageToWorker],Option[Int])] = Nil
  //this one is a list, because the most common operations are add and takeFirst
  private var startingSearches: SortedMap[Long, (SearchTask, Long, ActorRef[MessageToWorker])] = SortedMap.empty
  //need to add, and remove regularly, based on ID
  private var ongoingSearches: SortedMap[Long, (SearchTask, ActorRef[MessageToWorker])] = SortedMap.empty
  private var totalStartedSearches = 0
  private var nextSearchID: Long = 0
  private var nextStartID: Long = 0 //search+worker

  override def onMessage(msg: MessagesToSupervisor): Behavior[MessagesToSupervisor] = {
    msg match {

      case Tic() =>
        context.log.info(status)

        tic match {
          case _: Infinite => ;
          case f: FiniteDuration => context.scheduleOnce(f, context.self, Tic())
        }

      case SpawnNewActor(behavior:Behavior[Any],behaviorName:String, replyTo) =>
        replyTo ! context.spawn(behavior, s"customSearchActor$nbCustomSearchActor$behaviorName")
        nbCustomSearchActor += 1

      case SpawnWorker(workerBehavior) =>
        context.spawn(workerBehavior, s"localWorker$nbLocalWorker")
        nbLocalWorker += 1

      case NbWorkers(replyTo) =>
        replyTo ! this.allKnownWorkers.size

      case NewWorkerEnrolled(workerRef: ActorRef[MessageToWorker]) =>
        allKnownWorkers = workerRef :: allKnownWorkers
        idleWorkersAndTheirCurentModelID = (workerRef,None) :: idleWorkersAndTheirCurentModelID
        context.self ! StartSomeSearch()
        context.log.info("new worker enrolled:" + workerRef.path)

      case StartSomeSearch() =>
        //context.log.info("StartSomeSearch")
        (waitingSearches.isEmpty, idleWorkersAndTheirCurentModelID) match {
          case (true, idleWorkers) if idleWorkers.nonEmpty => ;
            if (verbose) context.log.info(status)

          case (_, Nil) => ;
            if (verbose) context.log.info(status)

          case (false, _ :: _) =>

            def startSearch(search: SearchTask, worker: ActorRef[MessageToWorker], currentSolutionAtWorker:Option[Int]): Unit = {
              if (verbose) context.log.info(s"assigning search:${search.searchId} to worker:${worker.path}")
              val startID = nextStartID
              nextStartID = nextStartID + 1
              totalStartedSearches += 1

              val solutionForThisSearch = search.request.startSolutionOpt


              val simplifiedSearch = (solutionForThisSearch,currentSolutionAtWorker) match{
                case (Some(x),Some(y)) if x.solutionId == y => search.copy(request = search.request.copy(startSolutionOpt = None))
                case _ => search
              }

              implicit val responseTimeout: Timeout = 3.seconds
              context.ask[MessageToWorker, MessagesToSupervisor](worker, res => StartSearch(simplifiedSearch, startID, res)) {
                case Success(_: SearchStarted) => SearchStarted(search, startID, worker)
                case Success(_: SearchNotStarted) => SearchNotStarted(search, worker)
                case Failure(_) => SearchNotStarted(search, worker)
                case _ => SearchNotStarted(search, worker) // Default case
              }

              startingSearches = startingSearches + (startID -> (search, startID, worker))
            }

            val nbIdleWorkers = idleWorkersAndTheirCurentModelID.size
            val nbAvailableSearches = waitingSearches.size
            var nbSearchToStart = nbIdleWorkers min nbAvailableSearches

            var couldDequeue = true
            while (couldDequeue && nbSearchToStart != 0) {
              couldDequeue = false
              waitingSearches.dequeueFirst(searchTask => {
                val nID = searchTask.request.neighborhoodID.neighborhoodID
                val preferredWorkerOpt = neighborhoodToPreferredWorker.get(nID)
                preferredWorkerOpt match {
                  case Some(preferredWorker) =>
                    val newIdle = idleWorkersAndTheirCurentModelID.filter(_._1.path != preferredWorker.path)
                    if (newIdle.size != idleWorkersAndTheirCurentModelID.size) {
                      //start this one
                      //println("hotRestart" + searchTask.request.neighborhoodID)
                      val modelAtWorkerSide = idleWorkersAndTheirCurentModelID.filter(_._1.path == preferredWorker.path) match{
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
              neighborhoodToPreferredWorker = neighborhoodToPreferredWorker + (searchToStart.request.neighborhoodID.neighborhoodID -> worker._1)
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

      case SearchStarted(search, startID, worker) =>
        startingSearches.get(startID) match {
          case Some((search2, startID2, worker2)) if startID2 == startID =>
            require(search.searchId == search2.searchId)
            if (verbose) context.log.info(s"search:${search.searchId} start confirmed by worker:${worker.path}")
            ongoingSearches = ongoingSearches + (search.searchId -> (search2, worker2))
            startingSearches = startingSearches.-(startID)
          case _ =>
            if (verbose) context.log.warn(s"unexpected search:${search.searchId} start confirmed to Supervisor by worker:${worker.path}; asking for abort")
            worker ! AbortSearch(search.searchId)
        }

      case SearchNotStarted(search, worker) =>
        if (verbose) context.log.info(s"search:${search.searchId} could not be started by worker:${worker.path}")
        waitingSearches.enqueue(search)
        context.self ! StartSomeSearch()
      //we do not register the worker as available here because it will register itself through another call,
      // at least to show it is not completely crashed.

      case ReadyForWork(worker: ActorRef[MessageToWorker], completedSearchID: Option[Long], completedNeighborhoodIDAndMoveFound: Option[(Int,Boolean)], currentModelId:Option[Int]) =>
        if (verbose) context.log.info(s"got a worker ready:${worker.path}; finished search:$completedSearchID")

        require(allKnownWorkers contains worker)
        completedNeighborhoodIDAndMoveFound match {
          case None => ;
          case Some((s:Int,found)) =>
            if(!found){
              neighborhoodToPreferredWorker = neighborhoodToPreferredWorker.-(s)
            }
        }

        completedSearchID match{
          case Some(s) => ongoingSearches = ongoingSearches.-(s)
          case None => ;
        }

        idleWorkersAndTheirCurentModelID = (worker,currentModelId) :: idleWorkersAndTheirCurentModelID
        context.self ! StartSomeSearch()

      case GetNewUniqueID(replyTo:ActorRef[Long]) =>
        replyTo ! nextSearchID
        nextSearchID += 1

      case DelegateSearch(searchRequest, sendSearchResultTo, givenSearchId) =>

        val searchId = if(givenSearchId == -1){
          val x = nextSearchID
          nextSearchID += 1
          x
        } else givenSearchId

        if (verbose) context.log.info(s"got new waiting search:$searchId for :${sendSearchResultTo.path}")

        //now, we have a WorkGiver actor, we search for an available Worker or put this request on a waiting list.
        val theSearch = SearchTask(searchRequest, searchId, sendSearchResultTo)
        waitingSearches.enqueue(theSearch)
        context.self ! StartSomeSearch()

      case CancelSearchToSupervisor(searchID: Long,keepAliveIfOjBelow:Option[Long]) =>

        waitingSearches.dequeueFirst(_.searchId == searchID) match {
          case None =>
            //Search was already ongoing on some worker
            //the search is already being processed by some search worker.

            ongoingSearches.get(searchID) match {
              case Some((search, worker)) =>
                if (verbose) context.log.info(s"got cancel request for ongoing search:$searchID; forward to worker:${worker.path}")
                worker ! AbortSearch(search.searchId,keepAliveIfOjBelow)
              case None =>

                startingSearches.find(_._2._1.searchId == searchID) match {
                  case Some((startID2, (search, startID3, worker))) =>
                    startingSearches = startingSearches.-(startID2)
                    if (verbose) context.log.info(s"got cancel request for starting search:$searchID forward to worker:${worker.path}")
                    worker ! AbortSearch(search.searchId,keepAliveIfOjBelow)
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

      case ShutDown(replyTo: Option[ActorRef[Unit]]) =>
        //ask for a coordinated shutdown of all workers

        context.log.info(s"got Shutdown command; forwarding to workers")
        for (worker <- allKnownWorkers) {
          worker ! ShutDownWorker()
        }
        replyTo match {
          case Some(x) => x ! ()
          case None => ;
        }

        tic match {
          case _: Infinite => ;
          case f: FiniteDuration =>
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
    case f: FiniteDuration => context.scheduleOnce(f, context.self, Tic())
  }

  def status: String = {
    s"workers(total:${allKnownWorkers.size} busy:${allKnownWorkers.size - idleWorkersAndTheirCurentModelID.size}) searches(waiting:${waitingSearches.size} starting:${startingSearches.size} running:${ongoingSearches.size} totalStarted:$totalStartedSearches)"
  }
}
