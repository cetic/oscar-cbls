package oscar.cbls.core.distrib

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import org.slf4j.{Logger, LoggerFactory}
import oscar.cbls.core.computation.{Solution, Store}

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.SortedMap
import scala.concurrent.{ExecutionContext, Future}

sealed trait MessageToWorker
final case class StartSearch(search: SearchRequest,
                             startID: Long,
                             replyTo: ActorRef[MessagesToSupervisor]) extends MessageToWorker
final case class AbortSearch(searchId: Long,
                             keepAliveIfOjBelow:Option[Long] = None) extends MessageToWorker
final case class WrappedSearchEnded(searchId:Long) extends MessageToWorker
case object ShutDownWorker extends MessageToWorker
final case class Ping(replyTo: ActorRef[ExternalWorkerState]) extends MessageToWorker
final case class GetStatisticsFor(neighborhood:RemoteTaskIdentification,
                                  indice:Int,
                                  replyTo: ActorRef[(Int,List[Array[String]])]) extends MessageToWorker

sealed trait ExternalWorkerState
case class WorkerBusy(searchId: Long, durationMs:Long) extends ExternalWorkerState
case class WorkerAborting(searchID: Long) extends ExternalWorkerState
case object WorkerIdle extends ExternalWorkerState
case object WorkerShuttingDown extends ExternalWorkerState

object WorkerActor {
  /////
  def startWorkerAndActorSystem(neighborhoods: SortedMap[Int, RemoteNeighborhood],
                                m: Store,
                                master: ActorRef[MessagesToSupervisor],
                                workerName: String = "worker",
                                verbose: Boolean): ActorSystem[MessageToWorker] = {

    val startLogger: Logger = LoggerFactory.getLogger("WorkerObject")
    startLogger.info("Starting actor system and worker:" + workerName)
    ActorSystem(createWorkerBehavior(neighborhoods, m, master, verbose, workerName), workerName)
  }

  def createWorkerBehavior(neighborhoods: SortedMap[Int, RemoteTask],
                           m: Store,
                           master: ActorRef[MessagesToSupervisor],
                           verbose: Boolean = false,
                           workerName:String): Behavior[MessageToWorker] = {
    new WorkerActor(neighborhoods, m, master, verbose, workerName).initBehavior()
  }

  def spawnWorker(neighborhoods: SortedMap[Int, RemoteNeighborhood],
                  m: Store,
                  master: ActorRef[MessagesToSupervisor],
                  context: ActorContext[_],
                  workerName: String = "worker",
                  verbose: Boolean): ActorRef[MessageToWorker] = {
    context.spawn(createWorkerBehavior(neighborhoods, m, master, verbose,workerName), workerName)
  }
}

class WorkerActor(remoteTasks: SortedMap[Int, RemoteTask],
                  m: Store,
                  master: ActorRef[MessagesToSupervisor],
                  verbose: Boolean,
                  workerName:String) {

  sealed trait WorkerState
  case class IAmBusy(search: SearchRequest, started:Long) extends WorkerState
  case class Aborting(search: SearchRequest) extends WorkerState
  case object Idle extends WorkerState
  case object ShuttingDown extends WorkerState

  // This is the single thread that is ready to perform all computation.
  // There is at most one computation per worker at any point in time, so the threadPool is 1.
  private val executorForComputation = Executors.newFixedThreadPool(1,
    new java.util.concurrent.ThreadFactory {
      final private val threadNumber = new AtomicInteger(1)
      override def newThread(r: Runnable): Thread = {
        val t = new Thread(null, r, "workerThread" + threadNumber.getAndIncrement, 0)
        t.setDaemon(false)
        t.setPriority(Thread.MAX_PRIORITY)
        t
      }
    }
  )

  private val executionContextForComputation: ExecutionContext = ExecutionContext.fromExecutor(executorForComputation)

  //This is a shared variable. it is not good, but that's the only way to send the abort signal to the Future that contains the computation.
  //Scala requires the final and volatile flags

  @volatile
  private final var nbExploredNeighborhoods: Int = 0

  @volatile
  private final var nbAbortedNeighborhoods: Int = 0

  @volatile
  private final var currentNeighborhood: RemoteTask = null

  @volatile
  final var currentSolOpt:Option[(Solution,SolutionID)] = None

  def initBehavior(): Behavior[MessageToWorker] = {
    Behaviors.setup { context =>
      master ! NewWorkerEnrolled(context.self)
      if (verbose) context.log.info(s"ready for work nbNeighborhoods:${remoteTasks.size}")
      next(Idle)
    }
  }

  private def next(state: WorkerState): Behavior[MessageToWorker] = {
    Behaviors.receive { (context, command) =>
      command match {
        case Ping(replyTo) =>
          replyTo ! (state match {
            case IAmBusy(search,started) => WorkerBusy(search.uniqueSearchId, System.currentTimeMillis() - started)
            case Aborting(search) => WorkerAborting(search.uniqueSearchId)
            case Idle => WorkerIdle
            case ShuttingDown => WorkerShuttingDown
          })
          Behaviors.same

        case GetStatisticsFor(neighborhood,indice,replyTo) =>
          state match {
            case Idle =>
              remoteTasks(neighborhood.taskId) match {
                case neighborhood1: RemoteNeighborhood =>
                  replyTo ! ((indice, neighborhood1.neighborhood.
                    collectProfilingStatistics.map(profilingLine => {
                    profilingLine(0) = workerName + "." + profilingLine(0)
                    profilingLine
                  })))
                case _ =>
                  replyTo!((indice,Nil))
              }
            case _ => ;
              //TODO: we should enqueue this query somewhere and answer it later
              replyTo!((indice,Nil))
          }
          Behaviors.same

        case StartSearch(newSearch, startID, replyTo) =>
          state match {
            case ShuttingDown =>
              Behaviors.same

            case IAmBusy(_, _) =>
              //we do not tell anything to the supervisor; it will have to find another slave
              if (verbose) context.log.info(s"got command for start search:${newSearch.uniqueSearchId} but already busy")
              replyTo ! SearchNotStarted(newSearch.uniqueSearchId, startID, context.self)
              Behaviors.same

            case Aborting(_) =>
              //TODO: il faudrait pouvoir stocker cette recherche en local ici pour déjà avoir la recherche suivante
              // en cas d'abort
              if (verbose) context.log.info(s"got command for start search:${newSearch.uniqueSearchId} but already busy aborting a search")
              replyTo ! SearchNotStarted(newSearch.uniqueSearchId, startID, context.self)
              Behaviors.same

            case Idle =>
              if (verbose) context.log.info(s"starting search:${newSearch.uniqueSearchId} neighborhood:${newSearch.remoteTaskId}")
              this.nbExploredNeighborhoods += 1
              currentNeighborhood = remoteTasks(newSearch.remoteTaskId.taskId)
              // This is the thread of the search, expressed as a future.
              Future {
                //TODO nothing can happen after the future is bound, opportunity to improve and postpone cleaning tasks?
                try {
                  currentSolOpt = currentNeighborhood.doTask(newSearch, m, currentSolOpt, Some(context.self.path.toString))
                } catch {
                  case e:Throwable =>
                    newSearch.sendResultTo ! SearchCrashed(newSearch.uniqueSearchId,Some(newSearch.remoteTaskId),e,context.self)
                    master ! Crash(context.self)
                }
                context.self ! WrappedSearchEnded(newSearch.uniqueSearchId)
              }(executionContextForComputation)
              // While search computes, notify it has started
              replyTo ! SearchStarted(newSearch.uniqueSearchId, startID, context.self)
              next(IAmBusy(newSearch,System.currentTimeMillis()))
          }

        case AbortSearch(searchId, keepAliveIfOjBelow:Option[Long]) =>
          state match {
            case ShuttingDown =>
              Behaviors.same

            case IAmBusy(search,_) =>
              if (searchId == search.uniqueSearchId) {
                currentNeighborhood match{
                  case null => //RAS
                    //otherwise, ignore.
                    Behaviors.same

                  case neighborhood:RemoteNeighborhood =>
                    val mustAbort:Boolean = neighborhood != null && (
                      if (keepAliveIfOjBelow.isDefined) neighborhood.bestObjSoFar >= keepAliveIfOjBelow.get
                      else true
                    )
                    if (mustAbort) {
                      if (verbose) context.log.info(s"got abort command for search:$searchId; aborting")
                      neighborhood.abort()
                      nbAbortedNeighborhoods += 1
                      next(Aborting(search))
                    } else {
                      if (verbose) context.log.info(s"ignoring conditional abort command, for search:$searchId bestOBj:${neighborhood.bestObjSoFar} < threshold:${keepAliveIfOjBelow.get}")
                      Behaviors.same
                    }

                  case t:RemoteTask =>
                    t.abort()
                    nbAbortedNeighborhoods += 1
                    next(Aborting(search))
                }

              } else {
                if (verbose) context.log.info(s"got abort command for search:$searchId but busy on search:${search.uniqueSearchId}; ignoring")
                //otherwise, ignore.
                Behaviors.same
              }

            case Aborting(search) =>
              //this is an error; ignore
              if (verbose) context.log.info(s"got abort command for search:$searchId; and was already aborting from ${search.uniqueSearchId}; ignoring")
              Behaviors.same

            case Idle =>
              //this is possibly an error
              if (verbose) context.log.info(s"got abort command for search:$searchId; and was idle; ignoring")
              Behaviors.same
          }

        case WrappedSearchEnded(uniqueId) =>
          // send result to supervisor
          state match {
            case IAmBusy(search,_) =>
              require(search.uniqueSearchId == uniqueId)
              if (verbose) context.log.info(s"finished search:${search.uniqueSearchId} neighborhood:${search.remoteTaskId}")
              master ! ReadyForWork (
                context.self,
                Some(search.uniqueSearchId),
                currentSolOpt.map(_._2)
              )
              next(Idle)

            case Aborting(search) =>
              //ok, we've done it for nothing.
              if (verbose) context.log.info(s"aborted search:${search.uniqueSearchId}")
              master ! ReadyForWork(context.self, Some(search.uniqueSearchId),currentSolOpt.map(_._2))
              next(Idle)

            case Idle =>
              //this is an error
              if (verbose) context.log.info(s"finished search $uniqueId and was actually idle??")
              Behaviors.same

            case ShuttingDown =>
              if (verbose) context.log.info(s"shutdown; explored $nbExploredNeighborhoods neighborhoods; aborted $nbAbortedNeighborhoods")
              executorForComputation.shutdown()
              Behaviors.stopped
          }

        case ShutDownWorker =>
          state match {
            case _:IAmBusy =>
              currentNeighborhood.abort() //shared variable
              //we kill the other thread
              if (verbose) context.log.info(s"aborting prior to shutdown")
              next(ShuttingDown)

            case _:Aborting =>
              if (verbose) context.log.info(s"got shutdown command")
              next(ShuttingDown)

            case Idle =>
              if (verbose) context.log.info(s"shutdown; explored $nbExploredNeighborhoods neighborhoods; aborted $nbAbortedNeighborhoods")
              executorForComputation.shutdown()
              Behaviors.stopped

            case ShuttingDown =>
              if (verbose) context.log.info(s"got shutdown command")
              Behaviors.same
          }
      }
    }
  }
}
