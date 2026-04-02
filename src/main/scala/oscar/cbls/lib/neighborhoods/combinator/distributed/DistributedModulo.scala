package oscar.cbls.lib.neighborhoods.combinator.distributed

import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.slf4j.Logger
import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.distributed.computation._
import oscar.cbls.core.distributed.protocol._
import oscar.cbls.core.distributed.search.RemotelyCallableTask
import oscar.cbls.core.search._
import oscar.cbls.lib.neighborhoods.combinator.distributed.DistributedModulo.ModuloRange

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.util.{Failure, Success, Try}

object DistributedModulo {

  /** This combinator explores a neighborhood in a distributed way by splitting it using a modulo
    * approach. The user specifies a function that instantiates a neighborhood with a
    * [[ModuloRange]] parameter.
    *
    * @param moduloNeighborhood
    *   You must define here how the [[ModuloRange]] is used to split the neighborhood.
    *
    * Typically, the neighborhood will explore the neighbors numbered as: {offset, offset+step,
    * offset+2*step, offset+3*step, .... }. This splitting is interesting because we might sometime
    * not have an idea of how many neighbors must be explored, so it will balance the work naturally
    * among workers. Furthermore, some neighbors might be more costly to explore than others, for
    * instance, the [[oscar.cbls.lib.neighborhoods.Swap]] has symmetry elimination, so that indices
    * explored by the first search Zone have very different cost.
    *
    * It is also possible to split the neighborhood in this way: {offset*N/step,offset*N/step+1,
    * offset*N/step+2 , ..., offset*N/step}.
    *
    * @param nbWorkerToNbClusters
    *   a function to specify in how many parts the neighborhood is cut. By default, it is cut in as
    *   many parts as there are available workers, but you cna influence this split here by changing
    *   this default behavior.
    * @param first
    *   true for the first move, false for the best move.
    * @return
    *   A DistributedModulo combinator.
    */
  def apply(
    moduloNeighborhood: ModuloRange => Neighborhood,
    nbWorkerToNbClusters: Int => Int = a => a,
    first: Boolean = true
  ): DistributedModulo =
    new DistributedModulo(moduloNeighborhood, nbWorkerToNbClusters, first)

  /** This combinator will explore the neighborhoods in parallel. It returns the first or best move
    * according the 'first' parameter. It is a simplified version of the [[DistributedModulo]]
    * combinator, but might be less efficient. On the other hand, the sb neighborhoods can have
    * hotRestart
    * @param neighborhoods
    *   the neighborhoods to explore. You can use hotRestart on them, however, there is no guarantee
    *   that it will work since hotRestart
    * @param first
    *   true for the first move, false for the best move.
    * @return
    *   a [[NeighborhoodCombinator]] that explores the neighborhoods in parallel.
    */
  def distributed(neighborhoods: Iterable[Neighborhood], first: Boolean): DistributedModulo = {
    val neighborhoodArray = neighborhoods.toArray
    DistributedModulo(
      moduloNeighborhood = (r: ModuloRange) => neighborhoodArray(r.offset),
      nbWorkerToNbClusters = _ => neighborhoodArray.length,
      first = first
    )
  }

  /** A useful class to skip part of an iterable, to use in the [[DistributedModulo]] This iterable
    * transforms a base iterable by skipping all its content except the element coming at the
    * following positions: {modulo.offset, modulo.offset+modulo.step, modulo.offset+2*modulo.step,
    * modulo.offset+3*modulo.step, .... }.
    *
    * @param modulo
    *   defines ow the base Iterable will be skipped
    * @param base
    *   the base iterable.
    * @tparam T
    *   the type parameter of the Iterable
    */
  private case class ModuloIterable[T](modulo: ModuloRange, base: Iterable[T]) extends Iterable[T] {
    override def iterator: Iterator[T] = new Iterator[T] {
      private val baseIterator = base.iterator
      for (_ <- 0 until modulo.offset - 1) {
        if (baseIterator.hasNext) {
          baseIterator.next()
        }
      }
      private var baseHasNext = baseIterator.hasNext

      override def hasNext: Boolean = baseHasNext

      override def next(): T = {
        val toReturn = baseIterator.next()
        for (_ <- 0 until modulo.step - 1) {
          if (baseIterator.hasNext) {
            baseIterator.next()
          }
        }
        baseHasNext = baseIterator.hasNext
        toReturn
      }
    }
  }

  /** The parameters defining a partial exploration of the neighborhood.
    *
    * Two example of splitting a neighborhood:
    *   - split in a modulo way: {offset, offset+step, offset+2*step, offset+3*step, .... }
    *   - divide into big chunks, in case you know how many neighbors must be explored:
    *     {offset*N/step,offset*N/step+1, * offset*N/step+2 , ..., offset*N/step}.
    *
    * @param step
    *   The step of the exploration range.
    * @param offset
    *   The offset on the exploration range.
    */
  case class ModuloRange(step: Int, offset: Int) {

    /** applies this modulo to the base iterable and returns an iterable that will skip all the
      * content of the base iterable except the element coming at the following positions:
      * {modulo.offset, modulo.offset+modulo.step, modulo.offset+2*modulo.step, *
      * modulo.offset+3*modulo.step, .... }.
      *
      * @param base
      *   the base iterable
      * @tparam T
      *   the type parameter of the base iterable
      * @return
      *   a [[ModuloIterable]]
      */
    def apply[T](base: Iterable[T]): Iterable[T] = {
      ModuloIterable[T](this, base)
    }
  }
}

/** A task parameter that is inserted in the protocol for the [[DistributedModulo]] combinator
  * @param startSolution
  *   the start solution
  * @param objective
  *   the objective to optimize
  * @param parameters
  *   the modulo range that restricts the range of the neighborhood
  */
case class GetModuloMove(
  startSolution: StoreIndependentSolution,
  objective: StoreIndependentObjective,
  parameters: ModuloRange
) extends TaskParameters

/** This combinator explores a neighborhood in a distributed way by splitting it using a modulo
  * approach. The user specifies a function that instantiates a neighborhood with a [[ModuloRange]]
  * parameter.
  *
  * @param moduloNeighborhood
  *   You must define here how the [[ModuloRange]] is used to split the neighborhood.
  *
  * Typically, the neighborhood will explore the neighbors numbered as: {offset, offset+step,
  * offset+2*step, offset+3*step, .... }. This splitting is interesting because we might sometime
  * not have an idea of how many neighbors must be explored, so it will balance the work naturally
  * among workers. Furthermore, some neighbors might be more costly to explore than others, for
  * instance, the [[oscar.cbls.lib.neighborhoods.Swap]] has symmetry elimination, so that indices
  * explored by the first search Zone have very different cost.
  *
  * It is also possible to split the neighborhood in this way: {offset*N/step,offset*N/step+1,
  * offset*N/step+2 , ..., offset*N/step}.
  *
  * @param nbWorkerToNbClusters
  *   a function to specify in how many parts the neighborhood is cut. By default, it is cut in as
  *   many parts as there are available workers, but you cna influence this split here by changing
  *   this default behavior.
  * @param first
  *   true for the first move, false for the best move.
  */
class DistributedModulo(
  moduloNeighborhood: ModuloRange => Neighborhood,
  nbWorkerToNbClusters: Int => Int = a => a,
  first: Boolean
) extends NeighborhoodCombinator("DistributedModulo", Nil) {

  protected var taskClass: Int                   = -1
  protected var searchConnector: SearchConnector = _

  /** To declare the remotely callable task for this distributed neighborhood. We cannot use the
    * standard one because teh remotely callable neighborhood is not just a neighborhood; it must be
    * instantiated based on a [[ModuloRange]]
    * @param searchConnector
    *   The search connector used to communicate with the distributed search machinery.
    */
  override def declareRemotelyCallableTasks(searchConnector: SearchConnector): Unit = {
    this.searchConnector = searchConnector
    taskClass = searchConnector.declareRemotelyCallableTask(
      new RemotelyCallableModuloNeighborhood(moduloNeighborhood)
    )
  }

  /** The remote task that performs the search based on the [[ModuloRange]]. This is an internal
    * private class since no one should ever use it except the [[DistributedModulo]].
    * @param moduloNeighborhood
    *   The neighborhood to instantiate and explore, based on a [[ModuloRange]].
    */
  private class RemotelyCallableModuloNeighborhood(moduloNeighborhood: ModuloRange => Neighborhood)
      extends RemotelyCallableTask {
    override def performTask(
      description: TaskParameters,
      shouldStop: () => Boolean,
      computationSupport: SearchConnector,
      log: Logger,
      verbosityLevel: Int,
      taskId: Long,
      taskClass: Int
    ): ActualResult = {
      description match {
        case g: GetModuloMove =>
          val neighborhood = moduloNeighborhood(g.parameters)
          computationSupport.attachSolutionToStore(g.startSolution).restoreSolution()
          neighborhood.getMove(computationSupport.attachObjectiveToStore(g.objective)) match {
            case NoMoveFound => TaskResultNoMoveFound
            case MoveFound(move) =>
              TaskResultMove(computationSupport.detachMoveFromStore(move))
          }
      }
    }
  }

  /** The main method that explores the combinator
    * @param objective
    *   The Objective of the search
    * @return
    *   The search result, either [[oscar.cbls.core.search.MoveFound]] or
    *   [[oscar.cbls.core.search.NoMoveFound]]
    */
  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {

    val supervisor = searchConnector.supervisor

    // Used to block the main thread and wait for the result of the search
    val searchResultPromise = Promise[Try[SearchResult]]()

    /** The first state of the search actor: waiting for the status repost, so that we know how many
      * workers are available and ask for the searchIDs
      * @return
      *   The next behavior of the actor.
      */
    def askSearchIDs(): Behavior[MessageToSearch] = {
      Behaviors.receive { (context: ActorContext[MessageToSearch], command: MessageToSearch) =>
        command match {
          case StatusReport(nbWorkers, _, _) =>
            val nbClusters = nbWorkerToNbClusters(nbWorkers)
            supervisor ! GetNewUniqueTaskIds(context.self, nbClusters, context.self)
            startSearch(nbClusters)
          case Crash(error) =>
            searchResultPromise.complete(Success(Failure(error)))
            Behaviors.stopped
          case _: ResultObtained =>
            // should not happen
            Behaviors.same
          case _: NewUniqueTaskIds =>
            // should not happen
            Behaviors.same
        }
      }
    }

    /** The second state of the search actor: waiting for the searchIDs and starting all search
      * tasks once we have the search IDs
      * @param nbClusters
      *   The number of clusters, based on the number of available workers.
      * @return
      *   The next behavior of the actor.
      */
    def startSearch(nbClusters: Int): Behavior[MessageToSearch] = {
      Behaviors.receive { (context: ActorContext[MessageToSearch], command: MessageToSearch) =>
        command match {
          case NewUniqueTaskIds(firstTaskId, _) =>
            val startSolDetached     = searchConnector.saveDetachedSolution
            val objDetachedFromStore = searchConnector.detachObjectiveFromStore(objective)

            supervisor ! CreateTasks(
              context.self,
              (0 until nbClusters).toList.map(offset => {
                val task =
                  Task(
                    firstTaskId + offset,
                    taskClass,
                    GetModuloMove(
                      startSolDetached,
                      objDetachedFromStore,
                      ModuloRange(nbClusters, offset)
                    )
                  )

                (task, context.self)
              })
            )
            stopOrWaitResult(remainingAnswers = nbClusters, None, context)
          case StatusReport(_, _, _) =>
            Behaviors.same
          case Crash(throwable) =>
            searchResultPromise.complete(Success(Failure(throwable)))
            Behaviors.stopped
          case _: ResultObtained =>
            // should not happen
            Behaviors.same
        }
      }
    }

    /** The wait state of the search actor, possible stops in case we have all the answers.
      * @param remainingAnswers
      *   How many answers are we still waiting for.
      * @param bestResultSoFar
      *   The best result so far, possibly none if we did not receive anything or if we only receive
      *   [[NoMoveFound]].
      * @param context
      *   The cotext, so that we can stop the search if remainingAnswers==0.
      * @return
      *   The next behavior of the actor.
      */
    def stopOrWaitResult(
      remainingAnswers: Int,
      bestResultSoFar: Option[TaskResultMove],
      context: ActorContext[MessageToSearch]
    ): Behavior[MessageToSearch] = {
      if (remainingAnswers == 0) {
        supervisor ! CancelAllMyRemainingTasks(context.self, searchFinished = true)
        bestResultSoFar match {
          case Some(move) =>
            searchResultPromise.complete(
              Success(Success(MoveFound(searchConnector.attachMoveToStore(move.move))))
            )
          case None =>
            searchResultPromise.complete(Success(Success(NoMoveFound)))
        }
        Behaviors.stopped
      } else {
        waitResult(remainingAnswers, bestResultSoFar)
      }
    }

    /** The wait state of the search actor, only if remainingAnswers>0
      * @param remainingAnswers
      *   How many answers are we still waiting for.
      * @param bestResultSoFar
      *   The best result so far, possibly none if we did not receive anything or if we only receive
      *   [[NoMoveFound]].
      * @return
      *   The next behavior of the actor.
      */
    def waitResult(
      remainingAnswers: Int,
      bestResultSoFar: Option[TaskResultMove]
    ): Behavior[MessageToSearch] = {
      require(remainingAnswers > 0)
      Behaviors.receive { (context: ActorContext[MessageToSearch], command: MessageToSearch) =>
        command match {
          case Crash(throwable) =>
            searchResultPromise.complete(Success(Failure(throwable)))
            Behaviors.stopped

          case ResultObtained(TaskResult(_, _, _, result)) =>
            result match {
              case TaskResultNoMoveFound =>
                stopOrWaitResult(remainingAnswers - 1, bestResultSoFar, context)

              case newMove: TaskResultMove =>
                val (nextBest, shouldStop) = bestResultSoFar match {
                  case Some(move) =>
                    if (objective.isValueNewBest(move.move.objAfter, newMove.move.objAfter)) {
                      (Some(newMove), first)
                    } else {
                      (Some(move), first)
                    }
                  case None => (Some(newMove), first)
                }

                if (shouldStop) {
                  stopOrWaitResult(0, nextBest, context)
                } else {
                  stopOrWaitResult(remainingAnswers - 1, nextBest, context)
                }
            }
          case _: NewUniqueTaskIds =>
            // should not happen
            Behaviors.same
          case StatusReport(_, _, _) =>
            // cannot change the clustering since it is already sent
            Behaviors.same
        }
      }
    }

    /// Start the search actor
    supervisor ! SpawnSearchActorAndStatusRequest(Behaviors.setup[MessageToSearch](_ => {
      askSearchIDs()
    }))

    /// Wait for the search actor to return its result.
    Await.result(searchResultPromise.future, Duration.Inf) match {
      case Failure(exception) =>
        val e = new Exception(exception.toString)
        e.setStackTrace((exception.getStackTrace.toList ::: e.getStackTrace.toList).toArray)
        throw e
      case Success(value) => value
    }
  }
}
