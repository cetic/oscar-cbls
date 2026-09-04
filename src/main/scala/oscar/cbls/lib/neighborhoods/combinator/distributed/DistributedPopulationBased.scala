package oscar.cbls.lib.neighborhoods.combinator.distributed

import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import oscar.cbls.core.computation.{Solution, Store}
import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.distributed.computation._
import oscar.cbls.core.distributed.protocol._
import oscar.cbls.core.distributed.search.RemotelySearchableNeighborhood
import oscar.cbls.core.search._
import oscar.cbls.lib.neighborhoods.combinator.{LoadSolutionMove, StoreIndependentLoadSolutionMove}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.util.{Failure, Success, Try}

object DistributedPopulationBased {

  /** Creates a distributed population-based search combinator with per-individual data.
    *
    * This combinator implements a population-based meta-heuristic where individual diversification
    * is distributed across workers. It maintains a population of solutions, each carrying
    * user-defined data of type `D`, and repeatedly:
    *   1. Calls the `step` function to decide which neighborhoods to apply to each individual
    *   2. Distributes the diversification tasks to workers
    *   3. Selects the best solutions in the population
    *
    * The data `D` stays entirely on the supervisor and is never serialized to workers.
    *
    * @tparam D
    *   the type of data associated to each individual
    * @param neighborhoods
    *   The neighborhoods available for diversifying individuals. Workers must have identical copies.
    *   The `genChildren` function returned by `step` must return neighborhoods from this array
    *   (compared by reference equality).
    * @param initData
    *   Generates data for the initial individual.
    * @param step
    *   Controls the iteration logic. Receives the iteration number and the data of all individuals
    *   in the current population. Returns `None` to stop, or
    *   `Some((genChildren, nbToKeep, alternateObjective))` where:
    *   - `genChildren: (Solution, D) => (Boolean, List[(Neighborhood, D)])` receives the solution
    *     and data of an individual, and returns:
    *     - whether the parent must be kept in the next generation (keepOld)
    *     - a list of `(neighborhood, childData)` pairs specifying which neighborhoods to apply and
    *       the data for each resulting child. The returned `Neighborhood` objects must be elements
    *       of the `neighborhoods` array (reference equality).
    *   - `nbToKeep` is how many individuals to keep after selection
    *   - `alternateObjective` is an optional [[Objective]] that, when provided, is used for both
    *     generation (workers optimize for it) and selection (supervisor sorts individuals by it).
    *     The main objective is still used for anytime-best tracking and final result comparison.
    * @param store
    *   The store, used for solution management.
    * @param maxIt
    *   The maximal number of iterations.
    * @param saveAnytimeBest
    *   If true, the best solution ever encountered is preserved across iterations.
    * @param filterRedundantElements
    *   If true, identical solutions in the population are filtered out.
    * @param dropIfNoMoveFound
    *   If true, an individual is dropped if no move is found by the neighborhood. If false, the
    *   parent is kept.
    * @param name
    *   A name for this combinator used in logs.
    */
  def apply[D](
    neighborhoods: Array[Neighborhood],
    initData: () => D,
    step: (
      Int,
      List[D]
    ) => Option[((Solution, D) => (Boolean, List[(Neighborhood, D)]), Int, Option[Objective])],
    store: Store,
    maxIt: Int = Int.MaxValue,
    saveAnytimeBest: Boolean = false,
    filterRedundantElements: Boolean = true,
    dropIfNoMoveFound: Boolean = false,
    name: String = "DistributedPopulationBased"
  ): DistributedPopulationBased[D] =
    new DistributedPopulationBased(
      neighborhoods,
      initData,
      step,
      store,
      maxIt,
      saveAnytimeBest,
      filterRedundantElements,
      dropIfNoMoveFound,
      name
    )

  /** Simplified API: all neighborhoods are applied uniformly to every individual.
    *
    * This is a convenience wrapper that uses `Unit` as the data type. Every individual gets
    * diversified by running all neighborhoods, and the `keepOld` flag applies globally.
    *
    * @param neighborhoods
    *   The neighborhoods used for diversifying individuals.
    * @param step
    *   Controls iteration logic: `(iteration, popSize) => Option[nbToKeep]`.
    * @param store
    *   The store.
    * @param maxIt
    *   Maximum number of iterations.
    * @param saveAnytimeBest
    *   Preserve the best solution ever encountered.
    * @param filterRedundantElements
    *   Filter identical solutions.
    * @param dropIfNoMoveFound
    *   Drop individuals when no move is found.
    * @param keepOld
    *   Keep parent alongside children.
    * @param name
    *   Name for logging.
    */
  def simple(
    neighborhoods: Array[Neighborhood],
    step: (Int, Int) => Option[Int],
    store: Store,
    maxIt: Int = Int.MaxValue,
    saveAnytimeBest: Boolean = false,
    filterRedundantElements: Boolean = true,
    dropIfNoMoveFound: Boolean = false,
    keepOld: Boolean = false,
    name: String = "DistributedPopulationBased"
  ): DistributedPopulationBased[Unit] = {
    val allNeighborhoods = neighborhoods.toList
    val fullStep: (
      Int,
      List[Unit]
    ) => Option[
      ((Solution, Unit) => (Boolean, List[(Neighborhood, Unit)]), Int, Option[Objective])
    ] =
      (it, dataList) =>
        step(it, dataList.size).map { nbToKeep =>
          val genChildren: (Solution, Unit) => (Boolean, List[(Neighborhood, Unit)]) =
            (_, _) => (keepOld, allNeighborhoods.map(n => (n, ())))
          (genChildren, nbToKeep, None)
        }
    new DistributedPopulationBased(
      neighborhoods,
      () => (),
      fullStep,
      store,
      maxIt,
      saveAnytimeBest,
      filterRedundantElements,
      dropIfNoMoveFound,
      name
    )
  }
}

/** A distributed population-based search combinator with per-individual data.
  *
  * This combinator distributes individual diversification across workers using the existing
  * distributed search framework. Population management (iterations, selection, filtering,
  * per-individual data) runs on the supervisor, while `doAllMoves` on each individual is executed
  * by workers.
  *
  * The data `D` stays entirely on the supervisor and is never serialized.
  *
  * @tparam D
  *   the type of data associated to each individual
  * @param neighborhoods
  *   The neighborhoods available for diversifying individuals.
  * @param initData
  *   Generates data for the initial individual.
  * @param step
  *   Controls iteration logic:
  *   `(iteration, dataList) => Option[(genChildren, nbToKeep, alternateObjective)]`.
  * @param store
  *   The store.
  * @param maxIt
  *   Maximum number of iterations.
  * @param saveAnytimeBest
  *   Preserve the best solution ever encountered.
  * @param filterRedundantElements
  *   Filter identical solutions.
  * @param dropIfNoMoveFound
  *   Drop individuals when no move is found.
  * @param name
  *   Name for logging.
  */
class DistributedPopulationBased[D](
  neighborhoods: Array[Neighborhood],
  initData: () => D,
  step: (
    Int,
    List[D]
  ) => Option[
    ((Solution, D) => (Boolean, List[(Neighborhood, D)]), Int, Option[Objective])
  ],
  store: Store,
  maxIt: Int,
  saveAnytimeBest: Boolean,
  filterRedundantElements: Boolean,
  dropIfNoMoveFound: Boolean,
  name: String
) extends NeighborhoodCombinator(name, neighborhoods.toList) {

  protected var neighborhoodTaskClasses: Array[Int] = _
  protected var searchConnector: SearchConnector    = _

  /** Stores an individual in the population. Uses store-independent solutions so that they can
    * travel between supervisor and workers. The data `D` stays on the supervisor.
    *
    * @param solution
    *   the store-independent solution
    * @param objForSelection
    *   the value of the objective that was optimized to build this individual; this is the
    *   alternate objective when the step function provided one, and the main objective otherwise.
    *   It is the value used for sorting and selecting individuals.
    * @param data
    *   user data associated to this individual
    * @param mainObjOpt
    *   the value of the main objective, when it differs from `objForSelection`, i.e. when the
    *   individual was built by optimizing an alternate objective
    */
  private case class Individual(
    solution: StoreIndependentSolution,
    objForSelection: Long,
    data: D,
    mainObjOpt: Option[Long] = None
  ) {

    /** Returns the main objective value, which is the one used for anytime-best tracking and for
      * the final comparison against the initial solution.
      */
    def obj: Long = mainObjOpt.getOrElse(objForSelection)
  }

  override def declareRemotelyCallableTasks(searchConnector: SearchConnector): Unit = {
    this.searchConnector = searchConnector
    neighborhoodTaskClasses = neighborhoods.map(n =>
      searchConnector.declareRemotelyCallableTask(RemotelySearchableNeighborhood(n))
    )
  }

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {

    val supervisor = searchConnector.supervisor

    // Save the initial solution as the starting population
    val startSolDetached = searchConnector.saveDetachedSolution
    val startObj: Long   = objective.objValue.value()
    val objDetached      = searchConnector.detachObjectiveFromStore(objective)

    // Used to block the main thread and wait for the result of the search
    val searchResultPromise = Promise[Try[SearchResult]]()

    if (verbosityLevel >= 1) {
      println(s"$name: start (obj: $startObj)")
    }

    // Metadata for tracking which task corresponds to which parent and child data
    case class TaskMeta(parentIdx: Int, childData: D)

    // --- Actor state machine ---

    /** State 1: Wait for a StatusReport to know how many workers are available. */
    def waitForStatus(): Behavior[MessageToSearch] = {
      Behaviors.receive { (context: ActorContext[MessageToSearch], command: MessageToSearch) =>
        command match {
          case StatusReport(_, _, _) =>
            val initialPopulation = List(Individual(startSolDetached, startObj, initData()))
            runIteration(0, initialPopulation, None, context)
          case Crash(error) =>
            searchResultPromise.complete(Success(Failure(error)))
            Behaviors.stopped
          case _ =>
            Behaviors.same
        }
      }
    }

    /** State 2: Start an iteration. Calls the step function to decide whether to continue. */
    def runIteration(
      it: Int,
      population: List[Individual],
      anyTimeBest: Option[Individual],
      context: ActorContext[MessageToSearch]
    ): Behavior[MessageToSearch] = {
      if (it >= maxIt || population.isEmpty) {
        finish(population, context)
        return Behaviors.stopped
      }

      step(it, population.map(_.data)) match {
        case None =>
          if (verbosityLevel >= 2) {
            println(s"$name(it: $it): step returned None, stopping")
          }
          finish(population, context)
          Behaviors.stopped

        case Some((genChildren, nbToKeep, alternateObjectiveOpt)) =>
          if (verbosityLevel >= 1) {
            println(
              s"$name(it: $it, popSize: ${population.size}, " +
                s"obj: [${population.map(_.obj).mkString(", ")}])"
            )
          }

          // Resolve here, once and for all, which objective the workers must optimize: the
          // alternate one when the step function provided one, the main one otherwise. Workers
          // are given that single objective, never a choice between two.
          val searchObjDetached: StoreIndependentObjective = alternateObjectiveOpt match {
            case None      => objDetached
            case Some(alt) => searchConnector.detachObjectiveFromStore(alt)
          }
          // When the workers optimize the alternate objective, they are also asked to measure the
          // main objective on the solution they reach, so that anytime-best tracking and the final
          // comparison can keep using the main objective.
          val mainObjToMeasure: Option[StoreIndependentObjective] =
            if (alternateObjectiveOpt.isDefined) Some(objDetached) else None

          // For each individual, call genChildren to determine which tasks to dispatch.
          // The Solution is attached to the supervisor store so the user can inspect it
          // (e.g. restoreSolution and read variables). Neighborhoods returned by the user
          // are mapped back to indices via reference equality on the pre-registered array.
          var taskSpecs: List[(Int, Int, D)] = Nil // (parentIdx, nhIndex, childData)
          var parentKeepOld: Map[Int, Boolean] = Map.empty

          population.zipWithIndex.foreach { case (individual, parentIdx) =>
            val attachedSolution = searchConnector.attachSolutionToStore(individual.solution)
            val (keepOld, children) = genChildren(attachedSolution, individual.data)
            parentKeepOld += (parentIdx -> keepOld)
            children.foreach { case (nh, childData) =>
              val nhIndex = neighborhoods.indexWhere(_ eq nh)
              require(
                nhIndex >= 0,
                s"$name: returned neighborhood is not part of the pre-registered neighborhoods " +
                  s"array. genChildren must return neighborhoods from the `neighborhoods` " +
                  s"argument passed to DistributedPopulationBased."
              )
              taskSpecs ::= ((parentIdx, nhIndex, childData))
            }
          }
          taskSpecs = taskSpecs.reverse

          if (taskSpecs.isEmpty) {
            // No tasks to dispatch — handle parents with no children
            val keptParents = population.zipWithIndex.flatMap { case (parent, idx) =>
              val keepOld = parentKeepOld.getOrElse(idx, false)
              if (keepOld || !dropIfNoMoveFound) List(parent)
              else Nil
            }
            processResults(it, keptParents, nbToKeep, alternateObjectiveOpt, anyTimeBest, context)
          } else {
            supervisor ! GetNewUniqueTaskIds(context.self, taskSpecs.size, context.self)
            waitForTaskIds(
              it,
              population,
              taskSpecs,
              parentKeepOld,
              nbToKeep,
              searchObjDetached,
              mainObjToMeasure,
              alternateObjectiveOpt,
              anyTimeBest
            )
          }
      }
    }

    /** State 3: Wait for task IDs, then create and dispatch all tasks. */
    def waitForTaskIds(
      it: Int,
      population: List[Individual],
      taskSpecs: List[(Int, Int, D)], // (parentIdx, nhIndex, childData)
      parentKeepOld: Map[Int, Boolean],
      nbToKeep: Int,
      searchObjDetached: StoreIndependentObjective,
      mainObjToMeasure: Option[StoreIndependentObjective],
      alternateObjectiveOpt: Option[Objective],
      anyTimeBest: Option[Individual]
    ): Behavior[MessageToSearch] = {
      Behaviors.receive { (context: ActorContext[MessageToSearch], command: MessageToSearch) =>
        command match {
          case NewUniqueTaskIds(firstTaskId, _) =>
            var taskId = firstTaskId
            val taskMetaBuilder = Map.newBuilder[Long, TaskMeta]
            val tasksAndAnswerTo = taskSpecs.map { case (parentIdx, nhIndex, childData) =>
              val task = Task(
                taskId,
                neighborhoodTaskClasses(nhIndex),
                DoAllMoves(
                  population(parentIdx).solution,
                  searchObjDetached,
                  None,
                  mainObjToMeasure
                )
              )
              taskMetaBuilder += (taskId -> TaskMeta(parentIdx, childData))
              taskId += 1
              (task, context.self)
            }

            supervisor ! CreateTasks(context.self, tasksAndAnswerTo)
            val taskMetas = taskMetaBuilder.result()

            waitForResults(
              it,
              population,
              taskMetas,
              parentKeepOld,
              remaining = tasksAndAnswerTo.size,
              results = Nil,
              nbToKeep,
              alternateObjectiveOpt,
              anyTimeBest
            )

          case StatusReport(_, _, _) =>
            Behaviors.same
          case Crash(error) =>
            searchResultPromise.complete(Success(Failure(error)))
            Behaviors.stopped
          case _ =>
            Behaviors.same
        }
      }
    }

    /** State 4: Wait for all task results, then assemble children per parent. */
    def waitForResults(
      it: Int,
      population: List[Individual],
      taskMetas: Map[Long, TaskMeta],
      parentKeepOld: Map[Int, Boolean],
      remaining: Int,
      results: List[(Int, D, Option[(StoreIndependentSolution, Long, Option[Long])])],
      nbToKeep: Int,
      alternateObjectiveOpt: Option[Objective],
      anyTimeBest: Option[Individual]
    ): Behavior[MessageToSearch] = {
      if (remaining == 0) {
        Behaviors.setup { context =>
          supervisor ! CancelAllMyRemainingTasks(context.self, searchFinished = false)

          // Group results by parent and assemble new individuals
          val resultsByParent = results.groupBy(_._1)
          val newIndividuals = population.indices.toList.flatMap { parentIdx =>
            val parent = population(parentIdx)
            val keepOld = parentKeepOld.getOrElse(parentIdx, false)
            val parentResults = resultsByParent.getOrElse(parentIdx, Nil)

            val children = parentResults.flatMap {
              case (_, childData, Some((sol, searchObj, mainObjOpt))) =>
                Some(Individual(sol, searchObj, childData, mainObjOpt))
              case _ => None
            }

            if (children.isEmpty && !dropIfNoMoveFound) {
              List(parent)
            } else if (keepOld) {
              parent :: children
            } else {
              children
            }
          }

          processResults(
            it,
            newIndividuals,
            nbToKeep,
            alternateObjectiveOpt,
            anyTimeBest,
            context
          )
        }
      } else {
        Behaviors.receive { (context: ActorContext[MessageToSearch], command: MessageToSearch) =>
          command match {
            case Crash(error) =>
              searchResultPromise.complete(Success(Failure(error)))
              Behaviors.stopped

            case ResultObtained(TaskResult(taskId, _, _, result)) =>
              val meta = taskMetas(taskId)
              val resultOpt: Option[(StoreIndependentSolution, Long, Option[Long])] = result match {
                case TaskResultNoMoveFound => None
                case TaskResultMove(move) =>
                  move match {
                    case slm: StoreIndependentLoadSolutionMove =>
                      Some((slm.solution, slm.objAfter, slm.measuredObjAfter))
                    case other =>
                      throw new Error(
                        s"$name: unexpected move type from worker: ${other.getClass.getSimpleName}"
                      )
                  }
                case Aborted => None
              }

              val newResult = (meta.parentIdx, meta.childData, resultOpt)
              waitForResults(
                it,
                population,
                taskMetas,
                parentKeepOld,
                remaining - 1,
                newResult :: results,
                nbToKeep,
                alternateObjectiveOpt,
                anyTimeBest
              )

            case StatusReport(_, _, _) =>
              Behaviors.same
            case _ =>
              Behaviors.same
          }
        }
      }
    }

    /** Process collected results: filter redundant, select best, apply anytime best.
      *
      * @param alternateObjectiveOpt
      *   when present, used for sorting/selection instead of the main objective. The main objective
      *   is still used for anytime-best tracking and final result comparison.
      */
    def processResults(
      it: Int,
      allSolutions: List[Individual],
      nbToKeep: Int,
      alternateObjectiveOpt: Option[Objective],
      anyTimeBestOpt: Option[Individual],
      context: ActorContext[MessageToSearch]
    ): Behavior[MessageToSearch] = {

      if (verbosityLevel >= 2) {
        println(s"$name(it: $it): ${allSolutions.size} solutions after diversification")
      }

      // Sort by objective value (best first).
      // When an alternate objective is provided, sort by the alternate objective value
      // using the alternate objective's comparison function (which determines min vs max).
      val selectionObjective = alternateObjectiveOpt.getOrElse(objective)
      val sorted = allSolutions.sortWith { (a, b) =>
        selectionObjective.isValueNewBest(b.objForSelection, a.objForSelection)
      }

      // Filter redundant elements if requested
      var selected: List[Individual] =
        if (filterRedundantElements) {
          filterRedundantAndSelect(sorted, nbToKeep, selectionObjective)
        } else {
          sorted.take(nbToKeep)
        }

      // Update and apply anytime best.
      // Always uses the MAIN objective (not the alternate) for anytime-best tracking.
      val updatedAnyTimeBest =
        if (saveAnytimeBest) {
          val currentBest = selected.headOption
          val newAnyTimeBest = (anyTimeBestOpt, currentBest) match {
            case (None, Some(best))        => Some(best)
            case (Some(saved), Some(best)) =>
              if (objective.isValueNewBest(saved.obj, best.obj)) Some(best) else Some(saved)
            case (saved, None) => saved
            case _             => None
          }

          // Ensure anytime best is in the population
          newAnyTimeBest.foreach { atb =>
            if (selected.isEmpty) {
              selected = List(atb)
            } else if (objective.isValueNewBest(selected.head.obj, atb.obj)) {
              // Current best is worse than saved; reintroduce
              selected = atb :: selected
              if (verbosityLevel >= 2) {
                println(s"$name: restoring anyTimeBest (obj: ${atb.obj})")
              }
            }
          }
          newAnyTimeBest
        } else {
          None
        }

      if (verbosityLevel >= 2) {
        println(
          s"$name(it: $it): selected ${selected.size} best: [${selected.map(_.obj).mkString(", ")}]"
        )
      }

      // Next iteration
      runIteration(it + 1, selected, updatedAnyTimeBest, context)
    }

    /** Filters redundant (identical) solutions and selects the best.
      *
      * @param selectionObjective
      *   the objective used for comparison (main or alternate)
      */
    def filterRedundantAndSelect(
      sorted: List[Individual],
      nbToKeep: Int,
      selectionObjective: Objective
    ): List[Individual] = {
      sorted match {
        case Nil      => Nil
        case h :: Nil => List(h)
        case h :: tail =>
          var toReturn: List[Individual] = List(h)
          var current                    = h
          var remaining                  = nbToKeep - 1
          var nbFiltered                 = 0
          for (next <- tail if remaining > 0) {
            if (next.solution != current.solution) {
              toReturn ::= next
              current = next
              remaining -= 1
            } else {
              nbFiltered += 1
            }
          }
          if (verbosityLevel >= 2 && nbFiltered > 0) {
            println(s"$name: filtered $nbFiltered redundant solutions")
          }
          toReturn.reverse
      }
    }

    /** Finish the population loop: complete the promise with the best solution found. */
    def finish(
      population: List[Individual],
      context: ActorContext[MessageToSearch]
    ): Unit = {
      supervisor ! CancelAllMyRemainingTasks(context.self, searchFinished = true)

      population.headOption match {
        case None =>
          if (verbosityLevel >= 1) {
            println(s"$name: NoMoveFound (empty population)")
          }
          searchResultPromise.complete(Success(Success(NoMoveFound)))

        case Some(bestIndividual) =>
          if (verbosityLevel >= 1) {
            println(
              s"$name: finished (best obj: ${bestIndividual.obj}, start obj: $startObj)"
            )
          }

          // Check if we improved over the start (always uses the main objective)
          if (!objective.isValueNewBest(startObj, bestIndividual.obj)) {
            if (verbosityLevel >= 2) {
              println(s"$name: best solution is not better than initial, returning NoMoveFound")
            }
            searchResultPromise.complete(Success(Success(NoMoveFound)))
          } else {
            val attachedSolution =
              searchConnector.attachSolutionToStore(bestIndividual.solution)
            val move = LoadSolutionMove(attachedSolution, bestIndividual.obj, name)
            searchResultPromise.complete(Success(Success(MoveFound(move))))
          }
      }
    }

    // --- Start the actor ---
    supervisor ! SpawnSearchActorAndStatusRequest(Behaviors.setup[MessageToSearch](_ => {
      waitForStatus()
    }))

    // --- Block main thread until the population loop finishes ---
    Await.result(searchResultPromise.future, Duration.Inf) match {
      case Failure(exception) =>
        val e = new Exception(exception.toString)
        e.setStackTrace((exception.getStackTrace.toList ::: e.getStackTrace.toList).toArray)
        throw e
      case Success(value) => value
    }
  }
}
