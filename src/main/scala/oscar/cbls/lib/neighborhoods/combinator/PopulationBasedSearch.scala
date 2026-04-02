package oscar.cbls.lib.neighborhoods.combinator

import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.computation.{Solution, Store}
import oscar.cbls.core.search._
import oscar.cbls.util.Tabulator

object PopulationBasedSearch {

  /** This combinator implements a population-based meta-heuristics that maintains a population of
    * solutions and repeatedly (1) diversifies and improves the solutions by applying a set of
    * neighborhoods on each of them (2) selects the best solutions in this population. Upon
    * termination, the best solution in this population is returned, only if it improves over the
    * initial solution.
    *
    * This if a form of restart meta-heuristics that allows devoting little time to low quality
    * solutions while maintaining some diversity throughout the search.
    *
    * This combinator is a long-lasting combinator; ie: it will perform and manage it business and
    * return a loadSolutionMove. It cannot be used on the right hand side of a dynAndThen for
    * instance.
    *
    * This meta-heuristics is described in:
    *
    * [[https://doi.org/10.1016/j.cor.2020.105166 Florian Arnold and Kenneth Sörensen, A progressive filtering heuristic for the location-routing problem and variants, Computers & Operations Research, vol 129, 2021,]]
    *
    * The difference with this reference paper is that this implementation does not feature a
    * crossover operator that would combine two solutions into a new one.
    *
    * Since this combinator is long-lasting, it will perform many local searches in a row and has
    * different verbose modes than the general verbosity defined for combinators. The search
    * procedure used by this meta heuristics are allocated a lower verbosity mode. The verbosity are
    * as follows:
    *   - verbose = 1 will show the evolution of the population and the base search procedure has
    *     verbose = 0
    *   - verbose = 2 will show mode insight on the algorithm: how many identical individuals were
    *     filtered and the base search procedure has verbose = 0
    *   - verbose = 3 also shows the start and end information the search procedure used in the
    *     search. and the base search procedure has verbose = 1
    *
    * @tparam D
    *   the type of the data associated to each individual
    *
    * @param initData
    *   generates data for the initial individual
    * @param step
    *   the definition of the diversification and filtering operations to apply on the population.
    *   <p>
    *
    * It receives:
    *   - the iteration number,
    *   - the data associated to each individual.
    *
    * It returns optionally :
    *   - a function used to generate children of an individual. This function receives the
    *     [[oscar.cbls.core.computation.Solution]] and the data associated to the individual. It
    *     returns whether the individual must be keep in the next generation, a list of
    *     [[Neighborhood]] used to generate children the data associated to each child ;
    *   - how many individual must be kept in the next generation ;
    *   - an optional [[Objective]] that can be used to generate and select children for the wole
    *     generation.
    *
    * If it returns None, the meta-heuristics stops and the best current solution is returned if
    * accepted by the objective.
    * @param store
    *   the store, used to save solutions
    * @param maxIt
    *   The maximal number of iterations. This is somehow redundant with the previous parameter, but
    *   if you need it, it is there.
    * @param saveAnytimeBest
    *   - If set to true, the meta-heuristics will permanently ensure that the best solution
    *     encountered during the meta-heuristics is saved and reloaded into the population. This can
    *     happen if the neighborhood performs a restart or any form of randomization. This parameter
    *     only makes sense if keepOld is set to false.
    *   - If set to false, this additional mechanism is deactivated.
    * @param filterRedundantElements
    *   - If set to true, the meta-heuristics will identify and filter identical solutions in the
    *     population within each iteration.
    *   - If set to false, this additional mechanism is deactivated. It can then spare time. Logs
    *     are provided in the console about the number of elements in the population that are
    *     filtered out by this mechanism.
    * @param dropIfNoMoveFound
    *   - If set to true, a solution will be dropped if no move is found by a neighborhood.
    *   - If set to false, a solution is kept in the population even if no move is found by the
    *     neighborhood.
    * @param name
    *   a name for this combinator that will be used in logs
    */
  def apply[D](
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
    name: String = "PopulationBasedSearch"
  ): PopulationBasedSearch[D] =
    new PopulationBasedSearch(
      initData,
      step,
      store,
      maxIt,
      saveAnytimeBest,
      filterRedundantElements,
      dropIfNoMoveFound,
      name
    )
}

/** This combinator implements a population-based meta-heuristics that maintains a population of
  * solutions and repeatedly (1) diversifies and improves the solutions by applying a set of
  * neighborhoods on each of them (2) selects the best solutions in this population. Upon
  * termination, the best solution in this population is returned, only if it improves over the
  * initial solution.
  *
  * This if a form of restart meta-heuristics that allows devoting little time to low quality
  * solutions while maintaining some diversity throughout the search.
  *
  * This combinator is a long-lasting combinator; ie: it will perform and manage it business and
  * return a loadSolutionMove. It cannot be used on the right hand side of a dynAndThen for
  * instance.
  *
  * This meta-heuristics is described in:
  *
  * [[https://doi.org/10.1016/j.cor.2020.105166 Florian Arnold and Kenneth Sörensen, A progressive filtering heuristic for the location-routing problem and variants, Computers & Operations Research, vol 129, 2021,]]
  *
  * The difference with this reference paper is that this implementation does not feature a
  * crossover operator that would combine two solutions into a new one.
  *
  * Since this combinator is long-lasting, it will perform many local searches in a row and has
  * different verbose modes than the general verbosity defined for combinators. The search procedure
  * used by this meta heuristics are allocated a lower verbosity mode. The verbosity are as follows:
  *   - verbose = 1 will show the evolution of the population and the base search procedure has
  *     verbose = 0
  *   - verbose = 2 will show mode insight on the algorithm: how many identical individuals were
  *     filtered and the base search procedure has verbose = 0
  *   - verbose = 3 also shows the start and end information the search procedure used in the
  *     search. and the base search procedure has verbose = 1
  *
  * @tparam D
  *   the type of the data associated to each individual
  *
  * @param initData
  *   generates data for the initial individual
  * @param step
  *   the definition of the diversification and filtering operations to apply on the population. <p>
  *
  * It receives:
  *   - the iteration number,
  *   - the data associated to each individual.
  *
  * It returns optionally :
  *   - a function used to generate children of an individual. This function receives the
  *     [[oscar.cbls.core.computation.Solution]] and the data associated to the individual. It
  *     returns whether the individual must be keep in the next generation, a list of
  *     [[Neighborhood]] used to generate children the data associated to each child ;
  *   - how many individual must be kept in the next generation ;
  *     - an optional [[Objective]] that can be used to generate and select children.
  *
  * If it returns None, the meta-heuristics stops and the best current solution is returned if
  * accepted by the objective.
  * @param store
  *   the store, used to save solutions
  * @param maxIt
  *   The maximal number of iterations. This is somehow redundant with the previous parameter, but
  *   if you need it, it is there.
  * @param saveAnytimeBest
  *   - If set to true, the meta-heuristics will permanently ensure that the best solution
  *     encountered during the meta-heuristics is saved and reloaded into the population. This can
  *     happen if the neighborhood performs a restart or any form of randomization. This parameter
  *     only makes sense if keepOld is set to false.
  *   - If set to false, this additional mechanism is deactivated.
  * @param filterRedundantElements
  *   - If set to true, the meta-heuristics will identify and filter identical solutions in the
  *     population within each iteration.
  *   - If set to false, this additional mechanism is deactivated. It can then spare time. Logs are
  *     provided in the console about the number of elements in the population that are filtered out
  *     by this mechanism.
  * @param dropIfNoMoveFound
  *   - If set to true, a solution will be dropped if no move is found by a neighborhood.
  *   - If set to false, a solution is kept in the population even if no move is found by the
  *     neighborhood.
  * @param name
  *   a name for this combinator that will be used in logs
  */
class PopulationBasedSearch[D](
  initData: () => D,
  step: (
    Int,
    List[D]
  ) => Option[((Solution, D) => (Boolean, List[(Neighborhood, D)]), Int, Option[Objective])],
  store: Store,
  maxIt: Int = Int.MaxValue,
  saveAnytimeBest: Boolean,
  filterRedundantElements: Boolean,
  dropIfNoMoveFound: Boolean,
  name: String
) extends NeighborhoodCombinator(name, List.empty) {

  /** Stores an individual in the population-based algo
    * @param solution
    *   the solution
    * @param obj
    *   its objective value
    * @param ancestor
    *   the ancestor, for log and understanding of the meta-heuristics efficiency. Is not considered
    *   as part of the identity of the case class, so not in the compare method
    * @param data
    *   some useful data associated to this individual
    * @param isOldGen
    *   whether this individual is inherited from previous generation
    */
  private case class Individual(
    solution: Solution,
    obj: Long,
    ancestor: Option[Individual],
    data: D,
    isOldGen: Boolean = false
  ) extends Ordered[Individual] {

    var rank: Int       = -1
    var generation: Int = -1

    /** An optional objective value that can be used for selection. */
    var selectionObj: Option[Long] = None

    /** Returns the objective value to use for the selection. */
    def objForSelection: Long = selectionObj.getOrElse(obj)

    override def compare(that: Individual): Int = {
      // ancestorObj is only for logging and cannot be taken into account in this comparison.
      if (this.obj > that.obj) 1
      else if (this.obj < that.obj) -1
      else this.solution.compare(that.solution)
    }

    override def toString: String = {
      var str = s"Individual: $solution - obj: $obj"
      selectionObj.foreach(o => str += s" - selectObj: $o")
      str
    }
  }

  /** Stores internal statistics of this metaheuristics for display in verbose level 2.
    * @param it
    *   the iteration nr
    * @param populationSizeBefore
    *   the population size at the start of the iteration
    * @param avgNbOfNeighborhoods
    *   the average number of explored neighborhoods for a generation
    * @param populationSizeAfter
    *   the population size after exploring the neighborhoods (incorporating the keepOld if
    *   activated)
    * @param maxSize
    *   the number of individual to keep at this iteration
    * @param avgRunTime
    *   the avg run time of neighborhood exploration in milliseconds
    */
  private case class IterationStats(
    it: Int,
    populationSizeBefore: Int = 0,
    avgNbOfNeighborhoods: Int = 0,
    populationSizeAfter: Int = 0,
    maxSize: Int = 0,
    avgRunTime: Long = 0
  )

  /** Explores this Combinator. This is where you put the logic of your Combinator.
    *
    * @param objective
    *   The Objective of the search
    * @return
    *   The search result, either [[oscar.cbls.core.search.MoveFound]] or
    *   [[oscar.cbls.core.search.NoMoveFound]]
    */
  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {

    if (verbosityLevel >= 2) {
      println("PopulationBasedSearch: start")
    }

    val startSol: Solution      = store.extractSolution()
    val startObj: Long          = objective.objValue.value()
    var it: Int                 = 0
    var anyTimeBest: Individual = Individual(startSol, startObj, None, initData())
    anyTimeBest.generation = 0
    anyTimeBest.rank = 0
    var currentSolutions: List[Individual] = List(anyTimeBest)
    var stop: Boolean                      = false

    if (verbosityLevel >= 2) {
      println("PopulationBasedSearch(start obj: " + anyTimeBest.obj + ")")
    }

    var allStats: List[IterationStats] = Nil

    while (currentSolutions.nonEmpty && it < maxIt && !stop) {
      step(it, currentSolutions.map(_.data)) match {
        case None =>
          if (verbosityLevel >= 2) {
            println("\nPopulationBasedSearch(it: " + it + "): noMoreNeighborhoods")
          }

          stop = true
        case Some(
              (
                genChildren: ((Solution, D) => (Boolean, List[(Neighborhood, D)])),
                nbSolutionsToKeep: Int,
                generationAndSelectionObjective: Option[Objective]
              )
            ) =>
          if (verbosityLevel >= 1) {
            println(
              "PopulationBasedSearch(it " + it + " populationSize:" + currentSolutions.size + " obj:["
                + currentSolutions
                  .map(i => "" + i.obj + "(ex: " + i.ancestor.map(_.obj).getOrElse(startObj) + ")")
                  .mkString(", ") + "])"
            )
          }

          var currentStats = IterationStats(
            it,
            populationSizeBefore = currentSolutions.size,
            maxSize = nbSolutionsToKeep
          )

          val newSolutions: List[Individual] = {

            val startTime = System.currentTimeMillis()
            val children: List[Individual] = currentSolutions.flatMap(individual =>
              diversifyAndImprove(
                genChildren,
                individual,
                mainObjective = objective,
                generationObjective = generationAndSelectionObjective,
                dropIfNoMoveFound = dropIfNoMoveFound
              )
            )
            val endTime = System.currentTimeMillis()
            currentStats = currentStats.copy(avgRunTime = (endTime - startTime) / children.size)

            children.foreach(ind => if (ind.generation == -1) ind.generation = it + 1)

            children
          }

          currentStats = currentStats.copy(
            populationSizeAfter = newSolutions.size,
            avgNbOfNeighborhoods = newSolutions.size / currentSolutions.size
          )

          allStats = currentStats :: allStats
          if (verbosityLevel >= 2) {
            println("newSolutions after diversification and improvement:" + newSolutions.size)
          }

          currentSolutions = {
            if (filterRedundantElements) {
              filterRedundantAndSort(
                newSolutions,
                nbSolutionsToKeep,
                mainObjective = objective,
                selectionObjective = generationAndSelectionObjective
              )
            } else {
              val filteredCurrentSolutions = newSolutions
                .sortWith { case (individual1, individual2) =>
                  generationAndSelectionObjective
                    .getOrElse(objective)
                    .isValueNewBest(individual2.objForSelection, individual1.objForSelection)
                }
                .take(nbSolutionsToKeep)

              if (verbosityLevel >= 2) {
                println(
                  "PopulationBasedSearch(selected " + filteredCurrentSolutions.size + " bests out of " + newSolutions + " solutions)"
                )
              }
              filteredCurrentSolutions
            }
          }

          currentSolutions.zipWithIndex.foreach { case (individual, rank) =>
            if (individual.rank == -1) individual.rank = rank
          }

          if (saveAnytimeBest) {
            // anyTimeBest is the best for the global objective. Here we do not use the optional objective
            if (currentSolutions.isEmpty) {
              // reload anytimeBest because no solution at all
              currentSolutions ::= anyTimeBest
            } else if (objective.isValueNewBest(anyTimeBest.obj, currentSolutions.head.obj)) {
              // update the anytimeBest
              anyTimeBest = currentSolutions.head

              if (verbosityLevel >= 2) {
                println("PopulationBasedSearch: update the anyTimeBest")
              }
            } else {
              // reload anytimeBest
              currentSolutions ::= anyTimeBest
              if (verbosityLevel >= 2) {
                println("PopulationBasedSearch: restoring the anyTimeBest")
              }
            }
          }

      }
      it += 1
    }

    if (verbosityLevel >= 1) {
      println(s"Final population populationSize: ${currentSolutions.size}  obj: [${currentSolutions
          .map(i => "" + i.obj + "(ex: " + i.ancestor.map(_.obj).getOrElse(startObj) + ")")
          .mkString(", ")}]")
    }

    commitMove(objective, LoadSolutionMove(startSol, startObj, "Reload initial solution."))

    currentSolutions.headOption match {
      case None =>
        if (verbosityLevel >= 2) {
          println("PopulationBasedSearch: NoMoveFound")
        }
        NoMoveFound

      case Some(bestIndividual) =>
        if (verbosityLevel >= 2) {
          // searching for history of bestSol
          var currentOpt: Option[Individual]       = Some(bestIndividual)
          var rankHistory: List[(Int, Individual)] = Nil

          while (currentOpt.isDefined) {
            val current = currentOpt.get
            rankHistory ::= (current.generation, current)
            currentOpt = current.ancestor
          }

          val historyMap = rankHistory.toMap
          val maxRank    = rankHistory.map(_._2.rank).max

          println(rankHistory.map { case (gen, ind) => s"$gen -> ${ind.obj}" }.mkString(", "))

          val title =
            "PopulationBasedSearch: genealogy and stats " + (if (saveAnytimeBest)
                                                               s"(generation skipping possible: saveAnytimeBest= $saveAnytimeBest)"
                                                             else "")

          val table = Tabulator.format(
            List(
              "it",
              "rankOfAncestorOfFinalBest",
              "populationSizeBefore",
              "avgNbNeighborhoods",
              "populationSizeAfter",
              "maxSize",
              "avgRunTimeMs"
            ) :: allStats.reverse
              .map(stat =>
                List(
                  "" + stat.it,
                  "" + historyMap
                    .get(stat.it)
                    .map(individual =>
                      "" + individual.rank + (if (individual.isOldGen) " (oldGen)" else "")
                    )
                    .getOrElse("ancestor is anyTimeBest"),
                  "" + stat.populationSizeBefore,
                  "" + stat.avgNbOfNeighborhoods,
                  "" + stat.populationSizeAfter,
                  "" + stat.maxSize,
                  "" + stat.avgRunTime
                )
              )
          )

          println(title + "\n" + table)
          println("maxRank of Ancestor of finalBest: " + maxRank)
        }

        if (!objective.isValueNewBest(startObj, bestIndividual.obj)) {
          if (verbosityLevel >= 2) {
            println("PopulationBasedSearch: Best sol after search is worse than initial one")
          }
          NoMoveFound
        } else {
          if (verbosityLevel >= 2) {
            println("PopulationBasedSearch: evaluating final move on obj " + bestIndividual.obj)
          }

          val toReturn = MoveFound(
            LoadSolutionMove(bestIndividual.solution, bestIndividual.obj, name)
          )
          if (verbosityLevel >= 2) {
            println("PopulationBasedSearch: " + toReturn)
          }
          toReturn
        }
    }
  }

  /** Diversifies the individual by applying each neighborhood to it. Every neighborhood is applied
    * from the individual and gives rise to a descendant.
    * @param genChildren
    *   function used to generates all the children and their associated data
    * @param parent
    *   the individual to start with
    * @param mainObjective
    *   the main objective function to use when applying the neighborhoods
    * @param generationObjective
    *   an optional objective that can be used to generate the next generation instead of the main
    *   objective
    * @param dropIfNoMoveFound
    *   if no move found, should we drop the start solution or not
    * @return
    *   a list of new individuals
    */
  private def diversifyAndImprove(
    genChildren: (Solution, D) => (Boolean, List[(Neighborhood, D)]),
    parent: Individual,
    mainObjective: Objective,
    generationObjective: Option[Objective],
    dropIfNoMoveFound: Boolean
  ): Iterable[Individual] = {

    val (keepOld, children): (Boolean, List[(Neighborhood, D)]) =
      genChildren(parent.solution, parent.data)

    val toReturn: List[Individual] = children.flatMap {
      case (n, nextData) =>
        commitMove(
          mainObjective,
          LoadSolutionMove(parent.solution, parent.obj, "Load current individual.")
        )

        generationObjective.foreach(_.synchronizeObjectiveValue())

        n.reset()
        n.verbosityLevel = childVerbosity(verbosityLevel)
        val nbMoves = n.doAllMoves(generationObjective.getOrElse(mainObjective))

        if (nbMoves == 0) {
          None
        } else {
          val child = Individual(
            store.extractSolution(),
            mainObjective.objValue.value(),
            Some(parent),
            nextData
          )

          generationObjective.foreach(o => {
            child.selectionObj = Some(o.objValue.value())
            mainObjective.synchronizeObjectiveValue()
          })

          Some(child)
        }

      case _ => None
    }

    if (toReturn.isEmpty && !dropIfNoMoveFound) {
      Some(parent)
    } else if (keepOld) {
      val old = Individual(parent.solution, parent.obj, Some(parent), parent.data, isOldGen = true)
      old :: toReturn
    } else toReturn
  }

  /** Filters out redundant individuals and selects the best ones
    * @param solutions
    *   a list of individuals with potentially redundant individuals (ie: same solutions)
    * @param nbSolutionsToKeep
    *   the number of solutions to keep
    * @param mainObjective
    *   the main objective function to use when applying the neighborhoods
    * @param selectionObjective
    *   an optional objective that can be used to select individuals instead of the main objective
    * @return
    *   a sublist of solutions where there are no identical element and that includes the
    *   nbSolutionsToKeep best solutions
    */
  private def filterRedundantAndSort(
    solutions: List[Individual],
    nbSolutionsToKeep: Int,
    mainObjective: Objective,
    selectionObjective: Option[Objective]
  ): List[Individual] = {

    val sortedSolutionsByIncreasingObj: List[Individual] = solutions
      .sortWith { case (individual1, individual2) =>
        selectionObjective
          .getOrElse(mainObjective)
          .isValueNewBest(individual2.objForSelection, individual1.objForSelection)
      }

    if (verbosityLevel >= 3) {
      println(
        "PopulationBasedSearch: sortedSolutionsBestFirst: " + sortedSolutionsByIncreasingObj
          .map(_.obj)
          .mkString(", ")
      )
    }
    var toReturn: List[Individual] = Nil

    sortedSolutionsByIncreasingObj match {
      case Nil => Nil
      case h :: Nil =>
        if (verbosityLevel >= 3) {
          println("PopulationBasedSearch: no filtering performed because only one solution")
        }
        List(h)
      case h :: tail =>
        var current    = h
        var nbFiltered = 0
        toReturn ::= current
        var nbSolutionsToKeepNow = nbSolutionsToKeep - 1
        for (next <- tail if nbSolutionsToKeepNow > 0) {
          if (next.compare(current) != 0) {
            toReturn ::= next
            current = next
            nbSolutionsToKeepNow -= 1
          } else {
            nbFiltered += 1
          }
        }
        toReturn = toReturn.reverse
        if (verbosityLevel >= 3) {
          println(
            "PopulationBasedSearch: removed " + nbFiltered + " identical solutions and selected "
              + toReturn.size + " best ones: [" + toReturn.map(i => i.obj).mkString(", ") + "]"
          )
        }
        toReturn
    }
  }

  private def childVerbosity(verbosity: Int): Int = {
    verbosity match {
      case 0 => 0
      case 1 => 0
      case 2 => 0
      case 3 => 1
      case 4 => 2
    }
  }
}
