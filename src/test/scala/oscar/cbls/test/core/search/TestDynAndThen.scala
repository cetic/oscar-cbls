package oscar.cbls.test.core.search

import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.{Exploration, Minimize, Objective}
import oscar.cbls.core.search.profiling.{
  CompositionProfiler,
  NeighborhoodProfiler
}
import oscar.cbls.core.search._
import oscar.cbls.lib.invariant.numeric.IntInt2Int

import scala.util.Random

class TestDynAndThen extends AnyFunSuite {

  val random: Random = Random
  val seed: Int      = random.nextInt()
  random.setSeed(seed)

  test(s"DynAndThen works as intended with the new API. Seed : $seed") {
    val store                 = new Store()
    val a: IntVariable        = IntVariable(store, 500)
    val b: IntVariable        = IntVariable(store, 600)
    val objValue: IntVariable = IntVariable(store, 1000000, name = Some("Pythagoras"))
    val objective: Minimize   = Minimize(objValue)

    // Basically Pythagoras theorem
    new IntInt2Int(
      store,
      a,
      b,
      objValue,
      (a, b) => Math.sqrt(Math.pow(a.toDouble, 2) + Math.pow(b.toDouble, 2)).toLong
    )
    store.close()

    val left: TestAssignNeighborhood = new TestAssignNeighborhood(a, random, name = "TA A")
    val right: Move => TestAssignNeighborhood =
      _ => new TestAssignNeighborhood(a, random, name = "TA B")
    val searchProcedure = new DynAndThen(left, right)
    searchProcedure.verbosityLevel = 1
    searchProcedure.profileSearch()
    searchProcedure.doAllMoves(objective)
    searchProcedure.displayProfiling()
  }

  class DynAndThen(left: Neighborhood, right: Move => Neighborhood)
      extends NeighborhoodCombinator("DynAndThen", List(left)) {

    private var _searchProfilerOpt2: Option[CompositionProfiler] = None

    override def searchProfiler(): Option[CompositionProfiler] = _searchProfilerOpt2

    override def profileSearch(): Unit = {
      searchProfiler() match {
        case None =>
          _searchProfilerOpt2 = Some(CompositionProfiler(this, Some(left)))
          left.profileSearch()
        case _ => ;
      }
    }

    /** Explores this Combinator. This is where you put the logic of your Combinator.
      *
      * @param objective
      *   The Objective of the search
      * @return
      *   The search result, either [[oscar.cbls.core.search.MoveFound]] or
      *   [[oscar.cbls.core.search.NoMoveFound]]
      */
    override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
      val acceptFirstEvalComposition = new CompositionObjective(objective, right, this)
      val compositeMove              = left.getMove(acceptFirstEvalComposition)
      searchProfiler().get.mergeDynProfiler()
      compositeMove
    }
  }

  case class CompositeMove(
    rightMove: Move,
    leftMove: Move,
    objValueAfter: Long,
    compositeNeighborhoodName: String
  ) extends Move(objValueAfter, compositeNeighborhoodName) {

    /** Commits this move. */
    override def commit(): Unit = {
      rightMove.commit()
      leftMove.commit()
    }
  }

  class CompositionObjective(baseObj: Objective, right: Move => Neighborhood, dad: DynAndThen)
      extends Objective(baseObj.objValue) {
    // Creates the base objective exploration, used for composition evaluation.
    // (Get the initial obj upon Exploration creation)
    private val baseExploration = baseObj.newExploration[CompositeMove]()

    /** Creates a new Exploration instance. Must be called when starting an exploration. */
    override def newExploration[M1 <: Move](
      neighborhoodProfiler: Option[NeighborhoodProfiler]
    ): Exploration[M1] =
      new Exploration[M1](objValue.value(), neighborhoodProfiler) {

        /** Checks if the candidate objValue match the acceptance conditions */
        override def checkNeighbor(buildMove: Long => M1): Unit = {
          super.checkNeighbor(buildMove)
          val leftExploredMove = buildMove(0)
          // Accept and build move return by right neighborhood
          // Returned move is right neighborhood's move applied to left neighborhood's move
          val rightNeighborhood = right(leftExploredMove)
          rightNeighborhood.profileSearch()
          dad.searchProfiler().get.setCurrentRight(rightNeighborhood.searchProfiler().get)
          _toReturn = rightNeighborhood.getMove(new Objective(objValue) {

            override def newExploration[M2 <: Move](
              neighborhoodProfiler: Option[NeighborhoodProfiler]
            ): Exploration[M2] =
              new Exploration[M2](objValue.value(), neighborhoodProfiler) {

                /** Checks if the candidate objValue match the acceptance conditions */
                override def checkNeighbor(buildMove: Long => M2): Unit = {
                  super.checkNeighbor(buildMove)
                  // checkNeighbor uses baseObjective neighbor to evaluate the composition
                  baseExploration.checkNeighbor(buildMove =
                    newObjValue =>
                      CompositeMove(
                        leftExploredMove,
                        buildMove(newObjValue),
                        newObjValue,
                        "DynAndThen"
                      )
                  )
                  // Needed for Neighborhood's getMove method to return the searchResult
                  _toReturn = baseExploration.toReturn
                }
              }

            /** Returns the worst value that the objective value could have considering the
              * Objective.
              */
            override val worstValue: Long = baseObj.worstValue

            /** Returns true if newValue is a better value than currentBest.
              *
              * Depending on used Objective this information may vary
              *
              * @param currentBest
              *   The current best value (has to be given by the caller)
              * @param newValue
              *   The considered new value
              * @return
              *   True if newValue is better than currentBest
              */
            override def isValueNewBest(currentBest: Long, newValue: Long): Boolean =
              baseObj.isValueNewBest(currentBest, newValue)
          })

        }
      }

    /** Returns the worst value that the objective value could have considering the Objective. */
    override val worstValue: Long = baseObj.worstValue

    /** Returns true if newValue is a better value than currentBest.
      *
      * Depending on used Objective this information may vary
      *
      * @param currentBest
      *   The current best value (has to be given by the caller)
      * @param newValue
      *   The considered new value
      * @return
      *   True if newValue is better than currentBest
      */
    override def isValueNewBest(currentBest: Long, newValue: Long): Boolean =
      baseObj.isValueNewBest(currentBest, newValue)
  }

}
