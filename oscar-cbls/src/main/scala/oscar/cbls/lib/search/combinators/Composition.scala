package oscar.cbls.lib.search.combinators

import oscar.cbls._
import oscar.cbls.core.computation.{AbstractVariable, Solution, Store}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{AcceptanceCriterion, CallBackMove, CompositeMove, DifferentOf, DoNothingMove, DoNothingNeighborhood, LoadSolutionMove, Move, MoveFound, Neighborhood, NeighborhoodCombinator, NoMoveFound, SearchResult, StrictImprovement, SupportForAndThenChaining}

abstract class NeighborhoodCombinatorNoProfile(a: Neighborhood*) extends NeighborhoodCombinator(a:_*) {
  override def collectProfilingStatistics: List[Array[String]] = List.empty
  override def resetStatistics(): Unit ={}
}

object Mu {

  def apply[MoveType <: Move](firstNeighborhood : Neighborhood with SupportForAndThenChaining[MoveType],
                              neighborhoodGenerator : List[MoveType] => Option[Neighborhood with SupportForAndThenChaining[MoveType]],
                              maxDepth : Long,
                              intermediaryStops : Boolean): Neighborhood with SupportForAndThenChaining[CompositeMove] = {
    Mu[MoveType,Any](
      firstNeighborhood,
      (l,_) => neighborhoodGenerator(l) match {
        case None => None
        case Some(n) => Some((n,()))
      },
      (),
      maxDepth,
      intermediaryStops)
  }

  /**
   * @param firstNeighborhood
   * @param neighborhoodGenerator latest moves are closer to the head
   * @param x0
   * @param maxDepth
   * @param intermediaryStops
   * @tparam MoveType
   * @tparam X
   * @return
   */
  def apply[MoveType <: Move, X](firstNeighborhood : Neighborhood with SupportForAndThenChaining[MoveType],
                                 neighborhoodGenerator : (List[MoveType], X) => Option[(Neighborhood with SupportForAndThenChaining[MoveType], X)],
                                 x0 : X,
                                 maxDepth : Long,
                                 intermediaryStops : Boolean): Neighborhood  with SupportForAndThenChaining[CompositeMove] = {
    require(maxDepth >= 1L)

    def generateNextNeighborhood(oldMoves : List[MoveType], remainingDepth : Long, prevX : X)(newMove : MoveType):Neighborhood = {
      if (remainingDepth == 0L) {
        DoNothingNeighborhood()
      } else if (remainingDepth == 1L) {
        neighborhoodGenerator(newMove :: oldMoves, prevX) match {
          case Some((nextAtomicNeighborhood, _)) =>
            if (intermediaryStops) DoNothingNeighborhood() orElse nextAtomicNeighborhood
            else nextAtomicNeighborhood
          case None => DoNothingNeighborhood()
        }
      } else {
        val newMoveList = newMove :: oldMoves
        neighborhoodGenerator(newMove :: oldMoves, prevX) match {
          case Some((nextAtomicNeighborhood, newX)) =>
            val generatorForNext = generateNextNeighborhood(newMoveList, remainingDepth - 1L, newX) _
            if (intermediaryStops) DoNothingNeighborhood() orElse dynAndThen(nextAtomicNeighborhood, generatorForNext)
            else dynAndThen(nextAtomicNeighborhood, generatorForNext)
          case None => DoNothingNeighborhood()
        }
      }
    }
    new ChainableName(
      dynAndThen(
        firstNeighborhood,
        generateNextNeighborhood(List.empty, maxDepth - 1L, x0)
      ),
      "Mu(" + firstNeighborhood + ")"
    )
  }
}

/**
 * to build a composite neighborhood.
 * the first neighborhood is used only to provide a round robin exploration on its possible moves
 * you must ensure that this first neighborhood will perform a hotRestart, so that it will enumerate all its moves
 * internally, this neighborhood will be called with a fully acceptant acceptanceCriteria,
 *
 * the move combinator for every move provided by the first neighborhood, the combinator calls the second one
 * and we consider the composition of the two moves for the acceptance criteria.
 * the returned move is the composition of the two found moves
 *
 * you must also ensure that the two neighborhood evaluate the same objective function,
 * since this combinator needs to evaluate the whole composite move, and not only the last part of the composition
 *
 * A native composite neighborhood will probably be much faster than this combinator, so use this for prototyping
 * for instance, this combinator does not allow for some form of symmetry breaking, unless you are really doing it the hard way.
 *
 * this move will reset the first neighborhood on every call, since it is probably bounded by the number of moves it can provide
 *
 * @param a the first neighborhood, all moves delivered by this one will be considered
 * @param b given that the move returned by the first neighborhood is committed, we explore the globally improving moves of this one
 * @param maximalIntermediaryDegradation the maximal degradation that is admitted for the intermediary step; the higher, the more moves will be considered
 * @author renaud.delandtsheer@cetic.be
 */
case class AndThen[FirstMoveType<:Move](a: Neighborhood with SupportForAndThenChaining[FirstMoveType],
                                        b: Neighborhood,
                                        maximalIntermediaryDegradation: Long = Long.MaxValue)
  extends DynAndThen[FirstMoveType](a, _ => b, maximalIntermediaryDegradation){
}

/**
 * to build a composite neighborhood.
 * the first neighborhood is used only to provide a round robin exploration on its possible moves
 * you must ensure that this first neighborhood will perform a hotRestart, so that it will enumerate all its moves
 * internally, this neighborhood will be called with a fully acceptant acceptanceCriteria,
 *
 * the move combinator for every move provided by the first neighborhood, the combinator calls the second one
 * and we consider the composition of the two moves for the acceptance criteria.
 * the returned move is the composition of the two found moves
 *
 * you must also ensure that the two neighborhood evaluate the same objective function,
 * since this combinator needs to evaluate the whole composite move, and not only the last part of the composition
 *
 * A native composite neighborhood will probably be much faster than this combinator, so use this for prototyping
 * for instance, this combinator does not allow for some form of symmetry breaking, unless you are really doing it the hard way.
 *
 * this move will reset the first neighborhood on every call, since it is probably bounded by the number of moves it can provide
 *
 * @param a the first neighborhood, all moves delivered by this one will be considered
 * @param b given that the move returned by the first neighborhood is committed, we explore the globally improving moves of this one
 *          you pass a method to instantiate b, based on,the currently explored move from a
 * @param maximalIntermediaryDegradation the maximal degradation that is admitted for the intermediary step; the higher, the more moves will be considered
 */
class DynAndThen[FirstMoveType<:Move](a:Neighborhood with SupportForAndThenChaining[FirstMoveType],
                                      b:FirstMoveType => Neighborhood,
                                      maximalIntermediaryDegradation: Long = Long.MaxValue)
  extends NeighborhoodCombinatorNoProfile(a) with SupportForAndThenChaining[CompositeMove]{

  //we need to store currentB here because we might need to instantiate the current move from it.
  var currentB:Neighborhood = null

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {

    var bestObj:Long = Long.MaxValue
    var toReturn:SearchResult = NoMoveFound

    //the acceptance criterion is on the diff between the oldObj and the newObj over the two consecutive moves
    //it is evaluated for the second move
    //the first move is about accepting all moves that are not maxVal, since the newObj is for the consecutive moves,
    // and is already accepted by the time it is returned to the first neighrhood
    val firstAcceptanceCriterion = DifferentOf(Long.MaxValue)

    case class SecondAcceptanceCriterion(initialObj: Long) extends AcceptanceCriterion {
      override def apply(oldValue: Long, newValue: Long): Boolean = {
        (newValue < bestObj) && acceptanceCriteria(initialObj, newValue)
      }
    }

    val secondAcceptanceCriterion = SecondAcceptanceCriterion(initialObj)

    class InstrumentedObjectiveForFirstNeighborhood() extends Objective{

      override def detailedString(short: Boolean, indent: Long = 0L): String = nSpace(indent) + "AndThenInstrumentedObjective(initialObjective:" + obj.detailedString(short) + ")"

      override def model: Store = obj.model

      override def value: Long = {
        val intermediaryObjValue =
          if (maximalIntermediaryDegradation != Long.MaxValue) {
            //we need to ensure that intermediary step is admissible
            val intermediaryVal = obj.value
            val intermediaryDegradation = intermediaryVal - initialObj
            if (intermediaryDegradation > maximalIntermediaryDegradation) {
              //this is a return; the first step is altogheter ignored
              return Long.MaxValue //we do not consider this first step
            } else {
              intermediaryVal
            }
          } else {
            Long.MaxValue
          }

        //now, we need to check the other neighborhood
        //first, let's instantiate it:
        val currentMoveFromA = a.instantiateCurrentMove(intermediaryObjValue)
        currentB = b(currentMoveFromA)
        currentB.verbose = 0 max (a.verbose -1) //passing verbosity to b, because b.verbose was not set when it was set of a

        class secondInstrumentedObjective(obj:Objective) extends Objective{
          override def detailedString(short : Boolean, indent : Long) : String = obj.detailedString(short,indent)
          override def model : Store = obj.model
          override def value : Long = obj.value
        }

        currentB.getMove(new secondInstrumentedObjective(obj), initialObj, secondAcceptanceCriterion) match {
          case NoMoveFound => Long.MaxValue
          case MoveFound(m : Move) =>
            require(m.objAfter < bestObj)
            bestObj = m.objAfter
            toReturn = MoveFound(CompositeMove(List(a.instantiateCurrentMove(intermediaryObjValue),m),bestObj,"DynAndThen"))
            bestObj
        }
      }
    }

    val tmp = a.getMove(new InstrumentedObjectiveForFirstNeighborhood(), initialObj, firstAcceptanceCriterion)

    tmp match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m: Move) =>
        require(m.objAfter == bestObj)
        toReturn
    }
  }

  override def instantiateCurrentMove(newObj: Long): CompositeMove ={
    currentB match{
      case null => throw new Error("DynAndThen is not presently exploring something")
      case s:SupportForAndThenChaining[_] =>
        val moveFromB = s.instantiateCurrentMove(Long.MaxValue)
        val moveFromA = a.instantiateCurrentMove(Long.MaxValue)
        CompositeMove(List(moveFromA,moveFromB),newObj,"DynAndThen(" + moveFromA + "," + moveFromB + ")")
      case _ => throw new Error(s"DynAndThen: You are willing to use a DynAndThen 'a' as a left-hand side of another DynAndThen 'b'. This is ok, but the neighborhood on the right of the DynAndThen 'a' should support chaining, and it does not:$currentB")
    }
  }
}

case class DynAndThenWithPrev[FirstMoveType<:Move](x: Neighborhood with SupportForAndThenChaining[FirstMoveType],
                                                   b: (FirstMoveType,Solution) => Neighborhood,
                                                   maximalIntermediaryDegradation: Long = Long.MaxValue,
                                                   valuesToSave: Iterable[AbstractVariable])
  extends NeighborhoodCombinatorNoProfile(x) {

  val instrumentedA = new SnapShotOnEntry(x,valuesToSave) with SupportForAndThenChaining[FirstMoveType]{
    override def instantiateCurrentMove(newObj: Long): FirstMoveType = x.instantiateCurrentMove(newObj)
  }

  val slave = new DynAndThen(instrumentedA,
    (m:FirstMoveType) => b(m,instrumentedA.snapShot),
    maximalIntermediaryDegradation)


  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult = {
    slave.verbose = this.verbose
    slave.getMove(obj, initialObj, acceptanceCriterion)
  }
}

case class SnapShotOnEntry(a: Neighborhood, valuesToSave:Iterable[AbstractVariable])
  extends NeighborhoodCombinator(a) {

  var snapShot:Solution = null

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion = StrictImprovement): SearchResult = {
    val s = obj.model
    snapShot = s.saveValues(valuesToSave)
    a.getMove(obj,initialObj:Long, acceptanceCriterion)
  }
}

/**
 * This combinator supports a filter on moves, you can post any function to forbid some moves from being explored
 * beware: it is more efficient to filter upfront by appropriately tuning the parameters of the neighborhood a, so this is really some sort of DIY solution.
 * @param a the base neighborhood that we will restrain
 * @param filter the filter function through which you can accept/reject moves from a
 * @tparam MoveType the type of moves that a explores
 */
case class Filter[MoveType<:Move](a: Neighborhood with SupportForAndThenChaining[MoveType],
                                  filter: MoveType => Boolean) extends NeighborhoodCombinator(a) {

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult = {

    val obj2 = new Objective{
      override def detailedString(short: Boolean, indent: Long = 0L): String =
        obj.detailedString(short: Boolean, indent)

      override def model: Store = obj.model

      override def value: Long = {
        if(filter(a.instantiateCurrentMove(Long.MaxValue))){
          obj.value
        }else{
          Long.MaxValue
        }
      }
    }

    a.getMove(obj2,initialObj, acceptanceCriterion)

  }
}

/**
 * This is an atomic combinator, it represent that the neighborhood below should be considered as a single piece.
 * When you commit a move from this neighborhood, "a" is reset, and exhausted in a single move from Atomic(a)
 * Also, Atomic is a jump neighborhood as it cannot evaluate any objective function before the move is committed.
 *
 * @param a The base neighborhood
 * @param bound The bound for jumps
 */
case class AtomicJump(a: Neighborhood, bound: Int = Int.MaxValue) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion = StrictImprovement): SearchResult = {
    CallBackMove(() => a.doAllMoves(_ > bound, obj, acceptanceCriterion), Int.MaxValue, this.getClass.getSimpleName)
  }
}

/**
 * This is an atomic combinator, it represent that the neighborhood below should be considered as a single piece.
 * When you commit a move from this neighborhood, "a" is reset, and exhausted in a single move from Atomic(a)
 * Also, Atomic is a jump neighborhood as it cannot evaluate any objective function before the move is committed.
 *
 * @param a
 */
case class Atomic(a: Neighborhood,
                  shouldStop: Int => Boolean,
                  stopAsSoonAsAcceptableMoves: Boolean = false,
                  aggregateIntoSingleMove: Boolean = false) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion = StrictImprovement): SearchResult = {
    val startSolution = obj.model.solution()
    val stopProc = if(stopAsSoonAsAcceptableMoves){
      nbId:Int => shouldStop(nbId) || acceptanceCriterion(initialObj,obj.value)
    }else{
      shouldStop
    }

    if (aggregateIntoSingleMove) {

      val nbMoves = a.doAllMoves(stopProc, obj, acceptanceCriterion)

      //restore the initial solution
      val endObj = obj.value
      val endSolution = obj.model.solution(true)

      startSolution.restoreDecisionVariables()

      if (nbMoves == 0) {
        NoMoveFound
      } else {
        LoadSolutionMove(endSolution, endObj, s"Atomic($a)")
      }

    } else {
      val allMoves = a.getAllMoves(stopProc, obj, acceptanceCriterion)

      //restore the initial solution
      val endObj = obj.value
      startSolution.restoreDecisionVariables()

      if (allMoves.isEmpty) {
        NoMoveFound
      } else {
        CompositeMove(allMoves, endObj, s"Atomic($a)")
      }
    }
  }

  def stopAsSoonAsAcceptable: Atomic = {
    Atomic(a, shouldStop, stopAsSoonAsAcceptableMoves=true)
  }
}


/**
 * Ejection chains built ouf of a neighborhood
 * it is meant to be used as follows:
 *    neighborhood dynAndThen EjectionChain(_, move => nestStepNeighborhood)
 * @param initMove the initial move to start the chain
 * @param nextNeighborhood given the moves already selected in the chain, generates the next neighborhood
 * @param shouldStop given the number of steps, true is we should sop, false otherwise
 *                   (ejection chains stops as soon as the global move is acceptable, so this is to restrict the length of hte exploration)
 * @param acc the acceptance criterion to use in the chain
 *            the regular acceptance criterion is used to accept the full chain, but each move in the chain is accepted based on this acc
 * @param aggregateMoves true to aggregate the moves int oa single "solution load", false otherwise
 * @param name the name to use in the console
 */
case class EjectionChains(initMove:Move,
                          nextNeighborhood: List[Move] => Neighborhood,
                          shouldStop:Int => Boolean,
                          acceptanceCriterion: AcceptanceCriterion = StrictImprovement,
                          aggregateMoves:Boolean = false,
                          name:String = "EjectionChains") extends NeighborhoodCombinator() {
  override def getMove(obj: Objective,
                       initialObj: Long,
                       accCrit: AcceptanceCriterion = StrictImprovement): SearchResult = {
    if (aggregateMoves) {
      val startSolution = obj.model.solution()
      var prevMoves: List[Move] = List(initMove)
      var currentObj: Long = initialObj
      var nbMoves: Int = 0
      while (!shouldStop(nbMoves) && !accCrit(initialObj, currentObj)) {
        nextNeighborhood(prevMoves).getMove(obj, currentObj, accCrit) match {
          case NoMoveFound =>
            if (accCrit(initialObj, currentObj)) {
              val endSolution = obj.model.solution()
              startSolution.restoreDecisionVariables()
              if (nbMoves >= 1) {
                return MoveFound(LoadSolutionMove(endSolution, currentObj, name))
              } else {
                return MoveFound(DoNothingMove(currentObj,name))
              }
            } else {
              startSolution.restoreDecisionVariables()
              return NoMoveFound
            }
          case MoveFound(move) =>
            move.commit()
            prevMoves = move :: prevMoves
            currentObj = move.objAfter
            nbMoves = nbMoves + 1
        }
      }
      if (acceptanceCriterion(initialObj, currentObj)) {
        val endSolution = obj.model.solution()
        startSolution.restoreDecisionVariables()
        if (nbMoves >= 1) {
          MoveFound(LoadSolutionMove(endSolution, currentObj, name))
        } else {
          MoveFound(DoNothingMove(currentObj,name))
        }
      } else {
        startSolution.restoreDecisionVariables()
        NoMoveFound
      }
    } else {
      val startSolution = obj.model.solution()
      var allMoves:List[Move] = Nil
      var prevMoves: List[Move] = List(initMove)
      var currentObj: Long = initialObj
      var nbMoves: Int = 0
      while (!shouldStop(nbMoves) && !acceptanceCriterion(initialObj, currentObj)) {
        //it start from obj.value, not from currentObj because when used in a cross-product, initObj != obj.value
        nextNeighborhood(prevMoves).getMove(obj, obj.value, accCrit) match {
          case NoMoveFound =>
            if(acceptanceCriterion(initialObj, currentObj)) {
              startSolution.restoreDecisionVariables()
              if(nbMoves >= 1){
                return MoveFound(CompositeMove(allMoves.reverse, currentObj, name))
              }else{
                return MoveFound(DoNothingMove(currentObj,name))
              }
            }else {
              startSolution.restoreDecisionVariables()
              return NoMoveFound
            }
          case MoveFound(move) =>
            move.commit()
            prevMoves = move :: prevMoves
            currentObj = move.objAfter
            nbMoves = nbMoves + 1
            allMoves = move :: allMoves
        }
      }
      startSolution.restoreDecisionVariables()
      if (acceptanceCriterion(initialObj, currentObj)) {
        if (nbMoves >= 1) {
          MoveFound(CompositeMove(allMoves.reverse, currentObj, name))
        } else {
          MoveFound(DoNothingMove(currentObj, name))
        }
      } else {
        NoMoveFound
      }
    }
  }
}
