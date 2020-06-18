package oscar.cbls.lib.search.combinators

import oscar.cbls._
import oscar.cbls.core.computation.{AbstractVariable, Snapshot, Store}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{CallBackMove, CompositeMove, DoNothingNeighborhood, Move, MoveFound, Neighborhood, NeighborhoodCombinator, NoMoveFound, SearchResult, SupportForAndThenChaining}

abstract class NeighborhoodCombinatorNoProfile(a: Neighborhood*) extends NeighborhoodCombinator(a:_*){
  override def collectProfilingStatistics: List[Array[String]] = List.empty
  override def resetStatistics(): Unit ={}
}

object Mu {

  def apply[MoveType <: Move](firstNeighborhood : Neighborhood with SupportForAndThenChaining[MoveType],
                              neighborhoodGenerator : List[(MoveType)] => Option[Neighborhood with SupportForAndThenChaining[MoveType]],
                              maxDepth : Long,
                              intermediaryStops : Boolean): Neighborhood  with SupportForAndThenChaining[CompositeMove] = {
    Mu[MoveType,Any](
      firstNeighborhood,
      (l,_) => neighborhoodGenerator(l) match{
        case None => None
        case Some(n) => Some((n,Unit))
      },
      Unit,
      maxDepth,
      intermediaryStops)
  }

  /**
   *
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
                                 neighborhoodGenerator : (List[(MoveType)], X) => Option[(Neighborhood with SupportForAndThenChaining[MoveType], X)],
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
    new ChainableName(dynAndThen(firstNeighborhood,
      generateNextNeighborhood(List.empty, maxDepth - 1L, x0)),"Mu(" + firstNeighborhood + ")")
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
 * @author renaud.delandtsheer@cetic.be
 */
class DynAndThen[FirstMoveType<:Move](a:Neighborhood with SupportForAndThenChaining[FirstMoveType],
                                      b:(FirstMoveType => Neighborhood),
                                      maximalIntermediaryDegradation: Long = Long.MaxValue)
  extends NeighborhoodCombinatorNoProfile(a) with SupportForAndThenChaining[CompositeMove]{

  //we need to store currentB here because we might need to instantiate the current move from it.
  var currentB:Neighborhood = null

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    var bestObj:Long = Long.MaxValue
    var toReturn:SearchResult = NoMoveFound

    //the acceptance criterion is on the diff between the oldObj and the newObj over the two consecutive moves
    //it is evaluated for the second move
    //the first move is about accepting all moves that are not maxVal, since the newObj is for the consecutive moves,
    // and is already accepted by the time it is returned to the first neighrhood
    def firstAcceptanceCriterion(oldObj: Long, newObj: Long): Boolean = {
      newObj != Long.MaxValue
    }

    def secondAcceptanceCriteria(intermediaryObj: Long, newObj: Long): Boolean = {
      //we ignore the intermediaryObj.
      (newObj < bestObj) && acceptanceCriteria(initialObj, newObj)
    }

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
            }else{
              intermediaryVal
            }
          }else{
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

        currentB.getMove(new secondInstrumentedObjective(obj), initialObj, secondAcceptanceCriteria) match {
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
      case _ => throw new Error("DynAndThen: Neighborhood on the right cannot be chained")
    }
  }
}

case class DynAndThenWithPrev[FirstMoveType<:Move](x:Neighborhood with SupportForAndThenChaining[FirstMoveType],
                                                   b:((FirstMoveType,Snapshot) => Neighborhood),
                                                   maximalIntermediaryDegradation:Long = Long.MaxValue,
                                                   valuesToSave:Iterable[AbstractVariable]) extends NeighborhoodCombinatorNoProfile(x){

  val instrumentedA = new SnapShotOnEntry(x,valuesToSave) with SupportForAndThenChaining[FirstMoveType]{
    override def instantiateCurrentMove(newObj: Long): FirstMoveType = x.instantiateCurrentMove(newObj)
  }

  val slave = new DynAndThen(instrumentedA,
    (m:FirstMoveType) => b(m,instrumentedA.snapShot),
    maximalIntermediaryDegradation)


  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    slave.verbose = this.verbose
    slave.getMove(obj, initialObj, acceptanceCriterion)
  }
}

case class SnapShotOnEntry(a: Neighborhood, valuesToSave:Iterable[AbstractVariable])
  extends NeighborhoodCombinator(a){

  var snapShot:Snapshot = null

  override def getMove(obj: Objective,initialObj:Long,
                       acceptanceCriterion: (Long, Long) => Boolean = (oldObj, newObj) => oldObj > newObj): SearchResult = {
    val s = obj.model
    snapShot = s.snapShot(valuesToSave)
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
case class Filter[MoveType<:Move](a:Neighborhood with SupportForAndThenChaining[MoveType], filter:MoveType => Boolean) extends NeighborhoodCombinator(a) {

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {

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
 * @param a
 */
case class AtomicJump(a: Neighborhood, bound: Int = Int.MaxValue) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean = (oldObj, newObj) => oldObj > newObj): SearchResult = {
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
case class Atomic(a: Neighborhood, shouldStop:Int => Boolean, stopAsSoonAsAcceptableMoves:Boolean = false) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean = (oldObj, newObj) => oldObj > newObj): SearchResult = {

    val startSolution = obj.model.solution(true)

    val stopProc = if(stopAsSoonAsAcceptableMoves){
      nbId:Int => shouldStop(nbId) || acceptanceCriterion(initialObj,obj.value)
    }else{
      shouldStop
    }

    val allMoves = a.getAllMoves(stopProc, obj, acceptanceCriterion)

    //restore the initial solution
    val endObj = obj.value
    obj.model.restoreSolution(startSolution)

    if(allMoves.isEmpty){
      NoMoveFound
    } else {
      CompositeMove(allMoves,endObj, s"Atomic($a)")
    }
  }

  def stopAsSoonAsAcceptable:Atomic = {
    Atomic(a: Neighborhood, shouldStop:Int => Boolean, stopAsSoonAsAcceptableMoves=true)
  }
}

