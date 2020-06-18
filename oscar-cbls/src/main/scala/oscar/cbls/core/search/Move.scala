/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
package oscar.cbls.core.search

import oscar.cbls.core.computation.{CBLSSetVar, Solution, Variable}
import oscar.cbls.core.objective.Objective

/** standard move template
  *
  * @param objAfter         the objective after this assignation will be performed
  *                         in case you degrade the objective because you make a jump, and you do not want to compute it,
  *                         you must set it to Long.MaxValue or just do not specify it, as it is the default value
  *                         we did not use an option there because there would anyway be a need
  *                         for arithmetic on this option in combinators suh as [[Best]]
  *                         Many combinators actually rely on this value to take decisions (eg: [[oscar.cbls.lib.search.combinators.SaveBest]] and [[Best]]
  * @param neighborhoodName the name of the neighborhood that generated this move, used for pretty printing purpose.
  *                         Notice that the name is not the type of the neighborhood.
  * @author renaud.delandtsheer@cetic.be
 */
abstract class Move(val objAfter:Long = Long.MaxValue, val neighborhoodName:String = null){
  /**to actually take the move*/
  def commit(): Unit

  /**
   * to get the list of variables that are modified by the move.
   * use this to update a Tabu for instance
   * notice that is a variable is touched twice by the move, it will appear twice in this list
   * This can happen with a set where we add two elements in two distinct moves that are aggregated into a [[CompositeMove]]
    *
    * @return the list of touched variables.
   */
  def touchedVariables:Iterable[Variable] = throw new Exception(this.getClass().getSimpleName + "cannot provide touched variables")

  /**
   * @return a readable string of the objective after wit ha space before, or an empty string
   */
  def objToString:String = if(objAfter == Long.MaxValue) "" else " objAfter:" +objAfter

  protected def neighborhoodNameToString:String = if (neighborhoodName != null) neighborhoodName + ":" else ""

  /** this performs the move, evaluates the objective function, and backtracks the move
    * notice that the objAfter is supposed to carry the proper value, so you generally do not need to call this
    * since it is not an efficient way to proceed
    * notice that it relies on the touchedVariables method.
    *
    * @param obj the objective function to evaluate
    * @return the value of the objective function if the move were taken
    */
  def evaluate(obj:Objective):Long = {
    val model = obj.model
    val snapshot = model.saveValues(touchedVariables)
    commit()
    val toReturn = obj.value
    model.restoreSolution(snapshot)
    toReturn
  }

  def shortString:String = toString

  def decomposeMove:List[String] = List(this.shortString)
}

object Move{
  def apply(objAfter:Long = 0L, neighborhoodName:String = null)(code: =>Unit):CodedMove =
    new CodedMove(code, objAfter, neighborhoodName)
}
/**
  * this class does not provide an implementation for touchedVariables,
  * since we are only inputting source code for executing the move
  * */
class CodedMove(code: => Unit, override val objAfter: Long, override val neighborhoodName: String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit(): Unit = {code}


  override def toString: String = neighborhoodNameToString + "CodedMove"
}

/**
  * this class does not provide an implementation for touchedVariables,
  * since we are only inputting source code for executing the move
  * you must incorporate the obj into the move name in order to display it
  * */
class EvaluableCodedMove(doAndUndo: () => (() => Unit),
                         override val objAfter: Long = Long.MaxValue,
                         override val neighborhoodName: String = "EvaluableCodedMove",
                         val moveName:String = "EvaluableCodedMove")
  extends Move(objAfter, neighborhoodName){

  override def commit(): Unit = {doAndUndo()}

  override def evaluate(obj: Objective): Long = {
    val undoOp = doAndUndo()
    val toReturn = obj.value
    undoOp()
    toReturn
  }

  override def toString: String = neighborhoodNameToString + ":" + moveName //this does not include obj. obj is to be displayed by the moveName
}

/**
 * this move loads solution s
 *
 * @param s the solution that is loaded when the move is comitted
 * @param objAfter         the objective after this assignation will be performed
 *                         in case you degrade the objective because you make a jump, and you do not want to compute it,
 *                         you must set it to Long.MaxValue or just do not specify it, as it is the default value
 *                         we did not use an option there because there would anyway be a need
 *                         for arithmetic on this option in combinators suh as [[Best]]
 *                         Many combinators actually rely on this value to take decisions (eg: [[oscar.cbls.lib.search.combinators.SaveBest]] and [[Best]]
 * @param neighborhoodName the name of the neighborhood that generated this move, used for pretty printing purpose.
 *                         Notice that the name is not the type of the neighborhood.
 */
case class LoadSolutionMove(s:Solution,override val objAfter:Long, override val neighborhoodName:String = null) extends Move(objAfter,neighborhoodName){
  /** to actually take the move */
  override def commit(): Unit = s.model.restoreSolution(s)

  override def toString : String = s"${neighborhoodNameToString}LoadSolutionMove(objAfter:$objAfter)"
}

/** standard move that adds a value to a CBLSSetVar
  *
  * @param s the variable
  * @param v the value to add to the set
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class AddToSetMove(s:CBLSSetVar,v:Int, override val objAfter:Long, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit(): Unit = {s :+= v}

  override def toString: String = {
    s"${neighborhoodNameToString}AddToSetMove($s :+= $v$objToString)"
  }

  override def touchedVariables: List[Variable] = List(s)
}

/** standard move that removes a value to a CBLSSEtVar
  *
  * @param s the variable
  * @param v the value to remove to the set
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class RemoveFromSetMove(s:CBLSSetVar,v:Int, override val objAfter:Long, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit(): Unit = {s :-= v}

  override def toString: String = {
    s"${neighborhoodNameToString}RemoveFromSetMove($s :-= $v$objToString)"
  }

  override def touchedVariables: List[Variable] = List(s)
}

/**
  * This neighborhood always returns the same move, given in the constructor
  * it checks the objctive function before returning it, and adds the objective funtion value to the move
  *
  * this one canot be chained because there is no UNDO operation defined
  *
  * @param m the move to return when the neighborhood is queried for a move
  */
case class ConstantMoveNeighborhood(m: Move,
                                    skipAcceptanceCriterion:Boolean = false,
                                    neighborhoodName:String = null)
  extends Neighborhood with SupportForAndThenChaining[Move] {
  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    if (skipAcceptanceCriterion) {
      MoveFound(m)
    } else {
      val newObj: Long = m.evaluate(obj)
      if (acceptanceCriterion(initialObj, newObj)) {
        MoveFound(new OverrideObj(m, newObj))
      } else {
        NoMoveFound
      }
    }
  }

  override def instantiateCurrentMove(newObj: Long): Move =
    new OverrideObj(m, newObj,neighborhoodName)
}

case class ConstantMovesNeighborhood[MoveType<:Move](ms:() => Iterable[Long => MoveType],
                                                     selectMoveBehavior:LoopBehavior = First(),
                                                     neighborhoodName:String = "ConstantMovesNeighborhood")
  extends EasyNeighborhoodMultiLevel[MoveType](neighborhoodName) {

  var currentMove:(Long => MoveType) = null

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(initialObj: Long): Unit = {
    val (moveIterator, notifyFound1) = selectMoveBehavior.toIterator(ms())
    while (moveIterator.hasNext) {
      currentMove = moveIterator.next()

      val newObj = currentMove(Int.MaxValue).evaluate(obj)

      if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {
        notifyFound1()
      }
    }
    currentMove = null
  }

  override def instantiateCurrentMove(newObj: Long): MoveType =
    currentMove(newObj)
}

class OverrideObj(initMove:Move, objAfter:Long, neighborhoodName:String = null)
  extends Move(objAfter = objAfter, neighborhoodName = if(neighborhoodName != null) neighborhoodName else initMove.neighborhoodName){

  override def commit(): Unit = initMove.commit()
  
  override def toString: String = s"OverrideObj(initMove:$initMove$objToString)"

  override def decomposeMove:List[String] = initMove.decomposeMove
}

case class DoNothingNeighborhood() extends Neighborhood with SupportForAndThenChaining[DoNothingMove]{
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    val objValue = obj.value
    if(acceptanceCriterion(objValue,objValue)){
      MoveFound(DoNothingMove(objValue))
    }else{
      NoMoveFound
    }
  }

  override def instantiateCurrentMove(newObj : Long) : DoNothingMove = DoNothingMove(newObj)
}

case class DoNothingMove(override val objAfter:Long,override val neighborhoodName:String = null) extends Move(objAfter,neighborhoodName){
  override def commit() : Unit = {}

  override def decomposeMove:List[String] = List.empty
}

/** a composition of a list of moves; the move will be taken in the order given by the list
  *
  * @param ml the list of moves constituting this one
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class CompositeMove(ml:List[Move], override val objAfter:Long, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  def this(ml:List[Move]){
    this(ml, ml.last.objAfter)
  }

  override def commit(): Unit = {
    for(m <- ml) m.commit()
  }

  override def toString: String  = {
    val parts = decomposeMove
    s"${neighborhoodNameToString}CompositeMove(size:${parts.size} [${parts.mkString(",")}]$objToString)"
  }

  override def touchedVariables: List[Variable] = ml.flatMap(_.touchedVariables)

  override def shortString: String = s"CompositeMove(${ml.map(_.shortString).mkString(",")})"

  override def decomposeMove:List[String] = this.ml.flatMap(_.decomposeMove)
}

case class NamedMove(m:Move, override val neighborhoodName:String = null)
  extends Move(m.objAfter, neighborhoodName){

  override def commit(): Unit = {
    m.commit()
  }

  override def toString: String  = {
    s"$neighborhoodNameToString$m"
  }

  override def touchedVariables: Iterable[Variable] = m.touchedVariables

  override def evaluate(obj: Objective): Long = m.evaluate(obj)

  override def decomposeMove:List[String] = m.decomposeMove
}

/** an instrumented move that performs a callBack before being taken
  * the move itself is given in parameter.
  *
  * @param initialMove the actual move
  * @param callBack the method to invoke before the actual move is taken
  * @author renaud.delandtsheer@cetic.be
  */
case class InstrumentedMove(initialMove:Move, callBack: () => Unit = null, afterMove: () => Unit = null) extends Move(initialMove.objAfter, initialMove.neighborhoodName){
  def commit(): Unit ={
    if(callBack != null) callBack()
    initialMove.commit()
    if(afterMove != null) afterMove()
  }

  override def toString: String = initialMove.toString

  override def touchedVariables: Iterable[Variable] = initialMove.touchedVariables

  override def decomposeMove:List[String] = initialMove.decomposeMove
}

object CallBackMove{
  def apply(callBack: () => Unit, objAfter:Long, neighborhoodName:String)=
    new CallBackMove[Null](_ => {callBack()}, objAfter, neighborhoodName)
}

/** a callBackMove when committed calls some method
  * this is how it takes its move
  *
  * @param callBack the method that is called when the move is committed it takes a parameter of type T, which is given in param
  * @param objAfter         the objective after this assignation will be performed
  *                         in case you degrade the objective because you make a jump, and you do not want to compute it,
  *                         you must set it to Long.MaxValue or just do not specify it, as it is the default value
  *                         we did not use an option there because there would anyway be a need
  *                         for arithmetic on this option in combinators suh as [[Best]]
  *                         Many combinators actually rely on this value to take decisions (eg: [[oscar.cbls.lib.search.combinators.SaveBest]] and [[Best]]
  * @param neighborhoodName the name of the neighborhood that generated this move, used for pretty printing purpose.
  *                         Notice that the name is not the type of the neighborhood.
  * @param param            the parameter that is passed to the callBack method when the move is committed
  * @tparam T
  */
case class CallBackMove[T](callBack: T => Unit, override val objAfter:Long, override val neighborhoodName:String, param:T = null) extends Move{
  def commit(): Unit = {
    callBack(param)
  }

  override def toString: String = {
    neighborhoodNameToString
  }
}
