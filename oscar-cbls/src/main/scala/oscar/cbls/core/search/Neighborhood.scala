/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cbls.core.search

import oscar.cbls.core.computation.Store
import oscar.cbls.core.objective.{LoggingObjective, Objective}
import oscar.cbls.lib.search.combinators._
import oscar.cbls.util.Properties

import scala.collection.immutable.SortedMap

abstract sealed class SearchResult
case object NoMoveFound extends SearchResult

case class MoveFound(m: Move) extends SearchResult {
  def commit(): Unit = { m.commit() }
  def objAfter: Long = m.objAfter
  override def toString: String = m.toString
}

object SearchResult {
  implicit def moveToSearchResult(m: Move): MoveFound = MoveFound(m)
}

abstract class JumpNeighborhood(name:String) extends Neighborhood(name) {

  /**
   * the method that actually performs the move
   * notice that this method is called when the move is committed,
   * which happens after the neighborhood returns the move.
   */
  def doIt(): Unit

  /**
   * this method checks that the jump can actually be performed
   * it is called before the neighborhood returns either MoveFound or NoMoveFound
   * notice that the doIt method is called only if canDoIt returned true.
   * override it if your jump might not be applicable
   * (and do not forget to handle this case in your search strategy)
   *
   * @return
   */
  def canDoIt: Boolean = true

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean = (oldObj, newObj) => oldObj > newObj): SearchResult = {
    if (canDoIt) CallBackMove(() => doIt(), valueAfter, name)
    else NoMoveFound
  }

  /**
   * returns the value after the move
   * called by getMove, by default, this is Long.MaxValue, which is the correct value for a jump
   * in case your jump does not modify the obj function and you want to include this in the move description, override this method
   *
   * @return
   */
  def valueAfter: Long = Long.MaxValue
}

abstract class JumpNeighborhoodParam[T](name:String) extends Neighborhood(name) {

  final def doIt(): Unit = {
    doIt(getParam)
  }

  def doIt(param: T): Unit

  /**if null is returned, the neighborhood returns NoMoveFound*/
  def getParam: T
  def getShortDescription(param: T): String

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    val param: T = getParam
    if (param == null) NoMoveFound
    else CallBackMove((param: T) => doIt(param), Long.MaxValue, name, param)
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class Neighborhood(name:String = null) {

  /**
   * collects and returns the statistics that have been requested in the neighborhood.
   * use the Statistics combinator to activate the collection of statistics
   *
   * @return
   */
  final def profilingStatistics:String = Properties.justifyRightArray(Profile.statisticsHeader :: collectProfilingStatistics/*.map(a => ("" :: a.toList).toArray)*/).mkString("\n")
  def collectProfilingStatistics:List[Array[String]] = List.empty

  /**
   * the method that returns a move from the neighborhood.
   * The returned move should typically be accepted by the acceptance criterion over the objective function.
   * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
   *
   * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.core.objective.Objective]] there is an implicit conversion available
   * @param acceptanceCriterion
   * @return
   */
  def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean = (oldObj, newObj) => oldObj > newObj): SearchResult

  //this resets the internal state of the Neighborhood
  def reset(): Unit = {}

  def resetStatistics(): Unit = {}

  override def toString: String = (if(name == null) this.getClass.getSimpleName else name)

  var _verbose: Int = 0

  /**
   * possible verbosity levels:
   * 0: no verbosities
   * 1: every 10th of a second, summarise all performed moves, by neighbourhoods
   * 2: print every move
   * 3: print every search
   * 4: print every explored neighbour
   * */
  def verbose: Int = _verbose
  /**
   * possible verbosity levels:
   * 0: no verbosities
   * 1: every 10th of a second, summarise all performed moves, by neighbourhoods
   * 2: print every move
   * 3: print every search
   * 4: print every explored neighbour
   * */
  def verbose_=(i: Int): Unit = {
    _verbose = i
    additionalStringGenerator = null
  }

  var additionalStringGenerator: () => String = null

  /**
   * sets the verbosity level with an additional string generator that ins called eieher on eahc move (level = 1L)
   *   or for each explored neighbor (level = 2L)
   */
  def verboseWithExtraInfo(verbosity: Int, additionalString: () => String): Unit = {
    verbose = verbosity
    additionalStringGenerator = additionalString
  }

  protected def printMoveSythesis:Boolean = verbose == 1L
  protected def printTakenMoves: Boolean = verbose >= 2L
  protected def printExploredNeighborhoods: Boolean = verbose >= 3L
  protected def printExploredNeighbors: Boolean = verbose >= 4L

  /**
   * @return true if a move has been performed, false otherwise
   */
  def doImprovingMove(obj: Objective): Boolean = 0L != doAllMoves(_ >= 1L, obj)

  /**
   * @param shouldStop a function that takes the iteration number and returns true if search should be stopped
   *                   eg if the problem is considered as solved
   *                   you can evaluate some objective function there such as a violation degree
   * @param acceptanceCriterion a criterion for accepting a move
   *                            by default, we only accept improving moves, but you could change it
   *                            and accept degrading moves as well (eg for tabu search)
   *                            notice that some moves do not consider the acceptance criterion
   *                            because their purpose is to randomize the current solution.
   * @return the number of moves performed
   */
  def doAllMoves(shouldStop: Int => Boolean = _ => false, obj: Objective, acceptanceCriterion: (Long, Long) => Boolean = (oldObj, newObj) => oldObj > newObj): Int = {

    def nStrings(n: Int, s: String): String = if (n <= 0) "" else s + nStrings(n - 1, s)
    def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)
    def trimToLength(s: String, l: Int) = if (s.length >= l) s.substring(0, l) else s

    if (verbose != 0){
      println(s"start doAllMove at ${java.time.LocalDateTime.now}")
      println(s"initial objective function:$obj")
    }
    var moveSynthesis = SortedMap.empty[String,Int]

    val startSearchNanotime = System.nanoTime()
    var nanoTimeAtNextSynthesis = startSearchNanotime + (1000*1000*100) //100ms

    var bestObj = Long.MaxValue
    var prevObj = Long.MaxValue
    var toReturn = 0
    var moveCount = 0
    val enrichedObj = if (additionalStringGenerator == null) obj else new ObjWithStringGenerator(obj, additionalStringGenerator)
    while (!shouldStop(moveCount)) {
      getMove(enrichedObj, obj.value, acceptanceCriterion) match {
        case NoMoveFound =>

          if(printMoveSythesis && moveSynthesis.nonEmpty){
            val finalObj = obj.value
            val firstPrefix = if (finalObj < prevObj) "-"
            else if (finalObj == prevObj) "="
            else "+"
            val smallPaddingLength = 20

            val secondPrefix = (if (finalObj < bestObj) {
              " # "
            } else if (finalObj == bestObj) " ° "
            else "   ") + padToLength(finalObj.toString,smallPaddingLength)
            println(firstPrefix + secondPrefix + moveSynthesis.toList.map({case ((name,n)) => padToLength(trimToLength(name, smallPaddingLength-4)+ ":"+n, smallPaddingLength)}).mkString(" "))

            moveSynthesis = SortedMap.empty[String,Int]
            nanoTimeAtNextSynthesis = System.nanoTime() + (1000*1000*100) //100ms
          }
          if (printTakenMoves || printMoveSythesis) {

            val runDurationMs:Long = ((System.nanoTime() - startSearchNanotime) / 1000000).ceil.toLong
            val hours = (runDurationMs / (1000.0 * 60 * 60)).floor.toInt
            val minutes = (runDurationMs / (1000.0 * 60)).floor.toInt % 60
            val seconds: Double = (runDurationMs / 1000.0) % 60

            println(f"no more move found after $toReturn it, duration:$hours:$minutes:$seconds%1.3f")
          }

          return toReturn;
        case m: MoveFound =>
          if(printMoveSythesis){

            var didPrintBefore:Boolean = false
            if(m.m.objAfter == Long.MaxValue && moveSynthesis.nonEmpty){
              //in case we are jumping and tehre are some moves to print, we force print a synthesis
              didPrintBefore = true
              //TODO: we should not consider m.objAfter!!! we should consider the obj BEFORE the move is taken

              val currentObjValue = obj.value // this is the obj before the jump
              //flush the preceding moves before a jump
              val firstPrefix = if (currentObjValue  < prevObj) "-"
              else if (currentObjValue  == prevObj) "="
              else "+"

              prevObj = currentObjValue //we update it since it was displayed

              val smallPaddingLength = 20

              val secondPrefix = (if (currentObjValue < bestObj) {
                bestObj = currentObjValue
                " # "
              } else if (currentObjValue == bestObj) " ° "
              else "   ") + padToLength(currentObjValue.toString,smallPaddingLength)
              println(firstPrefix + secondPrefix + moveSynthesis.toList.map({case ((name,n)) => padToLength(trimToLength(name, smallPaddingLength-4)+ ":"+n, smallPaddingLength)}).mkString(" "))

              moveSynthesis = SortedMap.empty[String,Int]
              nanoTimeAtNextSynthesis = System.nanoTime() + (1000*1000*100) //100ms
            }

            val neighborhoodName = m.m.neighborhoodName
            moveSynthesis = moveSynthesis + ((neighborhoodName,moveSynthesis.getOrElse(neighborhoodName,0)+1))

            val printSynthesis = (System.nanoTime() >= nanoTimeAtNextSynthesis) || m.m.objAfter == Long.MaxValue

            if(printSynthesis){

              val firstPrefix = if (m.objAfter < prevObj) "-"
              else if (m.objAfter == prevObj) "="
              else "+"

              prevObj = m.objAfter

              val smallPaddingLength = 20

              val secondPrefix = (if (m.objAfter < bestObj) {
                bestObj = m.objAfter
                " # "
              } else if (m.objAfter == bestObj) " ° "
              else "   ") + padToLength(m.objAfter.toString,smallPaddingLength)
              println(firstPrefix + secondPrefix + moveSynthesis.toList.map({case ((name,n)) => padToLength(trimToLength(name, smallPaddingLength-4)+ ":"+n, smallPaddingLength)}).mkString(" "))

              moveSynthesis = SortedMap.empty[String,Int]
              nanoTimeAtNextSynthesis = System.nanoTime() + (1000*1000*100) //100ms
            }else if(didPrintBefore){
              prevObj = m.objAfter
            }

            m.commit()
            //TODO: additionalString should be handled with synthesis!
            if (printSynthesis && additionalStringGenerator != null) println(additionalStringGenerator())
            if (obj.value == Long.MaxValue) println("Warning : objective == MaxLong, maybe you have some strong constraint violated?")

            require(m.objAfter == Long.MaxValue || obj.value == m.objAfter, "neighborhood was lying!:" + m + " got " + obj)

          }else if (printTakenMoves) {

            if (m.objAfter != Long.MaxValue) {
              val firstPrefix = if (m.objAfter < prevObj) "-"
              else if (m.objAfter == prevObj) "="
              else "+"

              prevObj = m.objAfter

              val secondPrefix = (if (m.objAfter < bestObj) {
                bestObj = m.objAfter
                " # "
              } else if (m.objAfter == bestObj) " ° "
              else "   ") + " " + m.objAfter

              println(firstPrefix + secondPrefix + "   " + m.toString())
            } else {
              prevObj = m.objAfter
              println(m.toString())
            }

            m.commit()
            if (additionalStringGenerator != null) println(additionalStringGenerator())
            if (obj.value == Long.MaxValue) println("Warning : objective == MaxLong, maybe you have some strong constraint violated?")

            require(m.objAfter == Long.MaxValue || obj.value == m.objAfter, s"neighborhood was lying!:$m got $obj")

          }else{
            m.commit()
            if (additionalStringGenerator != null) println(additionalStringGenerator())
            if (obj.value == Long.MaxValue) println("Warning : objective == MaxLong, maybe you have some strong constraint violated?")

            require(m.objAfter == Long.MaxValue || obj.value == m.objAfter, s"neighborhood was lying!:$m got $obj")
          }
      }

      toReturn += 1
      moveCount += 1
    }

    if(printMoveSythesis && moveSynthesis.nonEmpty) {

      val currentObjValue = obj.value
      //flush the preceding moves before a jump
      val firstPrefix = if (currentObjValue < prevObj) "-"
      else if (currentObjValue == prevObj) "="
      else "+"

      val smallPaddingLength = 20

      val secondPrefix = (if (currentObjValue < bestObj) {
        bestObj = currentObjValue
        " # "
      } else if (currentObjValue == bestObj) " ° "
      else "   ") + padToLength(currentObjValue.toString, smallPaddingLength)
      println(firstPrefix + secondPrefix + moveSynthesis.toList.map({case ((name, n)) => padToLength(trimToLength(name, smallPaddingLength - 4) + ":" + n, smallPaddingLength) }).mkString(" "))
    }

    if (printTakenMoves || printMoveSythesis) {
      println(s"stop criteria of doAllMove met after $moveCount moves, ${((System.nanoTime() - startSearchNanotime)/1000000).toInt} ms")
    }
    toReturn
  }

  def getAllMoves(shouldStop: Int => Boolean = _ => false, obj: Objective, acceptanceCriterion: (Long, Long) => Boolean = (oldObj, newObj) => oldObj > newObj): List[Move] = {

    var toReturn : List[Move] = List.empty

    val instrumentedThis = this afterMoveOnMove(m => toReturn = m :: toReturn)
    instrumentedThis.doAllMoves(shouldStop,obj,acceptanceCriterion)

    toReturn.reverse
  }

}

/**
 * a neighborhood that never finds any move (quite useless, actually)
 */
case object NoMoveNeighborhood extends Neighborhood {
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = NoMoveFound
}


trait SupportForAndThenChaining[MoveType<:Move] extends Neighborhood{

  def instantiateCurrentMove(newObj:Long):MoveType

  def dynAndThen(other:MoveType => Neighborhood,maximalIntermediaryDegradation: Long = Long.MaxValue):DynAndThen[MoveType] = {
    new DynAndThen[MoveType](this,other,maximalIntermediaryDegradation)
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
   * @param b given that the move returned by the first neighborhood is committed, we explore the globally improving moves of this one
   * @author renaud.delandtsheer@cetic.be
   */
  def andThen(b: Neighborhood) = AndThen(this, b)

  /**
   * This combinator supports a filter on moves, you can post any function to forbid some moves from being explored
   * beware: it is more efficient to filter upfront by appropriately tuning the parameters of the neighborhood a, so this is really some sort of DIY solution.
   * @param filter the filter function through which you can accept/reject moves from a
   */
  def filter(filter:MoveType => Boolean):Neighborhood = new Filter[MoveType](this, filter)
}

/**
 * This is an easier way to implement your neighborhood; it provides a simplified interface and hides away searching for the best move vs. the first move
 * and the management of the acceptingCriterion.
 *
 * to implement a neighborhood, you must implement the method exploreNeighborhood
 * in this method, you evaluate moves, and to notify that a move has been
 * explored you call the method evaluateCurrentMoveObjTrueIfStopRequired(newObj:Long)
 *
 * this method tells you if the search must be stopped, or not.
 *
 * you must also implement the method instantiateCurrentMove,
 * so that the framework can actually get the current move, notably to return and comit it.
 *
 * this method must be able to return its result when you call the method evaluateCurrentMoveObjTrueIfStopRequired
 *
 * to evaluate the objective function, the Objective is in the variable obj
 *
 * @param best true if you want the best move false if you want the first acceptable move
 * @param neighborhoodName the name of the neighborhood, used for verbosities
 */
abstract class EasyNeighborhood[M<:Move](best:Boolean = false, neighborhoodName:String=null)
  extends Neighborhood with SupportForAndThenChaining[M]{

  protected def neighborhoodNameToString: String = if (neighborhoodName != null) neighborhoodName else this.getClass().getSimpleName()

  override def toString: String = neighborhoodNameToString

  //passing parameters, and getting return values from the search
  private var oldObj: Long = 0L
  private var acceptanceCriterion: (Long, Long) => Boolean = null
  private var toReturnMove: Move = null
  private var bestNewObj: Long = Long.MaxValue
  protected var obj: Objective = null

  override final def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {

    oldObj = initialObj
    this.acceptanceCriterion = acceptanceCriterion
    toReturnMove = null
    bestNewObj = Long.MaxValue
    this.obj = if (printExploredNeighbors) new LoggingObjective(obj) else obj
    if (printExploredNeighborhoods)
      println(neighborhoodNameToString + ": start exploration")

    exploreNeighborhood()

    if (toReturnMove == null || (best && !acceptanceCriterion(oldObj, bestNewObj))) {
      if (printExploredNeighborhoods) {
        println(neighborhoodNameToString + ": no move found")
      }
      NoMoveFound
    } else {
      if (printExploredNeighborhoods) {
        println(neighborhoodNameToString + ": move found: " + toReturnMove)
      }
      toReturnMove
    }
  }

  /**
   * This is the method you must implement and that performs the search of your neighborhood.
   * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
   * as explained in the documentation of this class
   */
  def exploreNeighborhood(): Unit

  def instantiateCurrentMove(newObj: Long): M

  var tmpNewObj: Long = 0L

  def evaluateCurrentMoveObjTrueIfStopRequired(newObj: Long): Boolean = {
    //cas à gérer:
    //verbose (on affiche le mouvement; qu'on instancie donc avec obj)
    //andThen (pas utile de l'instancier sns obj pq on va de tt façons propager en commençant le voisinage suivant.
    //normal
    //pour l'instant, cas normal uniquement (en fait le AndThen sera géré par le combinateur idoine)

    val myPrintExploredNeighbors = printExploredNeighbors

    if (best) {
      if (newObj < bestNewObj) {
        bestNewObj = newObj
        toReturnMove = instantiateCurrentMove(newObj)
        if (myPrintExploredNeighbors) {
          println(s"Explored $toReturnMove, new best, saved (might be filtered out if best is not accepted)")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
      } else {
        if (myPrintExploredNeighbors) {
          println(s"Explored ${instantiateCurrentMove(newObj)}, not the new best, not saved")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
      }
      false //since we are looking for the best one, we do not stop
    } else {
      if (acceptanceCriterion(oldObj, newObj)) {
        bestNewObj = newObj
        toReturnMove = instantiateCurrentMove(newObj)
        if (myPrintExploredNeighbors) {
          println(s"Explored $toReturnMove, accepted, exploration stopped")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
        true //since we are looking for the first one, we stop
      } else {
        //explored, but not saved
        if (myPrintExploredNeighbors) {
          println(s"Explored ${instantiateCurrentMove(newObj)}, not accepted, not saved")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
        false
      }
    }
  }

  def afterMoveOnMove(proc:M => Unit):Neighborhood = new DoOnMove(this,(m:Move) => proc(m.asInstanceOf[M]))
}

abstract class EasyNeighborhoodMultiLevel[M<:Move](neighborhoodName:String=null)
  extends Neighborhood with SupportForAndThenChaining[M]{

  protected def neighborhoodNameToString: String = if (neighborhoodName != null) neighborhoodName else this.getClass().getSimpleName()

  override def toString: String = neighborhoodNameToString

  //passing parameters, and getting return values from the search
  private var oldObj: Long = 0L
  private var acceptanceCriterion: (Long, Long) => Boolean = null
  private var toReturnMove: Move = null
  private var bestNewObj: Long = Long.MaxValue
  protected var obj: Objective = null
  private var exploring = false // to check that it is not called recursiely because it is not reentrant

  override final def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {

    require(!exploring,this + " is not a re-entrant neighborhood")
    exploring = true

    oldObj = initialObj
    this.acceptanceCriterion = acceptanceCriterion
    toReturnMove = null
    bestNewObj = initialObj //Long.MaxValue // //because we do not want "no move" to be considered as an actual move.
    this.obj = if (printExploredNeighbors) new LoggingObjective(obj) else obj
    if (printExploredNeighborhoods)
      println(neighborhoodNameToString + ": start exploration")

    exploreNeighborhood(oldObj)

    exploring = false

    if (toReturnMove == null) {
      if (printExploredNeighborhoods) {
        println(neighborhoodNameToString + ": no move found")
      }
      NoMoveFound
    } else {
      if (printExploredNeighborhoods) {
        println(neighborhoodNameToString + ": move found: " + toReturnMove)
      }
      toReturnMove
    }
  }

  /**
   * This is the method you must implement and that performs the search of your neighborhood.
   * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
   * as explained in the documentation of this class
   */
  def exploreNeighborhood(initialObj: Long): Unit

  def instantiateCurrentMove(newObj: Long): M

  def moveHasBeenFound:Boolean = toReturnMove != null

  def evaluateCurrentMoveObjTrueIfSomethingFound(newObj: Long): Boolean = {
    //on teste l'acceptance criterion sur tout
    //on garde toujours le meilleur mouvement
    //on dit juste si un mouvement a été accepté et améliore le best so far ou pas

    val myPrintExploredNeighbors = printExploredNeighbors

    if (newObj < bestNewObj && acceptanceCriterion(oldObj, newObj)) {
      bestNewObj = newObj
      toReturnMove = instantiateCurrentMove(newObj)
      if (myPrintExploredNeighbors) {
        println(s"Explored $toReturnMove, new best accepted)")
        println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
      }
      true
    } else {
      if (myPrintExploredNeighbors) {
        println(s"Explored ${instantiateCurrentMove(newObj)}, not the new best or not accepted, not saved")
        println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
      }
      false
    }
  }

  def afterMoveOnMove(proc:M => Unit):Neighborhood = DoOnMove(this,(m:Move) => proc(m.asInstanceOf[M]))
}

class ObjWithStringGenerator(obj: Objective, additionalStringGenerator: () => String) extends Objective {
  override def detailedString(short: Boolean, indent: Long): String = {
    obj.detailedString(short,indent)+ "\n" + nSpace(indent) + additionalStringGenerator().split("\\R",-1).mkString("\n" + nSpace(indent)) + "\n"
  }

  override def model: Store = obj.model

  override def value: Long = obj.value
}
