package oscar.cbls.lib.search.combinators

import oscar.cbls.algo.heap.{BinomialHeap, BinomialHeapWithMove}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search._

import scala.annotation.tailrec

/**
  * At each invocation, this combinator explores one of the neighborhoods in l (and repeat if it is exhausted)
  * neighborhoods are selected based on their slope. the slope is the total gain in objective function performed by the neighborhood, divided by the total amount of time spend exploring the neighborhood.
  * a tabu is added: in case a neighborhood is exhausted, it is not explored for a number of exploration of this combinator
  * the tabu can be overriden if all neighborhoods explored are exhausted. tabu neighborhood can be explored anyway if they are still tabu, but for less than overrideTabuOnFullExhaust invocations of this combinator
  * the refresh parameter forces the combinator to try all neighborhoods every "refresh" invocation. it is useful because some neighorhood can perform poorly at the beginning of search and much better later on, and we do not want the combinator to just "stick to its first impression"
  * @param l the neighborhoods to select from
  * @param tabuLength the number of invocation that they will not be explored when exhausted
  * @param overrideTabuOnFullExhaust the tabu can be overriden if all explored neighbors are exhausted, for each neighborhood that is tabu for les than this override
  * @param refresh a refresh of the slopee measuring must be perfored every refresh iterations
  */
case class BestSlopeFirst(l:List[Neighborhood],
                          tabuLength:Int = 10,
                          overrideTabuOnFullExhaust:Long = 9,
                          refresh:Int = 100)
  extends BestNeighborhoodFirst(l, tabuLength, overrideTabuOnFullExhaust, refresh){
  override protected def bestKey(p:Profile):Long = -p.slopeForCombinators()
}

/**
  * At each invocation, this combinator explores one of the neighborhoods in l (and repeat if it is exhausted)
  * neighborhoods are selected based on their speed the fasted one to find a move is selected
  * a tabu is added: in case a neighborhood is exhausted, it is not explored for a number of exploration of this combinator
  * the tabu can be overriden if all neighborhoods explored are exhausted. tabu neighborhood can be explored anyway if they are still tabu, but for less than overrideTabuOnFullExhaust invocations of this combinator
  * the refresh parameter forces the combinator to try all neighborhoods every "refresh" invocation. it is useful because some neighorhood can perform poorly at the beginning of search and much better later on, and we do not want the combinator to just "stick to its first impression"
  * @param l the neighborhoods to select from
  * @param tabuLength the number of invocation that they will not be explored when exhausted
  * @param overrideTabuOnFullExhaust the tabu can be overriden if all explored neighbors are exhausted, for each neighborhood that is tabu for les than this override
  * @param refresh a refresh of the slopee measuring must be perfored every refresh iterations
  */
case class FastestFirst(l:List[Neighborhood],
                        tabuLength:Int = 10,
                        overrideTabuOnFullExhaust:Long = 9,
                        refresh:Int = 100)
  extends BestNeighborhoodFirst(l, tabuLength, overrideTabuOnFullExhaust, refresh){
  override protected def bestKey(p:Profile):Long = - (p.totalTimeSpentMoveFound / p.nbFound).toInt
}

abstract class BestNeighborhoodFirst(l:List[Neighborhood],
                                     tabuLength:Int,
                                     overrideTabuOnFullExhaust:Long,
                                     refresh:Int)
  extends NeighborhoodCombinator(l:_*) {
  require(overrideTabuOnFullExhaust < tabuLength, "overrideTabuOnFullExhaust should be < tabuLength")

  protected var it:Int = 0
  protected def bestKey(p:Profile):Long

  protected val neighborhoodArray: Array[Profile] = l.map(Profile(_)).toArray
  protected val tabu:Array[Int] = Array.fill(neighborhoodArray.length)(0)
  protected var tabuNeighborhoods = new BinomialHeap[Int](tabu(_),tabu.length)

  protected val neighborhoodHeap = new BinomialHeapWithMove[Int]((neighborhoodID:Int) => bestKey(neighborhoodArray(neighborhoodID)), neighborhoodArray.length)
  neighborhoodArray.indices.foreach((i : Int) => neighborhoodHeap.insert(i))

  private def getBestNeighborhooID:Long = neighborhoodHeap.getFirst

  private def updateNeighborhodPerformances(neighborhoodID:Int): Unit ={
    neighborhoodHeap.notifyChange(neighborhoodID)
  }

  private def updateTabu(): Unit ={
    it +=1
    while(tabuNeighborhoods.nonEmpty && tabu(tabuNeighborhoods.getFirst) <= it){
      val newNonTabu = tabuNeighborhoods.popFirst()
      neighborhoodArray(newNonTabu).reset()
      neighborhoodHeap.insert(newNonTabu)
    }
  }

  protected def makeTabu(neighborhoodID:Int): Unit ={
    neighborhoodHeap.delete(neighborhoodID)
    tabu(neighborhoodID) = it + tabuLength
    tabuNeighborhoods.insert(neighborhoodID)
  }

  /**
    * the method that returns a move from the neighborhood.
    * The returned move should typically be accepted by the acceptance criterion over the objective function.
    * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
    *
    * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.core.objective.Objective]] there is an implicit conversion available
    * @param acceptanceCriterion
    * @return
    */
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    if((it > 0) && ((it % refresh) == 0) && neighborhoodArray.exists(_.nbFound!=0)){

      if(printExploredNeighborhoods){
        println("refreshing knowledge on neighborhood; statistics since last refresh: ")
        printStatus()
      }
      for(p <- neighborhoodArray.indices){
        neighborhoodArray(p).resetThisStatistics()
        if(tabu(p) <= it) updateNeighborhodPerformances(p)
      }
    }
    updateTabu()
    while(!neighborhoodHeap.isEmpty){
      val headID = neighborhoodHeap.getFirst
      val headNeighborhood = neighborhoodArray(headID)
      headNeighborhood.getMove(obj,initialObj, acceptanceCriterion) match{
        case NoMoveFound =>
          makeTabu(headID)
        case MoveFound(m) =>
          neighborhoodHeap.notifyChange(headID)
          return MoveFound(m)
      }
    }

    //ok, we try again with tabu, overriding tabu as allowed
    if(tabuNeighborhoods.nonEmpty && tabu(tabuNeighborhoods.getFirst) <= it + overrideTabuOnFullExhaust){
      val newNonTabu = tabuNeighborhoods.popFirst()
      neighborhoodArray(newNonTabu).reset()
      neighborhoodHeap.insert(newNonTabu)
      it -=1
      getMove(obj,initialObj,acceptanceCriterion)
    }else{
      NoMoveFound
    }
  }

  /**
    * prints the profile info for the neighborhoods, for verbosity purposes
    */
  def printStatus(): Unit ={
    println(Profile.selectedStatisticInfo(neighborhoodArray))
  }
}


case class RestrictedNeighborhood(n:Neighborhood, minimalSpaceBetweenExplorations:Long = 0)

case class BestSlopeFirstWithRestrictions(l:List[RestrictedNeighborhood],
                                          tabuLength:Long = 10,
                                          overrideTabuOnFullExhaust:Long = 9,
                                          refresh:Long = 100)
  extends BestNeighborhoodFirstWithRestrictions(l, tabuLength, overrideTabuOnFullExhaust, refresh){
  override protected def bestKey(p:Profile):Long = -p.slopeForCombinators()
}


abstract class BestNeighborhoodFirstWithRestrictions(l:List[RestrictedNeighborhood],
                                                     tabuLength:Long,
                                                     overrideTabuOnFullExhaust:Long,
                                                     refresh:Long)
  extends NeighborhoodCombinator(l.map(_.n):_*) {

  protected var it:Long = 0
  protected def bestKey(p:Profile):Long

  protected val neighborhoodArray: Array[(Profile,Long)] = l.map(r => (Profile(r.n),r.minimalSpaceBetweenExplorations)).toArray
  protected val tabu:Array[Long] = Array.fill(neighborhoodArray.length)(0L)
  protected val tabuExhaustedNeighborhoods = new BinomialHeap[Int](tabu(_),tabu.length)
  protected val tabuRestrictedNeighborhoods = new BinomialHeap[Int](tabu(_),tabu.length)

  protected val neighborhoodHeap = new BinomialHeapWithMove[Int]((neighborhoodID:Int) =>
    bestKey(neighborhoodArray(neighborhoodID)._1), neighborhoodArray.length)

  neighborhoodArray.indices.foreach((i : Int) => neighborhoodHeap.insert(i))

  private def getBestNeighborhoodID:Long = neighborhoodHeap.getFirst

  private def updateNeighborhodPerformances(neighborhoodID:Int): Unit ={
    neighborhoodHeap.notifyChange(neighborhoodID)
  }

  private def updateTabus(): Unit ={
    it +=1L

    while(tabuExhaustedNeighborhoods.nonEmpty && tabu(tabuExhaustedNeighborhoods.getFirst) <= it){
      val newNonTabu = tabuExhaustedNeighborhoods.popFirst()
      neighborhoodArray(newNonTabu)._1.reset()
      neighborhoodHeap.insert(newNonTabu)
    }
    while(tabuRestrictedNeighborhoods.nonEmpty && tabu(tabuRestrictedNeighborhoods.getFirst) <= it){
      val newNonTabu = tabuRestrictedNeighborhoods.popFirst()
      //there is no reset here because this tabu is normally for too efficient neighborhoods that must be slowed down
      neighborhoodHeap.insert(newNonTabu)
    }
  }

  protected def makeTabuExhausted(neighborhoodID:Int): Unit ={
    neighborhoodHeap.delete(neighborhoodID)
    tabu(neighborhoodID) = it + tabuLength
    tabuExhaustedNeighborhoods.insert(neighborhoodID)
  }

  protected def makeTabuRestricted(neighborhoodID:Int): Unit ={
    neighborhoodHeap.delete(neighborhoodID)
    tabu(neighborhoodID) = it + neighborhoodArray(neighborhoodID)._2
    tabuExhaustedNeighborhoods.insert(neighborhoodID)
  }

  /**
    * the method that returns a move from the neighborhood.
    * The returned move should typically be accepted by the acceptance criterion over the objective function.
    * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
    *
    * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.core.objective.Objective]] there is an implicit conversion available
    * @param acceptanceCriterion
    * @return
    */
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    if((it > 0L) && ((it % refresh) == 0L)){

      if(printExploredNeighborhoods){
        println("refreshing knowledge on neighborhood; statistics since last refresh: ")
        printStatus()
      }
      for(p <- neighborhoodArray.indices){
        neighborhoodArray(p)._1.resetThisStatistics()
        if(tabu(p) <= it) updateNeighborhodPerformances(p)
      }
    }

    updateTabus()

    while(!neighborhoodHeap.isEmpty){
      val headID = neighborhoodHeap.getFirst
      val headNeighborhood = neighborhoodArray(headID)
      headNeighborhood._1.getMove(obj,initialObj, acceptanceCriterion) match{
        case NoMoveFound =>
          makeTabuExhausted(headID)
        case MoveFound(m) =>
          if(headNeighborhood._2 != 0L){
            makeTabuRestricted(headID)
          }else{
            neighborhoodHeap.notifyChange(headID)
          }
          return MoveFound(m)
      }
    }

    //ok, we try again, and pop tabu restricted neighborhoods first
    if(tabuRestrictedNeighborhoods.nonEmpty){
      val newNonTabu = tabuRestrictedNeighborhoods.popFirst()
      neighborhoodArray(newNonTabu)._1.reset()
      neighborhoodHeap.insert(newNonTabu)
      return getMove(obj,initialObj,acceptanceCriterion)
    }

    //ok, we try again with tabu, overriding tabu exhausted as allowed
    if(tabuExhaustedNeighborhoods.nonEmpty && tabu(tabuExhaustedNeighborhoods.getFirst) <= it + overrideTabuOnFullExhaust){
      val newNonTabu = tabuExhaustedNeighborhoods.popFirst()
      neighborhoodArray(newNonTabu)._1.reset()
      neighborhoodHeap.insert(newNonTabu)
      it -=1L
      getMove(obj,initialObj,acceptanceCriterion)
    }else{
      NoMoveFound
    }
  }

  /**
    * prints the profile info for the neighborhoods, for verbosity purposes
    */
  def printStatus(): Unit ={
    println(Profile.selectedStatisticInfo(neighborhoodArray.map(_._1)))
  }
}


/**
  * performs a round robin on the neighborhood.
  * It proceeds to the next one after "step" invocations or if the explored one is exhausted
  *
  * @author renaud.delandtsheer@cetic.be
  */
class RoundRobin(l: List[Neighborhood], steps: Long = 1L)
  extends NeighborhoodCombinator(l: _*) {
  val robins = l.length
  var remainingSteps: Long = steps
  var tail: List[Neighborhood] = l
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult =
    myGetImprovingMove(obj, initialObj, acceptanceCriteria)

  private def myGetImprovingMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean, triedRobins: Long = 0L): SearchResult = {
    if (triedRobins >= robins) {
      NoMoveFound
    } else if (remainingSteps > 0L) {
      //no need to change neighborhood yet
      remainingSteps -= 1L
      tail.head.getMove(obj, initialObj:Long, acceptanceCriteria) match {
        case NoMoveFound =>
          tail.head.reset()
          moveToNextRobin()
          myGetImprovingMove(obj, initialObj, acceptanceCriteria, triedRobins + 1L)
        case x: MoveFound => x
      }
    } else {
      //move to next robin
      moveToNextRobin()
      myGetImprovingMove(obj, initialObj, acceptanceCriteria, triedRobins)
    }
  }

  private def moveToNextRobin(): Unit ={
    if (tail.tail.isEmpty) {
      tail = l
    } else {
      tail = tail.tail
    }
    remainingSteps = steps
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit ={
    remainingSteps = steps
    tail = l
    super.reset()
  }

  /**
    * proposes a round-robin with that.
    * notice that you can chain steps; this will build a round-robin on the whole sequence (although this operation is not associative)
    *
    * @param b
    * @return
    */
  def step(b: Neighborhood): RoundRobin = new RoundRobin(l ::: List(b))

  def repeat(i: Int): RoundRobin = {
    val last = l.last
    new RoundRobin(l ::: List.fill(i - 1)(last))
  }
}

class RoundRobinNoParam(val a: Neighborhood, val b: Neighborhood) {
  def step(s: Long): Neighborhood = new RoundRobin(List(a, b), s)
}

/**
  * this combinator randomly tries one neighborhood.
  * it tries the other if the first did not find any move
  *
  * @param a a neighborhood
  * @author renaud.delandtsheer@cetic.be
  */
class RandomCombinator(a: Neighborhood*) extends NeighborhoodCombinator(a:_*) {
  private val r = new scala.util.Random()

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    val neighborhoods = r.shuffle(a).iterator
    while (neighborhoods.hasNext) {
      val current = neighborhoods.next
      current.getMove(obj, initialObj, acceptanceCriteria) match {
        case m: MoveFound => return m
        case _ => ;
      }
    }
    NoMoveFound
  }
}

/**
  * this combinator randomly tries one neighborhood.
  * it tries the other if the first did not find any move
  *
  * @param a a neighborhood
  * @author renaud.delandtsheer@cetic.be
  */
class BiasedRandom(a: (Neighborhood,Double)*)(noRetryOnExhaust:Boolean = false) extends NeighborhoodCombinator(a.map(_._1):_*) {
  require(a.nonEmpty)

  abstract sealed class Node(val weight:Double)
  case class MiddleNode(l:Node,r:Node) extends Node(l.weight + r.weight)
  case class TerminalNode(override val weight:Double, n:Neighborhood) extends Node(weight)

  private val r = new scala.util.Random()

  def reduce(l:List[Node]):List[Node] = {
    l match{
      case h1 :: h2 :: t => MiddleNode(h1,h2) :: reduce(t)
      case List(_) => l
      case nil => nil
    }
  }

  def fixPoint[A](init:A, function:A => A, fixpointReached:A => Boolean):A={
    var current = init
    while(!fixpointReached(current)){
      current = function(current)
    }
    current
  }

  val initialNeighborhoodTree:Node =  fixPoint(
    a.toList.map(nw => TerminalNode(nw._2,nw._1)),
    reduce,
    (_:List[Node]) match{case List(_) => true; case _ => false}
  ).head

  def findAndRemove(n:Node,p:Double):(Option[Node],Neighborhood) = {
    n match{
      case TerminalNode(_,nt) => (None,nt)
      case MiddleNode(l,rm) =>
        val ((newNode,found),other) = if (p <= l.weight) (findAndRemove(l,p),rm) else (findAndRemove(rm,p-l.weight),l)
        newNode match{
          case None => (Some(other),found)
          case Some(pn) => (Some(MiddleNode(pn,other)),found)
        }
    }
  }

  var neighborhoodWithExhaustedRemoved:Option[Node] = Some(initialNeighborhoodTree)

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    var remainingNeighborhoods:Option[Node] = neighborhoodWithExhaustedRemoved
    while (true) {
      remainingNeighborhoods match{
        case None => return NoMoveFound
        case Some(node) =>
          val (newHead,selectedNeighborhood) = findAndRemove(node,r.nextFloat()*node.weight)
          remainingNeighborhoods = newHead
          selectedNeighborhood.getMove(obj, initialObj, acceptanceCriteria) match {
            case m: MoveFound => return m
            case _ =>
              if(noRetryOnExhaust) neighborhoodWithExhaustedRemoved = newHead
          }
      }
    }
    NoMoveFound
  }
}

/**
  * @param l
  * @param weightUpdate a function that updates the weight of a neighborhood. if the function returns a negative number, the neighborhood gets the average weight thatthe other received.
  * @param updateEveryXCalls
  */
@deprecated("use the bestSlopeFirst instead","")
class LearningRandom(l:List[Neighborhood],
                     weightUpdate:(Profile,Double) => Double =
                     (stat,oldWeight) => { if (stat.nbCalls == 0L) -1L
                     else {
                       val toReturn =  (stat.slopeOrZero + oldWeight)/2L
                       stat.resetStatistics()
                       toReturn
                     }
                     },
                     updateEveryXCalls:Long = 10L)
  extends NeighborhoodCombinator(l:_*){

  val instrumentedNeighborhood:List[Profile] = l.map(Profile(_))
  var weightedInstrumentedNeighborhoods:List[(Profile,Double)] = instrumentedNeighborhood.map((_,1.0))
  var currentRandom = new BiasedRandom(weightedInstrumentedNeighborhoods:_*)()
  var stepsBeforeUpdate = updateEveryXCalls

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    if(stepsBeforeUpdate <= 0L){
      val newlyWeightedNeighborhoods = weightedInstrumentedNeighborhoods.map(sd => (sd._1,weightUpdate(sd._1,sd._2)))
      val (totalWeightNonNegative,nonNegativeCount) = newlyWeightedNeighborhoods.foldLeft((0.0,0L))((a:(Double,Long),b:(Profile,Double)) => if(b._2 < 0L) a else (a._1 + b._2,a._2+1L))
      val defaultWeight = totalWeightNonNegative / nonNegativeCount
      weightedInstrumentedNeighborhoods = newlyWeightedNeighborhoods.map(sw => (sw._1,if (sw._2 < 0L) defaultWeight else sw._2))
      currentRandom = new BiasedRandom(weightedInstrumentedNeighborhoods :_*)()
      stepsBeforeUpdate = updateEveryXCalls
      if(printExploredNeighborhoods) println("LearningRandom: weights updated: " + weightedInstrumentedNeighborhoods )
    }
    stepsBeforeUpdate -=1L
    currentRandom.getMove(obj,initialObj,acceptanceCriterion)
  }
}


/**
  * this combinator is stateful.
  * it returns the result of the first Neighborhood until it returns NoMoveFound.
  * It then switches to the other Neighborhood,
  * but only if a move was found by the first neighborhood
  * it does not come back to the first one after the second one is exhausted
  *
  * @author renaud.delandtsheer@cetic.be
  */
class ExhaustAndContinueIfMovesFound(a: Neighborhood, b: Neighborhood) extends NeighborhoodCombinator(a, b) {
  var currentIsA = true
  var movesFoundWithCurrent = false
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    @tailrec
    def search(): SearchResult = {
      val current = if (currentIsA) a else b
      current.getMove(obj, initialObj:Long, acceptanceCriteria) match {
        case NoMoveFound =>
          if (currentIsA) {
            currentIsA = false
            movesFoundWithCurrent = false
            search()
          } else NoMoveFound
        case x: MoveFound =>
          movesFoundWithCurrent = true
          x
      }
    }
    search()
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit ={
    currentIsA = true
    movesFoundWithCurrent = false
    super.reset()
  }
}
