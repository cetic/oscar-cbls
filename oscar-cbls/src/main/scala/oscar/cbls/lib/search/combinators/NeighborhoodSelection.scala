package oscar.cbls.lib.search.combinators

import oscar.cbls.algo.heap.{BinomialHeap, BinomialHeapWithMove}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search._

import scala.annotation.tailrec

abstract class BestNeighborhoodFirst(l:List[Neighborhood],
                                     tabuLength:Int,
                                     overrideTabuOnFullExhaust:Long,
                                     refresh:Int)
  extends NeighborhoodCombinator(l:_*) {
  require(overrideTabuOnFullExhaust < tabuLength, "overrideTabuOnFullExhaust should be < tabuLength")

  protected var it:Int = 0
  protected def bestKey(neighborhoodId:Int):Long
  override val profiler =BestFirstProfiler(this,l)
  override def collectProfilingStatistics: List[Array[String]] =
    profiler.collectThisProfileStatistics ::: super.collectProfilingStatistics

  protected val neighborhoodArray: Array[Neighborhood] = l.toArray
  protected val tabu:Array[Int] = Array.fill(neighborhoodArray.length)(0)
  protected var tabuNeighborhoods = new BinomialHeap[Int](tabu(_),tabu.length)

  protected val neighborhoodHeap = new BinomialHeapWithMove[Int]((neighborhoodID:Int) => bestKey(neighborhoodID), neighborhoodArray.length)
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
  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult = {
    profiler.explorationStarted()
    if ((it > 0) && ((it % refresh) == 0) && neighborhoodArray.exists(_.nbFound!=0)) {
      if (printExploredNeighborhoods) {
        println("refreshing knowledge on neighborhood; statistics since last refresh: ")
        printStatus()
      }
      profiler.resetSelectionNeighborhoodStatistics()
      for(p <- neighborhoodArray.indices){
        if(tabu(p) <= it) updateNeighborhodPerformances(p)
      }
    }
    updateTabu()
    while(!neighborhoodHeap.isEmpty){
      val headID = neighborhoodHeap.getFirst
      val headNeighborhood = neighborhoodArray(headID)
      headNeighborhood.getMove(obj,initialObj, acceptanceCriterion) match{
        case NoMoveFound =>
          if (neighborhoodHeap.size == l.size) profiler.firstFailed()
          makeTabu(headID)
        case MoveFound(m) =>
          neighborhoodHeap.notifyChange(headID)
          profiler.explorationEnded(true)
          return MoveFound(m)
      }
    }

    //ok, we try again with tabu, overriding tabu as allowed
    if (tabuNeighborhoods.nonEmpty && tabu(tabuNeighborhoods.getFirst) <= it + overrideTabuOnFullExhaust) {
      val newNonTabu = tabuNeighborhoods.popFirst()
      neighborhoodArray(newNonTabu).reset()
      neighborhoodHeap.insert(newNonTabu)
      it -=1
      getMove(obj,initialObj,acceptanceCriterion)
    } else {
      profiler.explorationEnded(false)
      NoMoveFound
    }
  }

  /**
   * prints the profile info for the neighborhoods, for verbosity purposes
   */
  def printStatus(): Unit = {
    println(Profiler.selectedStatisticInfo(neighborhoodArray.map(_.profiler)))
  }
}

/**
 * At each invocation, this combinator explores one of the neighborhoods in l (and repeat if it is exhausted)
 * neighborhoods are selected based on their slope. the slope is the total gain in objective function performed by the neighborhood, divided by the total amount of time spend exploring the neighborhood.
 * a tabu is added: in case a neighborhood is exhausted, it is not explored for a number of exploration of this combinator
 * the tabu can be overriden if all neighborhoods explored are exhausted. tabu neighborhood can be explored anyway if they are still tabu, but for less than overrideTabuOnFullExhaust invocations of this combinator
 * the refresh parameter forces the combinator to try all neighborhoods every "refresh" invocation. it is useful because some neighorhood can perform poorly at the beginning of search and much better later on, and we do not want the combinator to just "stick to its first impression"
 *
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
  override protected def bestKey(neighborhoodId:Int):Long = -profiler.slopeForCombinators(neighborhoodId)
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
  extends BestNeighborhoodFirst(l, tabuLength, overrideTabuOnFullExhaust, refresh) {
  override protected def bestKey(neighborhoodId:Int):Long = {
    if (profiler.nbFoundSubN(neighborhoodId) == 0L)
      if (profiler.totalTimeSpentMoveFoundSubN(neighborhoodId) == 0L) 0L else -Long.MaxValue
    else
      - (profiler.totalTimeSpentMoveFoundSubN(neighborhoodId) / profiler.nbFoundSubN(neighborhoodId)).toInt
  }
}

/**
 * performs a round robin on the neighborhood.
 * It proceeds to the next one after "step" invocations or if the explored one is exhausted
 *
 * @author renaud.delandtsheer@cetic.be
 */
class RoundRobin(robins: Array[(Neighborhood,Int)], tabu:Int = 1)
  extends NeighborhoodCombinator(robins.map(_._1).toIndexedSeq:_*) {
  private var currentRobin = 0
  private var nbExplorationsOnCurrentRobin:Int = 0
  private var firstFailedRobinInRow:Int = -1

  var currentCycleNr = 0
  private val cycleOfLastFail:Array[Int] = Array.fill(robins.length)(Int.MinValue)

  override val profiler: SelectionProfiler = new SelectionProfiler(this, robins.map(_._1).toList)

  override def getMove(obj: Objective,
                       initialObj:Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    profiler.explorationStarted()
    while(true){
      //select next robin
      val prevRobin = currentRobin
      var nextNeighborFound = false
      var overrideTabu:Boolean = false
      if (nbExplorationsOnCurrentRobin >= robins(currentRobin)._2) {
        //move to next robin
        currentRobin += 1
        if (currentRobin == robins.length) {
          currentRobin = 0
          currentCycleNr += 1
        }
        nbExplorationsOnCurrentRobin = 0
      }
      while(! nextNeighborFound) {
        //check that we have not circled around whole set of robins
        if (currentRobin == firstFailedRobinInRow) {
          profiler.explorationEnded(false)
          return NoMoveFound
        }
        if(overrideTabu || (cycleOfLastFail(currentRobin) + tabu < currentCycleNr)) {
          nextNeighborFound = true
        }else{
          currentRobin += 1
          if (currentRobin == robins.length) {
            currentRobin = 0
            currentCycleNr += 1
          }
          nbExplorationsOnCurrentRobin = 0
          if(prevRobin == currentRobin) overrideTabu = true
        }
      }

      //explore current robin
      //profiler.subExplorationStarted(currentRobin)
      robins(currentRobin)._1.getMove(obj, initialObj:Long, acceptanceCriteria) match {
        case NoMoveFound =>
          //profiler.subExplorationEnded(currentRobin, None)
          if(firstFailedRobinInRow == -1) firstFailedRobinInRow = currentRobin
          nbExplorationsOnCurrentRobin = robins(currentRobin)._2
          cycleOfLastFail(currentRobin) = currentCycleNr
        //iterate, simply
        case x: MoveFound =>
          profiler.explorationEnded(true)
          //profiler.subExplorationEnded(currentRobin,Some(initialObj - x.objAfter))
          firstFailedRobinInRow = -1
          nbExplorationsOnCurrentRobin += 1
          return x
      }
    }
    throw new Error("should not be reached")
  }

  //this resets the internal state of the move combinators
  override def reset(): Unit ={
    currentRobin = 0
    nbExplorationsOnCurrentRobin = 0
    firstFailedRobinInRow = -1
    for (i <- cycleOfLastFail.indices) {
      cycleOfLastFail(i) = Int.MinValue
    }
    super.reset()
  }
}

class RoundRobinNoParam(val a: Neighborhood, val b: Neighborhood) {
  def step(s: Long): Neighborhood = new RoundRobin(Array((a,1), (b,1)),0)
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

  override val profiler: SelectionProfiler = new SelectionProfiler(this,a.toList)

  override def getMove(obj: Objective,
                       initialObj:Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    profiler.explorationStarted()
    val neighborhoods = a.toList
    val neighborhoodsIterator = r.shuffle(neighborhoods).iterator
    while (neighborhoodsIterator.hasNext) {
      val current = neighborhoodsIterator.next()
      current.getMove(obj, initialObj, acceptanceCriteria) match {
        case m: MoveFound =>
          profiler.explorationEnded(true)
          return m
        case _ =>
      }
    }
    profiler.explorationEnded(false)
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
class BiasedRandom(a: (Neighborhood,Double)*)
                  (noRetryOnExhaust:Boolean = false)
  extends NeighborhoodCombinator(a.map(_._1):_*) {
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

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult = {
    var remainingNeighborhoods:Option[Node] = neighborhoodWithExhaustedRemoved
    while (true) {
      remainingNeighborhoods match{
        case None => return NoMoveFound
        case Some(node) =>
          val (newHead,selectedNeighborhood) = findAndRemove(node,r.nextFloat()*node.weight)
          remainingNeighborhoods = newHead
          selectedNeighborhood.getMove(obj, initialObj, acceptanceCriterion) match {
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

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult = {
    @tailrec
    def search(): SearchResult = {
      val current = if (currentIsA) a else b
      current.getMove(obj, initialObj:Long, acceptanceCriterion) match {
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
