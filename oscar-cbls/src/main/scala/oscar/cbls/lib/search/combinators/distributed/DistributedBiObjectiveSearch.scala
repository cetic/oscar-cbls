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

package oscar.cbls.lib.search.combinators.distributed

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.util.Timeout
import oscar.cbls.{Objective, Solution}
import oscar.cbls.algo.dll.{DLLStorageElement, DoublyLinkedList}
import oscar.cbls.algo.heap.BinomialHeapWithMove
import oscar.cbls.core.computation.{Solution, Store}
import oscar.cbls.core.distrib._
import oscar.cbls.core.objective.{CascadingObjective, IndependentObjective}
import oscar.cbls.core.search.{DistributedCombinator, Neighborhood, NoMoveFound, SearchResult}
import oscar.cbls.lib.search.combinators.multiObjective.PlotPareto
import oscar.cbls.visual.SingleFrameWindow

import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap, TreeSet}
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.{Failure, Success}

class ParetoPointSearcher(taskId:Int,
                          minObj1WithOBj2BoundNeighborhood:Neighborhood,
                          minObj2WithFoundObj1BoundNeighborhood:Neighborhood)
  extends RemoteTask(taskId,"ParetoPointSearcher"){

  override def abort(): Unit = { } //there is no abort

  def doTask(taskMessage1:SearchRequest,model:Store,currentSolOpt:Option[(Solution,Int)]):(Solution,Int) = {
    val (startSol,solId):(Solution,Int) = loadSolution(taskMessage1.startSolutionOpt,model,currentSolOpt)

    val taskMessage = taskMessage1.asInstanceOf[OptimizeWithBoundRequest]

    val startTime = System.currentTimeMillis()
    val obj1 = taskMessage.obj1.convertToObjective(model)
    val obj2 = taskMessage.obj2.convertToObjective(model)

    println("obj2.value: " + obj2.value)
    println("taskMessage.maxValueForObj2:" + taskMessage.maxValueForObj2)
    val minObj1WithOBj2Bound =
      CascadingObjective(
        () => (0L max (obj2.value - taskMessage.maxValueForObj2)),
        obj1)
    //this ensures that metaheuristics starts from scratch properly
    minObj1WithOBj2BoundNeighborhood.reset()
    minObj1WithOBj2BoundNeighborhood.doAllMoves(obj = minObj1WithOBj2Bound)

    val foundOBj1 = obj1.value
    val foundOBj2 = obj2.value

    //this ensures that metaheuristics starts from scratch properly
    minObj2WithFoundObj1BoundNeighborhood.reset()
    val minObj2WithFoundObj1 =
      CascadingObjective(
        () => (0L max (obj1.value - foundOBj1)),
        obj2)
    minObj2WithFoundObj1BoundNeighborhood.doAllMoves(obj = minObj2WithFoundObj1)

    val dur = System.currentTimeMillis() - startTime

    println("finished")
    println("obj1.value:" + obj1.value)
    println("obj2.value:" + obj2.value)

    taskMessage.sendResultTo!SearchCompleted(
      taskMessage.uniqueSearchId,
      (obj1.value, obj2.value, IndependentSolution(model.solution()),taskMessage.maxValueForObj2),
      dur.toInt)

    startSol.restoreDecisionVariables()
    (startSol,solId)
  }
}

//there is no uniqueID here because we will not cancel tasks
case class OptimizeWithBoundRequest(override val remoteTaskId:RemoteTaskIdentification,
                                    obj1: IndependentObjective,
                                    obj2: IndependentObjective,
                                    maxValueForObj2:Long, //only this one is considered, the other are informative and traceability stuff
                                    startSolution: Option[IndependentSolution],
                                    override val sendResultTo: ActorRef[SearchEnded[(Long,Long,IndependentSolution,Long)]]
                                   ) extends SearchRequest(-1,remoteTaskId,sendResultTo){

  override def startSolutionOpt: Option[IndependentSolution] = startSolution //we are not interested by hotRestart

  override def dropStartSolution: SearchRequest = {
    println("dropping startsol")
    this.copy(startSolution = None)
  }

  override def neighborhoodIdOpt: Option[Int] = Some(remoteTaskId.taskId)

  override def toString: String = s"OptimizeWithBoundRequest(maxValueForObj2:$maxValueForObj2)"
}

/**
 * This is an implementation of the rectangle splitting heuristics for heuristic optimization presented in
 *     Matl, Piotr & Hartl, Richard & Vidal, Thibaut. (2017).
 *     Heuristic Rectangle Splitting: Leveraging Single-Objective Heuristics to Efficiently Solve Multi-Objective Problems.
 *
 * @param globalMaxObj1 the max value of objective 1
 * @param globalMinObj2 the min value of objective 2
 * @param solutionAtMax1Min2 the solution (max1,min2)
 * @param optimize a function that performs the optimization
 *                 it input a max value for obj2, and an initial solution.
 *                 This initial solution has obj2 < max2, and can be used as initial solution to perform the search.
 *                 It is expected to return a triplet with obj1,obj2 and the solution corresponding to these values.
 *                 In case the search cannot produce a suitable solution it returns None.
 *                 Notice that the absence of solution is unlikely
 *                 because the initial solution is actually acceptable, but will be filtered out by the Pareto filtering.
 * @param stopSurface this is a stop criterion based on the heuristics.
 *                    At any time, the remaining surface if the sum of all surface that must be explored.
 *                    If < stopSurface, the search is interrupted. to deactivate, set to zero.
 * @param maxPoints the max number of points that is searched. the search stops as soon as
 * @param verbose if true, prints the outcome of eery search
 *                outcome of search: storing new Pareto point, new point is dominated, removed dominated point
 *                metrics: remaining surface and number of points
 *                notice that the optimize method can also print verbose message; up to you)
 * @param visu true to display the Pareto front in real time
 * @param visuTitle the title to use for the visu
 * @param obj1Name the name of obj1, used for verbosities on the console and for the visu
 * @param obj2Name the name of obj2, used for verbosities on the console and for the visu
 * @param filterSquare an additional method that you can specify to filter away some squares of the search,
 *                     typically when you want to trade time for granularity of the Pareto front.
 */
class DistributedBiObjectiveSearch(minObj1Neighborhood:Neighborhood,
                                   minObj2Neighborhood:Option[Neighborhood] = None,
                                   obj1:Objective,
                                   obj2:Objective,
                                   stopSurface:Long = 0,
                                   maxPoints:Int = 200,
                                   verbose:Boolean = false,
                                   visu:Boolean = false,
                                   visuTitle: String = "Pareto",
                                   obj1Name: String = "obj1",
                                   obj2Name: String = "obj2",
                                   filterSquare:(Long, Long, Long, Long) => Boolean = (_:Long, _:Long, _:Long, _:Long) => true,
                                   stayAlive:Boolean = false,
                                   setMaxWorkers:Option[Int] = None
                                  ) extends DistributedCombinator(
  Array(),
  Array((taskId:Int) =>  new ParetoPointSearcher(
    taskId,
    minObj1WithOBj2BoundNeighborhood = minObj1Neighborhood,
    minObj2WithFoundObj1BoundNeighborhood = minObj2Neighborhood.getOrElse(minObj1Neighborhood)))) {

  val (plot, window) = if (visu) {
    val p = new PlotPareto(null, obj1Name, obj2Name)
    val window = SingleFrameWindow.show(p, visuTitle, width = 2000, height = 2000)
    (p, window)
  } else (null, null)

  //a square, anchored at a solution
  //the solution is the upper left corner
  class SortedSquare(val obj1:Long)
  case class Square(override val obj1: Long, obj2: Long,
                    maxObj1: Long, minObj2: Long,
                    solution: Solution,
                    independentSolution:IndependentSolution) extends SortedSquare(obj1){

    require(obj1 <= maxObj1)
    require(obj2 >= minObj2)
    // not requiring anything on the solution itself? like solution = (obj1, obj2)?

    def surface: Long = (maxObj1 - obj1) * (obj2 - minObj2)

    override def toString: String = s"Square(obj1:$obj1,obj2:$obj2,maxObj1:$maxObj1,minObj2:$minObj2,surf:" + surface + ")"
  }

  implicit val OrderingByObj1: Ordering[SortedSquare] = new Ordering[SortedSquare] {
    def compare(a: SortedSquare, b: SortedSquare): Int = a.obj1 compare b.obj1
  }
  implicit val Ordering2ByObj1: Ordering[Square] = new Ordering[Square] {
    def compare(a: Square, b: Square): Int = a.obj1 compare b.obj1
  }
  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  var dominatedSolutions: List[(Long, Long)] = Nil
  var remainingSurface: Long = 0 //equal to the surface in the pareto front
  val squaresToDevelopBiggestSquareFirst = new BinomialHeapWithMove[Square](getKey = -_.surface, (Int.MaxValue.toLong min (maxPoints.toLong * 2)).toInt)
  var paretoFront: TreeSet[SortedSquare] = new TreeSet()(OrderingByObj1)
  val store = obj1.model

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def redrawPareto(): Unit = {
    if (plot != null) {
      plot.reDrawPareto(
        paretoFront.toList.map({case square:Square => (square.obj1, square.obj2)}),
        Some(dominatedSolutions))
    }
  }

  def isNewSquareDominated(obj1:Long,obj2:Long): Boolean = {
    paretoFront.maxBefore(new SortedSquare(obj1)) match {
      case Some(potentialBetterSquare:Square)
        if potentialBetterSquare.obj2 <= obj2 && potentialBetterSquare.obj1 < obj1 => true
      case Some(potentialBetterSquare:Square)
        if potentialBetterSquare.obj2 < obj2 && potentialBetterSquare.obj1 <= obj1 => true
      case _ => false
    }
  }

  def removeDominatedSquare(square: Square): Unit = {
    paretoFront = paretoFront.excl(square)

    if(squaresToDevelopBiggestSquareFirst.deleteIfPresent(square)){
      remainingSurface -= square.surface
    }
    dominatedSolutions = (square.obj1, square.obj2) :: dominatedSolutions
  }

  def replaceSquareAndSchedule(oldSquare: Square, newSquare: Square): Unit = {
    paretoFront = paretoFront.excl(oldSquare).incl(newSquare)

    if(squaresToDevelopBiggestSquareFirst.deleteIfPresent(oldSquare)){
      remainingSurface -= oldSquare.surface
    }

    if(newSquare.surface !=0) {
      squaresToDevelopBiggestSquareFirst.insert(newSquare)
      remainingSurface += newSquare.surface
    }
  }

  def storeAndScheduleSquare(newSquare: Square): Unit = {
    paretoFront = paretoFront.incl(newSquare)
    if(newSquare.surface !=0) {
      squaresToDevelopBiggestSquareFirst.insert(newSquare)
      remainingSurface += newSquare.surface
    }
  }

  // /////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  def paretoOptimize(): List[(Long, Long, Solution)] = {

    val startSearchNanotime = System.nanoTime()

    if (verbose) println("BiObjectiveSearch: search first solution: minObj2")
    val rightMostSquare = {
      val neighborhoodForFistSolution = minObj2Neighborhood.getOrElse(minObj1Neighborhood)
      neighborhoodForFistSolution.doAllMoves(obj = obj2)

      val solutionAtMin2: Solution = obj2.model.solution()

      //Store initial given solution
      val square = Square(
        obj1 = obj1.value, obj2 = obj2.value,
        maxObj1 = obj1.value, minObj2 = obj2.value,
        solutionAtMin2,
        IndependentSolution(solutionAtMin2))

      storeAndScheduleSquare(square)
      square
    }

    val leftMostSquare = {
      val neighborhoodForFistSolution = minObj1Neighborhood
      neighborhoodForFistSolution.doAllMoves(obj = obj1)

      val solutionAtMin1: Solution = obj2.model.solution()

      //Store initial given solution
      val square = Square(
        obj1 = obj1.value, obj2 = obj2.value,
        maxObj1 = rightMostSquare.obj1-1, minObj2 = rightMostSquare.obj2+1,
        solutionAtMin1,
        IndependentSolution(solutionAtMin1))

      storeAndScheduleSquare(square)
      square
    }


    def frontStr:String = {
      paretoFront.toList.map(_.toString).mkString("\n\t")
    }
    println("squares:\n\t" + frontStr)


    if (verbose) println("BiObjectiveSearch: Start front exploration")

    def printStopCriterion(): Unit = {
      if (verbose) {
        println(s"stopCriterion(surface:$remainingSurface/$stopSurface nonDominatedSolutions:${paretoFront.size}/$maxPoints toDevelop:${squaresToDevelopBiggestSquareFirst.size})")
      }
    }

    def shouldStop:Boolean = {
      remainingSurface < stopSurface || paretoFront.size >= maxPoints
    }

    abstract class WrappedData
    case class WrappedSearchEnded(searchEnded: SearchEnded[(Long, Long, IndependentSolution,Long)], //obj1,obj2,sol,maxValueForObj2
                                  initSquare: Square //the one before the split, so we are on the left
                                 ) extends WrappedData
    case class WrappedCompleted() extends WrappedData
    case class WrappedError(msg:Option[String] = None,crash:Option[SearchCrashed] = None) extends WrappedData

    val resultPromise = Promise[WrappedData]()
    val futureResult: Future[WrappedData] = resultPromise.future

    val maxWorkers = setMaxWorkers match {
      case Some(m) => m
      case None => supervisor.nbWorkers //TODO: this is not great because workers can enroll throughout the search; we should be able to scale up when more workers arrive
    }
    implicit val system: ActorSystem[_] = supervisor.system
    implicit val timeout: Timeout = 3.seconds

    //the search is performed in a separated actor while this tread is waiting on a future
    //we wait on futureResult for the final answer
    supervisor.spawnNewActor(Behaviors.setup { context: ActorContext[WrappedData] => {
      if (verbose) context.log.info(s"start search")
      next(nbRunningOrStartingSearches = 0, context)
    }
    }, "DistributedBiObjective")

    def next(nbRunningOrStartingSearches: Int, context:ActorContext[WrappedData]): Behavior[WrappedData] = {
      context.log.info(s"nbRunningOrStartingSearches:$nbRunningOrStartingSearches heapSize:${squaresToDevelopBiggestSquareFirst.size} front size:${paretoFront.size} remainingSurface:$remainingSurface")
      if(nbRunningOrStartingSearches == 0 && (shouldStop || squaresToDevelopBiggestSquareFirst.isEmpty)){
        //we should stop
        context.log.info("should stop in the loop")
        resultPromise.success(WrappedCompleted())
        Behaviors.stopped
      }else if (!shouldStop && nbRunningOrStartingSearches < maxWorkers && !squaresToDevelopBiggestSquareFirst.isEmpty) {

        //split a square
        val squareToSplit:Square = squaresToDevelopBiggestSquareFirst.removeFirst()
        remainingSurface -= squareToSplit.surface

        context.ask[DelegateSearch, SearchEnded[(Long, Long, IndependentSolution,Long)]](
          supervisor.supervisorActor, ref =>  DelegateSearch(OptimizeWithBoundRequest(
            remoteTaskId = this.remoteTaskIdentification(0),
            obj1 = this.obj1.getIndependentObj,
            obj2 = this.obj2.getIndependentObj,
            maxValueForObj2 = (squareToSplit.minObj2 + squareToSplit.obj2)/2,
            startSolution = Some(paretoFront.minAfter(new SortedSquare(squareToSplit.obj1+1)).getOrElse(rightMostSquare).asInstanceOf[Square].independentSolution),
            sendResultTo = ref))) {
          case Success(ended:SearchEnded[(Long, Long, IndependentSolution,Long)]) => WrappedSearchEnded(ended, squareToSplit)
          case Failure(_) => WrappedError(msg = Some("DistributedBIObjectiveSearch timeout3"))
        }

        next(nbRunningOrStartingSearches = nbRunningOrStartingSearches + 1, context)

      } else {
        Behaviors.receive { (context, command) =>
          command match {
            case WrappedSearchEnded(searchEnded, initSquare) =>
              searchEnded match {
                case SearchCompleted(searchID: Long, (obj1, obj2, independentSolution, maxValueForObj2), durationMS) =>
                  //Ici, il faut analyser le front de Pareto correctement
                  context.log.info(s"searchCompleted: obj1:$obj1, obj2:$obj2")
                  context.log.info(s"init square: $initSquare")

                  if(isNewSquareDominated(obj1,obj2)){
                    //forget about it, but init square is pruned
                    val newInitSquare = initSquare.copy(minObj2 = initSquare.minObj2 max maxValueForObj2)
                    context.log.info(s"new square dominated, updating initSquare to:$newInitSquare")
                    replaceSquareAndSchedule(initSquare, newInitSquare)

                  }else{
                    val firstSquareOpt = paretoFront.maxBefore(new SortedSquare(obj1))
                    firstSquareOpt match{
                      case Some(firstSquare:Square) =>
                        context.log.info(s"firstSquare:" + firstSquare)
                        val newFirstSquare = firstSquare.copy(
                          maxObj1 = firstSquare.maxObj1 min (obj1-1),
                          minObj2 = firstSquare.obj2 min (firstSquare.minObj2 max (maxValueForObj2+1)))
                        replaceSquareAndSchedule(
                          firstSquare,
                          newFirstSquare)
                        context.log.info(s"first square updated to:" + newFirstSquare)

                      case _ => ;//it has disappeared since then
                    }
                    var dominatedSquareOpt = paretoFront.minAfter(new SortedSquare(obj1))
                    while(dominatedSquareOpt match{
                      case Some(dominated:Square) if dominated.minObj2 > obj2 =>
                        removeDominatedSquare(dominated)
                        dominatedSquareOpt = paretoFront.minAfter(new SortedSquare(obj1))
                        true
                      case _ => false
                    }) {}
                    //last square
                    dominatedSquareOpt match{
                      case Some(lastSquare:Square) =>
                        removeDominatedSquare(lastSquare)
                        storeAndScheduleSquare(Square(
                          obj1,
                          obj2,
                          maxObj1 = lastSquare.maxObj1,
                          minObj2 = lastSquare.minObj2,
                          solution = independentSolution.makeLocal(store),
                          independentSolution = independentSolution))
                      case _ =>
                        //we cut nothing
                        storeAndScheduleSquare(Square(
                          obj1,
                          obj2,
                          maxObj1 = obj1 max initSquare.maxObj1,
                          minObj2 = obj2 min initSquare.minObj2,
                          solution = independentSolution.makeLocal(store),
                          independentSolution = independentSolution))
                    }
                  }

                  next(nbRunningOrStartingSearches = nbRunningOrStartingSearches - 1, context)

                case SearchAborted(uniqueSearchID: Long) =>
                  //strange, we should never abort serches...
                  resultPromise.success(WrappedError(msg=Some("search aborted unexpectedly")))
                  Behaviors.stopped
                case c:SearchCrashed =>
                  //finish it all with a crash message
                  resultPromise.success(WrappedError(crash = Some(c)))
                  Behaviors.stopped
              }
            case  e:WrappedError =>
              resultPromise.success(e)
              Behaviors.stopped
          }
        }
      }
    }

    //await seems to block the actor system??
    Await.result(futureResult, Duration.Inf) match {
      case WrappedCompleted() =>

      case WrappedError(msg:Option[String],crash:Option[SearchCrashed])=>
        if(msg.isDefined){
          supervisor.shutdown()
          throw new Error(msg.get)
        }
        if(crash.isDefined){
          supervisor.throwRemoteExceptionAndShutDown(crash.get)
        }
        throw new Error("Error in DistributedRestart")
      case _ =>
        throw new Error("Unknown error in DistributedFirst")
    }

    paretoFront.toList.map({case square:Square => (square.obj1,square.obj2,square.solution)})
  }

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    throw new Error("DistributedPreto cannot be used as a regular neighborhood; use paretoOptimize method")
  }
}
