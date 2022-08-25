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

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import oscar.cbls.algo.heap.BinomialHeapWithMove
import oscar.cbls.core.computation.{Solution, Store}
import oscar.cbls.core.distrib._
import oscar.cbls.core.objective.{CascadingObjective, IndependentObjective, Objective}
import oscar.cbls.core.search.{DistributedCombinator, Neighborhood, SearchResult}
import oscar.cbls.lib.search.combinators.multiObjective.PlotPareto
import oscar.cbls.util.Properties
import oscar.cbls.visual.SingleFrameWindow

import scala.collection.immutable.TreeSet
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Future, Promise}
import scala.math.Ordered.orderingToOrdered
import scala.util.{Failure, Success}

class ParetoPointSearcher(taskId:Int,
                          minObj1WithOBj2BoundNeighborhood:() => Neighborhood,
                          minObj2WithFoundObj1BoundNeighborhood:() => Neighborhood)
  extends RemoteTask(taskId,"ParetoPointSearcher"){

  override def abort(): Unit = { } //there is no abort

  override def doTask(taskMessage1: SearchRequest, model: Store, currentSolOpt: Option[(Solution, SolutionID)], workerID: Option[String]): Option[(Solution, SolutionID)] = {

    val (startSol,solId):(Solution,Option[SolutionID]) = loadSolution(taskMessage1.startSolutionOpt,model,currentSolOpt,workerID)

    val taskMessage = taskMessage1.asInstanceOf[OptimizeWithBoundRequest]

    val startTime = System.currentTimeMillis()
    val obj1 = taskMessage.obj1.convertToObjective(model)
    val obj2 = taskMessage.obj2.convertToObjective(model)

    require(obj1.value == taskMessage.initObj1, "obj1.value:" + obj1.value + " taskMessage.initObj1:" + taskMessage.initObj1 + " taskMessage1.startSolutionOpt" + taskMessage1.startSolutionOpt)
    require(taskMessage.initObj2 == obj2.value, "taskMessage.initObj2:" + taskMessage.initObj2 + " obj2.value:" + obj2.value + " taskMessage1.startSolutionOpt" + taskMessage1.startSolutionOpt)

    //we optimize as requested
    val minObj1WithOBj2Bound =
      CascadingObjective(
        () => (0L max (obj2.value - taskMessage.maxValueForObj2)),
        obj1)

    minObj1WithOBj2BoundNeighborhood().doAllMoves(obj = minObj1WithOBj2Bound)

    val foundOBj1 = obj1.value
    val foundOBj2 = obj2.value

    //second pass, optimizingthe other objective while keeping the first constant,
    // to ensure that this is Pareto optimal
    val minObj2WithFoundObj1 =
      CascadingObjective(
        () => (0L max (obj1.value - foundOBj1)),
        obj2)
    minObj2WithFoundObj1BoundNeighborhood().doAllMoves(obj = minObj2WithFoundObj1)

    val dur = System.currentTimeMillis() - startTime

    taskMessage.sendResultTo!SearchCompleted(
      taskMessage.uniqueSearchId,
      (obj1.value, obj2.value, IndependentSolution(model.solution(), workerID),taskMessage.maxValueForObj2),
      dur.toInt)

    None
  }
}


case class OptimizeWithBoundRequest(override val remoteTaskId:RemoteTaskIdentification,
                                    obj1: IndependentObjective,
                                    obj2: IndependentObjective,
                                    maxValueForObj2:Long, //only this one is considered, the other are informative and traceability stuff
                                    startSolution: Option[IndependentSolution],
                                    initObj1:Long,
                                    initObj2:Long,
                                    override val sendResultTo: ActorRef[SearchEnded]
                                   ) extends SearchRequest(-1,remoteTaskId,sendResultTo){ //there is no uniqueID here because we will not cancel tasks

  override def startSolutionOpt: Option[IndependentSolution] = startSolution //we are not interested by hotRestart

  override def dropStartSolution: SearchRequest = {
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
 * @param filterRectangle an additional method that you can specify to filter away some rectangles of the search,
 *                     typically when you want to trade time for granularity of the Pareto front.
 */
class DistributedBiObjectiveSearch(minObj1Neighborhood:() => Neighborhood,
                                   minObj2Neighborhood:Option[() => Neighborhood] = None,
                                   obj1:Objective,
                                   obj2:Objective,
                                   stopSurface:Long = 0,
                                   maxPoints:Int = 200,
                                   verbose:Boolean = false,
                                   visu:Boolean = false,
                                   visuTitle: String = "Pareto",
                                   obj1Name: String = "obj1",
                                   obj2Name: String = "obj2",
                                   filterRectangle:(Long, Long, Long, Long) => Boolean = (_:Long, _:Long, _:Long, _:Long) => true,
                                   stayAlive:Boolean = false,
                                   setMaxWorkers:Option[Int] = None
                                  ) extends DistributedCombinator(
  Array(),
  Array((taskId:Int) =>  new ParetoPointSearcher(
    taskId,
    minObj1WithOBj2BoundNeighborhood = minObj1Neighborhood,
    minObj2WithFoundObj1BoundNeighborhood = minObj2Neighborhood.getOrElse(minObj1Neighborhood)))) {

  //a rectangle, anchored at a solution
  //the solution is the upper left corner
  class SortedRectangle(val obj1:Long)
  case class Rectangle(override val obj1: Long, obj2: Long,
                       maxObj1: Long, minObj2: Long,
                       solution: Solution,
                       independentSolution:IndependentSolution) extends SortedRectangle(obj1){

    require(obj1 <= maxObj1,s"obj1:$obj1  <= maxObj1=$maxObj1")
    require(obj2 >= minObj2,s"obj2:$obj2 >= minObj2:$minObj2")
    // not requiring anything on the solution itself? like solution = (obj1, obj2)?

    val surface: Long = (maxObj1 - obj1) * (obj2 - minObj2)
    require(surface >=0)

    override def toString: String = s"Rectangle(obj1:$obj1,obj2:$obj2,maxObj1:$maxObj1,minObj2:$minObj2,surf:" + surface + ")"

    def isSplitteable:Boolean = surface !=0 && (obj2 - minObj2) > 1
  }

  implicit val OrderingSortedRectByObj1: Ordering[SortedRectangle] = new Ordering[SortedRectangle] {
    def compare(a: SortedRectangle, b: SortedRectangle): Int = a.obj1 compare b.obj1
  }

  implicit val TotalOrderingRect: Ordering[Rectangle] = new Ordering[Rectangle] {
    def compare(a: Rectangle, b: Rectangle): Int = {
      ((a.obj1, a.obj2, a.maxObj1, a.minObj2) compare(b.obj1, b.obj2, b.maxObj1, b.minObj2))
    }
  }

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private var dominatedSolutions: List[(Long, Long)] = Nil
  private var remainingSurface: Long = 0 //equal to the surface in the pareto front
  private val rectanglesToDevelopBiggestRectangleFirst =
    new BinomialHeapWithMove[Rectangle](getKey = -_.surface, (Int.MaxValue.toLong min (maxPoints.toLong * 2)).toInt)
  private var paretoFront: TreeSet[SortedRectangle] = new TreeSet()(OrderingSortedRectByObj1)
  private val store = obj1.model

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def getDominatorRectangle(obj1:Long, obj2:Long): Option[Rectangle] = {
    paretoFront.maxBefore(new SortedRectangle(obj1)) match {
      case Some(potentialBetterRectangle:Rectangle)
        if potentialBetterRectangle.obj2 <= obj2 && potentialBetterRectangle.obj1 < obj1 => Some(potentialBetterRectangle)
      case Some(potentialBetterRectangle:Rectangle)
        if potentialBetterRectangle.obj2 < obj2 && potentialBetterRectangle.obj1 <= obj1 => Some(potentialBetterRectangle)
      case _ => None
    }
  }

  private def removeDominatedRectangle(rectangle: Rectangle): Unit = {
    paretoFront = paretoFront.excl(rectangle)
    remainingSurface -= rectangle.surface
    require(remainingSurface>=0)

    if(rectanglesToDevelopBiggestRectangleFirst.deleteIfPresent(rectangle)){
    }

    dominatedSolutions = (rectangle.obj1, rectangle.obj2) :: dominatedSolutions
  }

  private def replaceRectangleAndSchedule(oldRectangle: Rectangle, newRectangle: Rectangle): Unit = {
    paretoFront = paretoFront.excl(oldRectangle).incl(newRectangle)
    remainingSurface -= oldRectangle.surface
    require(remainingSurface>=0)
    if(rectanglesToDevelopBiggestRectangleFirst.deleteIfPresent(oldRectangle)){
    }

    if(newRectangle.isSplitteable) {
      rectanglesToDevelopBiggestRectangleFirst.insert(newRectangle)
      remainingSurface += newRectangle.surface
    }
  }

  private def storeAndScheduleRectangle(newRectangle: Rectangle): Unit = {
    paretoFront = paretoFront.incl(newRectangle)
    if(newRectangle.isSplitteable) {
      rectanglesToDevelopBiggestRectangleFirst.insert(newRectangle)
      remainingSurface += newRectangle.surface
      require(remainingSurface>=0)
    }
  }

  // /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
   * The method that performs pareto search
   * @return list of pareto points in the form (obj1,obj2,solution)
   */
  def paretoOptimize(): List[(Long, Long, Solution)] = {

    val (plot, window) = if (visu) {
      val p = new PlotPareto(null, obj1Name, obj2Name)
      val window = SingleFrameWindow.show(p, visuTitle, width = 2000, height = 2000)
      (p, window)
    } else (null, null)

    def redrawPareto(): Unit = {
      if (plot != null) {
        plot.reDrawPareto(
          paretoFront.toList.map({case rectangle:Rectangle => (rectangle.obj1, rectangle.obj2)}),
          Some(dominatedSolutions))
      }
    }
    def closeWindow():Unit = {
      if(window!=null)window.close()
    }

    val startSearchNanotime = System.nanoTime()

    val verbosityDelayMS = 1000

    var nextVerbosity = System.currentTimeMillis() + verbosityDelayMS
    def paretoFrontStr:String = {
      val paretoFrontCouples = paretoFront.toList.map({case s:Rectangle => ("" + s.obj1 ,"" + s.obj2)})
      "paretoFront:\n\t" + Properties.justifyLeft(paretoFrontCouples,",").mkString("\n\t")
    }
    if (verbose) println("BiObjectiveSearch: search first solution: minObj2")
    val rightMostRectangle = {

      val neighborhoodForFistSolution = minObj2Neighborhood.getOrElse(minObj1Neighborhood)
      neighborhoodForFistSolution().doAllMoves(obj = obj2)

      val foundOBj2 = obj2.value
      val minObj1WithFoundObj2 =
        CascadingObjective(
          () => (0L max (obj2.value - foundOBj2)),
          obj1)
      minObj1Neighborhood().doAllMoves(obj = minObj1WithFoundObj2)

      val solutionAtMin2: Solution = obj2.model.solution()

      //Store initial given solution
      val rectangle = Rectangle(
        obj1 = obj1.value, obj2 = obj2.value,
        maxObj1 = obj1.value, minObj2 = obj2.value,
        solutionAtMin2,
        IndependentSolution(solutionAtMin2,None))

      storeAndScheduleRectangle(rectangle)
      rectangle
    }

    if (verbose) println("BiObjectiveSearch: search first solution: minObj1")
    val leftMostRectangle = {
      val neighborhoodForFistSolution = minObj1Neighborhood
      neighborhoodForFistSolution().doAllMoves(obj = obj1)

      val foundOBj1 = obj1.value
      val minObj2WithFoundObj1 =
        CascadingObjective(
          () => (0L max (obj1.value - foundOBj1)),
          obj2)
      minObj2Neighborhood.getOrElse(minObj1Neighborhood)().doAllMoves(obj = minObj2WithFoundObj1)

      val solutionAtMin1: Solution = obj2.model.solution()

      //Store initial given solution
      val rectangle = Rectangle(
        obj1 = obj1.value, obj2 = obj2.value,
        maxObj1 = rightMostRectangle.obj1-1, minObj2 = rightMostRectangle.obj2+1,
        solutionAtMin1,
        IndependentSolution(solutionAtMin1,None))

      storeAndScheduleRectangle(rectangle)
      rectangle
    }

    def frontStr:String = {
      paretoFront.toList.map(_.toString).mkString("\n\t")
    }

    if (verbose) println("BiObjectiveSearch: Start front exploration")

    def printStopCriterion(): Unit = {
      if (verbose) {
        println(s"stopCriterion(surface:$remainingSurface/$stopSurface nonDominatedSolutions:${paretoFront.size}/$maxPoints toDevelop:${rectanglesToDevelopBiggestRectangleFirst.size})")
      }
    }

    def shouldStop:Boolean = {
      remainingSurface < stopSurface || paretoFront.size >= maxPoints
    }

    abstract class WrappedData
    case class WrappedSearchEnded(searchEnded: SearchEnded, //obj1,obj2,sol,maxValueForObj2
                                  initRectangle: Rectangle //the one before the split, so we are on the left
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
    implicit val timeout: Timeout = 30.minutes

    //the search is performed in a separated actor while this tread is waiting on a future
    //we wait on futureResult for the final answer
    supervisor.spawnNewActor(Behaviors.setup { context: ActorContext[WrappedData] => {
      if (verbose) context.log.info(s"start search")
      next(nbRunningOrStartingSearches = 0, context)
    }
    }, "DistributedBiObjective")

    def checkParetoFront() :Unit = {
      val sortedByObj1 = paretoFront.toList.sortBy(_.obj1).map(_.asInstanceOf[Rectangle]).toArray
      for(i <- 0 until sortedByObj1.length-1){
        require(sortedByObj1(i).obj1 < sortedByObj1(i+1).obj1, s"${sortedByObj1(i)};${sortedByObj1(i+1)}")
        require(sortedByObj1(i).obj2 > sortedByObj1(i+1).obj2, s"dominated square in front: ${sortedByObj1(i+1)} by ${sortedByObj1(i)}")
        require(sortedByObj1(i).maxObj1 < sortedByObj1(i+1).obj1, s"${sortedByObj1(i)};${sortedByObj1(i+1)}")
        require(sortedByObj1(i).minObj2 > sortedByObj1(i+1).obj2, s"${sortedByObj1(i)};${sortedByObj1(i+1)}")
      }
    }

    def logNext(context:ActorContext[WrappedData],nbRunningOrStartingSearches:Int): Unit ={
      if(verbose){
        val now = System.currentTimeMillis()
        if(now > nextVerbosity){
          context.log.info(s"nbRunningOrStartingSearches:$nbRunningOrStartingSearches heapSize:${rectanglesToDevelopBiggestRectangleFirst.size} paretoFrontSize:${paretoFront.size}/$maxPoints remainingSurface:$remainingSurface/$stopSurface")
          nextVerbosity = now + verbosityDelayMS
        }
      }
    }

    def next(nbRunningOrStartingSearches: Int, context:ActorContext[WrappedData]): Behavior[WrappedData] = {

      if(nbRunningOrStartingSearches == 0 && (shouldStop || rectanglesToDevelopBiggestRectangleFirst.isEmpty)){
        //we should stop
        logNext(context,nbRunningOrStartingSearches)
        context.log.trace("should stop in the loop")

        resultPromise.success(WrappedCompleted())
        Behaviors.stopped
      }else if (!shouldStop && nbRunningOrStartingSearches < maxWorkers && !rectanglesToDevelopBiggestRectangleFirst.isEmpty) {

        //split a rectangle
        val rectangleToSplit:Rectangle = rectanglesToDevelopBiggestRectangleFirst.removeFirst()

        val maxValueForObj2 = (rectangleToSplit.minObj2 + rectangleToSplit.obj2)/2
        val rectangleForStartSolution = paretoFront.minAfter(new SortedRectangle(rectangleToSplit.obj1+1)).getOrElse(rightMostRectangle).asInstanceOf[Rectangle]

        val startSolution = Some(rectangleForStartSolution.independentSolution)

        require(maxValueForObj2 >= rectangleForStartSolution.obj2, "maxValueForObj2:" + maxValueForObj2 + "obj2:" + rectangleForStartSolution.obj2)

        context.ask[DelegateSearch, SearchEnded](
          supervisor.supervisorActor, ref =>  DelegateSearch(OptimizeWithBoundRequest(
            remoteTaskId = this.remoteTaskIdentification(0),
            obj1 = this.obj1.getIndependentObj,
            obj2 = this.obj2.getIndependentObj,
            maxValueForObj2 = maxValueForObj2,
            startSolution = startSolution,
            initObj1 = rectangleForStartSolution.obj1,
            initObj2 = rectangleForStartSolution.obj2,
            sendResultTo = ref))) {
          case Success(ended:SearchEnded) => WrappedSearchEnded(ended, rectangleToSplit)
          case Failure(_) => WrappedError(msg = Some("DistributedBIObjectiveSearch timeout3"))
        }
        logNext(context,nbRunningOrStartingSearches+1)
        next(nbRunningOrStartingSearches = nbRunningOrStartingSearches + 1, context)

      } else {
        Behaviors.receive { (context, command) =>
          command match {
            case WrappedSearchEnded(searchEnded, initRectangle) =>
              searchEnded match {
                case SearchCompleted(_, (obj1: Long, obj2: Long, independentSolution: IndependentSolution, maxValueForObj2: Long), _) =>
                  //Ici, il faut analyser le front de Pareto correctement

                  getDominatorRectangle(obj1,obj2) match{
                    case Some(dominatingRectangle) =>
                      //forget about it, but the dominating rectangle can be pruned
                      //we do not know if init rectangle is still present...
                      if(verbose) context.log.info(s"new rectangle dominated")
                      dominatedSolutions = (obj1,obj2) :: dominatedSolutions
                      if(dominatingRectangle.minObj2 > maxValueForObj2) {
                        val newDominatingRectangle = dominatingRectangle.copy(minObj2 = dominatingRectangle.minObj2 max maxValueForObj2)
                        replaceRectangleAndSchedule(dominatingRectangle, newDominatingRectangle)
                      }
                    case None =>

                      val firstRectangleOpt = paretoFront.maxBefore(new SortedRectangle(obj1))
                      firstRectangleOpt match{
                        case Some(firstRectangle:Rectangle) =>
                          val newFirstRectangle = firstRectangle.copy(
                            maxObj1 = firstRectangle.maxObj1 min (obj1-1),
                            minObj2 = firstRectangle.obj2 min (firstRectangle.minObj2 max (maxValueForObj2+1)))
                          replaceRectangleAndSchedule(
                            firstRectangle,
                            newFirstRectangle)
                        case _ => ;//it has disappeared since then
                      }
                      var dominatedRectangleOpt = paretoFront.minAfter(new SortedRectangle(obj1))
                      while(dominatedRectangleOpt match{
                        case Some(dominated:Rectangle) if dominated.minObj2 > obj2 =>
                          removeDominatedRectangle(dominated)
                          dominatedRectangleOpt = paretoFront.minAfter(new SortedRectangle(obj1))
                          true
                        case _ => false
                      }) {}
                      //last rectangle
                      dominatedRectangleOpt match{
                        case Some(lastRectangle:Rectangle) if lastRectangle.obj2 >= obj2 =>
                          removeDominatedRectangle(lastRectangle)
                          if(verbose)  context.log.info("removed dominated rectangle:" + lastRectangle)
                          storeAndScheduleRectangle(Rectangle(
                            obj1,
                            obj2,
                            maxObj1 = lastRectangle.maxObj1,
                            minObj2 = lastRectangle.minObj2,
                            solution = independentSolution.makeLocal(store),
                            independentSolution = independentSolution))
                        //checkParetoFront()
                        case Some(lastRectangle:Rectangle) if lastRectangle.obj2 <= obj2 =>
                          //we cut nothing
                            storeAndScheduleRectangle(Rectangle(
                              obj1,
                              obj2,
                              maxObj1 = obj1 max (initRectangle.maxObj1 min (lastRectangle.obj1-1)),
                              minObj2 = obj2 min (initRectangle.minObj2 max (lastRectangle.obj2+1)),
                              solution = independentSolution.makeLocal(store),
                              independentSolution = independentSolution))
                      }
                  }
                  redrawPareto()

                  next(nbRunningOrStartingSearches = nbRunningOrStartingSearches - 1, context)

                case SearchAborted(uniqueSearchID: Long) =>
                  //strange, we never abort searches...
                  logNext(context,nbRunningOrStartingSearches)
                  resultPromise.success(WrappedError(msg=Some("search aborted unexpectedly")))
                  Behaviors.stopped
                case c:SearchCrashed =>
                  //finish it all with a crash message
                  logNext(context,nbRunningOrStartingSearches)
                  resultPromise.success(WrappedError(crash = Some(c)))
                  Behaviors.stopped
              }
            case  e:WrappedError =>
              logNext(context,nbRunningOrStartingSearches)
              resultPromise.success(e)
              Behaviors.stopped
          }
        }
      }
    }

    //await seems to block the actor system??
    Await.result(futureResult, Duration.Inf) match {
      case WrappedCompleted() =>

      case WrappedError(msg:Option[String],crash:Option[SearchCrashed]) =>
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


    if(verbose) {
      printStopCriterion()
      println("elapsed(ms):" + ((System.nanoTime() - startSearchNanotime)/1000000).toInt)
    }

    if(!stayAlive) window.close()

    paretoFront.toList.map({case rectangle:Rectangle => (rectangle.obj1,rectangle.obj2,rectangle.solution)}).sortBy(_._1)
  }

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    throw new Error("DistributedPareto cannot be used as a regular neighborhood; use paretoOptimize method")
  }
}
