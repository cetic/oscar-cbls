package oscar.cbls.benchmarks.vrp

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

import java.io.{File, PrintWriter}

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.core.search.First
import oscar.cbls.util.StopWatch

import scala.io.Source

object TSProutePoints extends App {

  def printMatrix(m:Array[Array[Long]]){
    println(m.map(l => l.mkString(" ")).mkString("\n"))
  }

  def benchmarkOnProblem(fileName:String){
    warmUp(1000L)
    val matrix = loadMatrixFromFile(fileName:String)
    val n = matrix.length
    val v = 1L
    val percent = 3L
    println("benchmarking " + fileName)
    print("n\tv\tpercent\ttime")
    print(n + "\t" + v + "\t" + percent + "\t")
    new TSPRoutePointsS(n, v, percent, 0L, matrix)
  }

  def warmUp(n:Long = 10000L){
    val verbose = 1L
    val maxPivotPerValuePercent = 4L
    val v = 100L
    val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1
    new TSPRoutePointsS(n, v, maxPivotPerValuePercent, verbose, symmetricDistanceMatrix)
    System.gc()
  }

  def performRandomBenchmark() {

    println("performing warm up")
    warmUp()
    println("done. ")
    val nbTrials = 100L

   // throw new Error("finished")
    println()
    print("n\tv\tpercent")
    for (t <- 1L to nbTrials) {
      print("\ttime")
    }
    println

    for { n <- 1000L to 11000L by 2000L
          v <- List(100L)
          maxPivotPerValuePercent <- List(0L, 1L, 2L, 3L, 4L, 5L, 20L) } {
      print(n + "\t" + v + "\t" + maxPivotPerValuePercent + "\t")
      for (t <- 1L to nbTrials) {
        val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1
        new TSPRoutePointsS(n, v, maxPivotPerValuePercent, 0L, symmetricDistanceMatrix)
        print("\t")
        System.gc()
      }
      println
    }
  }

  def loadMatrixFromFile(filename:String):Array[Array[Long]] = {
    val file = Source.fromFile(filename)
    val words: Array[String] = file.mkString.split("\\s+")
    file.close()
    val reader = words.iterator

    val n = reader.next().toInt

    val matrix = Array.tabulate(n)(_ => Array.fill(n)(-1L))

    for(i <- 0L until n){
      for(j <- 0L until n){
        matrix(i)(j) = reader.next().toInt
      }
    }
    require(!reader.hasNext)
    matrix
  }

  def writeMatrix(writer:PrintWriter,matrix:Array[Array[Long]]){
    val n = matrix.length
    for(i <- 0L until n){
      for(j <- 0L until n){
        writer.write(matrix(i)(j) + " ")
      }
      writer.write("\n")
    }
  }

  def saveMatrixToFile(fileName:String,matrix:Array[Array[Long]]){
    val writer = new PrintWriter(new File(fileName))
    writer.write(matrix.length + "\n")
    writeMatrix(writer,matrix)
    writer.close()
  }

  def saveMatrixToLocalSolverFile(fileName:String,matrix:Array[Array[Long]]){
    val writer = new PrintWriter(new File(fileName))
    val n = matrix.length
    writer.write("NAME: RANDOM" + fileName + "\n")
    writer.write("TYPE: TSP\n")
    writer.write("COMMENT: " + n + " city problem\n")
    writer.write("DIMENSION: " + n + "\n")
    writer.write("EDGE_WEIGHT_TYPE: EXPLICIT\n")
    writer.write("EDGE_WEIGHT_FORMAT: FULL_MATRIX\n")
    writer.write("EDGE_WEIGHT_SECTION\n")
    writeMatrix(writer,matrix)
    writer.write("EOF\n")
    writer.close()
  }

  def generateAllBenchmarks(){
    for(n <- benchmarkSizes){
      println("generating TSP n:" + n + " to file:" + fileName)
      val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1
      saveMatrixToFile(fileName + n + ".bench",symmetricDistanceMatrix)
      saveMatrixToLocalSolverFile(fileName + "_localSolver_" + n + ".tsp",symmetricDistanceMatrix)
    }
  }

  def runAllBenchmarks(){
    warmUp(200L)
    println()
    print("balise\tn\ttime\tobj")
    println

    for(n <- benchmarkSizes){
      print("runResult " + n + "\t")
      val matrix = loadMatrixFromFile(fileName + n + ".bench")
      new TSPRoutePointsS(n, 1L, 3L, 0L, matrix,true)
      print("\n")
      System.gc()
    }
  }

  def runBenchmark(fileName:String,n:Long){
    new TSPRoutePointsS(1000L, 100L, 3L, 0L, RoutingMatrixGenerator(1000L)._1)

    println()
    print("n\ttime\tobj")
    println
      print(n + "\t")
      val matrix = loadMatrixFromFile(fileName + n + ".bench")
      new TSPRoutePointsS(n, 1L, 3L, 0L, matrix,true)
      print("\n")
      System.gc()
  }

  val benchmarkSizes = 500L to 5000L by 500L
  var fileName = "C:\\Users\\rdl\\Documents\\Oscar\\BitBucket3\\oscar-cbls\\src\\main\\examples\\oscar\\examples\\cbls\\routing\\data\\bench"
  fileName = args(0L)
  println("benchmark path: " + fileName)
  //runBenchmark(fileName,1000L)
 // generateAllBenchmarks()
  runAllBenchmarks()
  //performRandomBenchmark()
}

class TSPRoutePointsS(n:Int,v:Int,maxPivotPerValuePercent:Long, verbose:Int, symmetricDistanceMatrix:Array[Array[Long]],printobj:Boolean = false) extends StopWatch{


  //  println("restrictions:" + restrictions)
  val model = new Store() //checker = Some(new ErrorChecker()))

  val myVRP = new VRP(model,n,v,maxPivotPerValuePercent = maxPivotPerValuePercent)

  //val totalRouteLength = constantRoutingDistance(myVRP.routes,n,v,false,symmetricDistanceMatrix,true,true,false)(0L)

  val (next,prev) = routeSuccessorAndPredecessors(myVRP.routes,v,n)()

  val distanceOut = Array.tabulate(n)((node:Int) => {
    val maxDistance = symmetricDistanceMatrix(node).max
    int2Int(next(node), nextNode => if(nextNode == n) 0L else symmetricDistanceMatrix(node)(nextNode),(0,maxDistance),false)})

  val totalRouteLengthSlow = sum(distanceOut)

  val penaltyForUnrouted  = 10000L

  val obj = Objective(totalRouteLengthSlow + (penaltyForUnrouted*(n - length(myVRP.routes))))

  override def toString : String = super.toString +  "objective: " + obj.value + "\n"

  model.close()


  val relevantPredecessorsOfNodes = (node:Long) => myVRP.nodes
  val closestRelevantNeighborsByDistance = Array.tabulate(n)(i => DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistanceMatrix,relevantPredecessorsOfNodes)(i))

  def routedPostFilter = (node:Long) => (neighbor:Long) => myVRP.isRouted(neighbor)
  def unRoutedPostFilter = (node:Long) => (neighbor:Long) => !myVRP.isRouted(neighbor)

  val routeUnroutdPoint =  profile(insertPointUnroutedFirst(() => myVRP.unrouted.value,()=> myVRP.kFirst(10L,(i => closestRelevantNeighborsByDistance(i)),routedPostFilter), myVRP,neighborhoodName = "InsertUF"))

  //TODO: using post-filters on k-nearest is probably crap
  val routeUnroutdPoint2 =  profile(insertPointRoutedFirst(() => myVRP.routed.value.toList.filter(_>=v),()=> myVRP.kFirst(10,(i => closestRelevantNeighborsByDistance(i)),unRoutedPostFilter),myVRP,neighborhoodName = "InsertRF")  guard(() => myVRP.routes.value.size < n/2L))

  def onePtMove(k:Long) = profile(onePointMove(() => myVRP.routed.value, () => myVRP.kFirst(k,(i => closestRelevantNeighborsByDistance(i)),routedPostFilter), myVRP))

  def customTwoOpt(k:Long) = profile(twoOpt(() => myVRP.routed.value, ()=> myVRP.kFirst(k,(i => closestRelevantNeighborsByDistance(i)),routedPostFilter), myVRP))

  def customThreeOpt(k:Long, breakSym:Boolean) = profile(threeOpt(() => myVRP.routed.value, ()=> (i => myVRP.kFirst(k,(i => closestRelevantNeighborsByDistance(i)),routedPostFilter)(i)), myVRP,selectFlipBehavior = First(),breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val vlsn1pt = mu[OnePointMoveMove](
    onePointMove(() => myVRP.routed.value, () => myVRP.kFirst(5L,(i => closestRelevantNeighborsByDistance(i)),routedPostFilter),myVRP),
    l => Some(onePointMove(() => List(l.head.newPredecessor).filter(_ >= v), () => myVRP.kFirst(3,(i => closestRelevantNeighborsByDistance(i)),routedPostFilter),myVRP, hotRestart = false)),
    intermediaryStops = true,
    maxDepth = 6)

  def segExchange(k:Long) = segmentExchange(myVRP,()=>myVRP.kFirst(k,(i => closestRelevantNeighborsByDistance(i)),routedPostFilter), () => myVRP.vehicles)

  val search = bestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, onePtMove(15),customTwoOpt(20), customThreeOpt(10,false))) exhaust customThreeOpt(25,false)

  // val search = (new RoundRobin(List(routeUnroutdPoint2,onePtMove(10L) guard (() => myVRP.unrouted.value.size != 0L)),10L)) exhaust BestSlopeFirst(List(onePtMove(20L),twoOpt, threeOpt(10L,true))) exhaust threeOpt(20L,true)

  search.verbose = verbose
  //search.verboseWithExtraInfo(1L, ()=> "" + myVRP)

  startWatch()

  search.doAllMoves(obj=obj,shouldStop = _ => getWatch >= 200L*1000L)

  print(getWatch)
  if(printobj){
    print("\t" + obj.value)
  }
}
