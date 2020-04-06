package oscar.cbls.benchmarks

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
/*
import oscar.cbls._
import oscar.cbls.core.computation.Store
import oscar.cbls.lib.invariant.seq.Length
import oscar.cbls.core.objective.Objective
import oscar.cbls.business.routing.model._
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}
import oscar.cbls.util.StopWatch

import scala.util.Random

class MySimpleRoutingWithUnroutedPoints(n:Long,v:Long,symmetricDistance:Array[Array[Long]],m:Store, maxPivot:Long)
  extends VRP(n,v,m,maxPivot)
  with TotalConstantDistance with ClosestNeighbors with RoutedAndUnrouted{

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Long, to : Long) : Long = symmetricDistance(from)(to)

  val penaltyForUnrouted  = 10000L

  val obj = Objective(totalDistance + (penaltyForUnrouted*(n - Length(routes))))

  override def toString : String = super.toString + "objective: " + obj.value + "\n"

  val closestNeighboursForward = computeClosestNeighborsForward()

  def size = routes.value.size
}

object SymmetricVRPBench extends App {

  val n = 1000L
  val v = 10L

  val verbose = 0L
  val maxPivotPerValuePercent = 4L
  new TSPRoutePointsS(1000L,100L,4L,verbose)
  System.gc()

  val nbTrials = 10L

  println()
  print("n\tv\tpercent")
  for ( _ <- 1 to nbTrials) {
    print("\ttime")
  }
  println


  for(n <- 1000 to 5000L by 2000L){
    for(v <- List(100L)){
      for (maxPivotPerValuePercent <- List(0L,1L,2L,3L,4L,5L,20L)) {
        print(n + "\t" + v + "\t" + maxPivotPerValuePercent + "\t")
        for ( _ <- 1 to nbTrials){
          new TSPRoutePointsS(n, v, maxPivotPerValuePercent, verbose)
          print("\t")
          System.gc()
        }
        println
      }
    }
  }
}

class TSPRoutePointsS(n:Long,v:Long,maxPivotPerValuePercent:Long, verbose:Long) extends StopWatch{

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1

  startWatch()
  //  println("restrictions:" + restrictions)
  val model = Store() //checker = Some(new ErrorChecker()))

  val myVRP = new MySimpleRoutingWithUnroutedPoints(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent)
  val nodes = myVRP.nodes

  model.close()

  val routeUnroutdPoint =  Profile(InsertPointUnroutedFirst(myVRP.unrouted,()=>myVRP.kFirst(10L,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,neighborhoodName = "InsertUF"))

  //TODO: using post-filters on k-nearest is probably crap
  val routeUnroutdPoint2 =  Profile(InsertPointRoutedFirst(myVRP.routed,()=>myVRP.kFirst(10L,myVRP.closestNeighboursForward,x => !myVRP.isRouted(x)),myVRP,neighborhoodName = "InsertRF")  guard(() => myVRP.size < n/2L))

  def onePtMove(k:Long) = Profile(OnePointMove(myVRP.routed, () => myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

  val twoOpt = Profile(TwoOpt(myVRP.routed, ()=>myVRP.kFirst(20L,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP))

  def threeOpt(k:Long, breakSym:Boolean) = Profile(ThreeOpt(myVRP.routed, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val search = BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, onePtMove(10L),twoOpt, threeOpt(10L,true))) exhaust threeOpt(20L,true)

  // val search = (new RoundRobin(List(routeUnroutdPoint2,onePtMove(10L) guard (() => myVRP.unrouted.value.size != 0L)),10L)) exhaust BestSlopeFirst(List(onePtMove(20L),twoOpt, threeOpt(10L,true))) exhaust threeOpt(20L,true)

  search.verbose = verbose
  //search.verboseWithExtraInfo(1L, ()=> "" + myVRP)

  search.doAllMoves(obj=myVRP.obj)

  print(getWatch)
}

object RoutingMatrixGenerator {
  val random = new Random(0L)

  def apply(N : Long, side : Long = 1000L) : (Array[Array[Long]], Array[(Long, Long)]) = {

    //we generate te cost distance matrix
    def randomXY : Long = (random.nextFloat() * side).toInt
    val pointPosition : Array[(Long, Long)] = Array.tabulate(N)( _ => (randomXY, randomXY))

    def distance(from : (Long, Long), to : (Long, Long)) =
      math.sqrt(math.pow(from._1 - to._1, 2L) + math.pow(from._2 - to._2, 2L)).toInt

    //for each delivery point, the distance to each warehouse
    (Array.tabulate(N)(
      n1 => Array.tabulate(N)(
        n2 => distance(pointPosition(n1), pointPosition(n2)))), pointPosition)
  }
}
*/