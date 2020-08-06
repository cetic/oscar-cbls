package oscar.cbls.test.neighborhood

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.{VRP, routeLength}
import oscar.cbls.core.computation.Store
import oscar.cbls.{CBLSSeqVar, Objective, length, sum}
import oscar.cbls._
import oscar.cbls.benchmarks.vrp.RoutingMatrixGenerator
import oscar.cbls.business.routing.model.helpers.DistanceHelper

object NeighborhoodTestBench {

  var obj :Objective = _
  var problem :MockedVRP = _

  def initTest(check :(Long,Long,IntSequence,IntSequence) => Unit) :(Store, MockedVRP,Objective,Array[Iterable[Int]],Int => Int => Boolean) = {
    val nbNode = 30
    val nbVehicle = 15
    val model = Store()
    problem = new MockedVRP(check,model,nbNode,nbVehicle)
    val relevantPredecessorsOfNodes = (_:Int) => problem.nodes
    val (symetricDistanceMatrix,_) = RoutingMatrixGenerator(nbNode)
    val routedPostFilter = (_:Int) => (neighbor:Int) => problem.isRouted(neighbor)
    val closestRelevantNeighbors =
      Array.tabulate(nbNode)(DistanceHelper.lazyClosestPredecessorsOfNode(symetricDistanceMatrix,relevantPredecessorsOfNodes)(_))

    val totalDistance = sum(routeLength(problem.routes, nbNode, nbVehicle, perVehicle = false, symetricDistanceMatrix, distanceIsSymmetric = true))
    val unroutedPenalty = 1000000
    obj = Objective((nbNode - length(problem.routes)) * unroutedPenalty + totalDistance)

    (model, problem, obj, closestRelevantNeighbors, routedPostFilter)
  }

  class MockedVRP(check :(Long,Long,IntSequence,IntSequence) => Unit,
                  m: Store,
                  n: Int,
                  v: Int,
                  maxPivotPerValuePercent:Int = 4) extends VRP(m,n,v,maxPivotPerValuePercent) {
    override val routes = new MockedCBLSSeqVar(check, m, IntSequence(0 until v), n-1, "routes", maxPivotPerValuePercent=maxPivotPerValuePercent)
  }

  class MockedCBLSSeqVar(val check :(Long,Long,IntSequence,IntSequence) => Unit,
                         givenModel:Store,
                         initialValue:IntSequence,
                         maxVal:Int = Int.MaxValue,
                         n: String = null,
                         maxPivotPerValuePercent:Int = 4,
                         maxHistorySize:Int = 50)
    extends CBLSSeqVar(givenModel,initialValue,maxVal,n,maxPivotPerValuePercent,maxHistorySize){

    var firstRun = true
    var previousObj :Long = _
    var previousSeq :IntSequence = _

    override def insertAtPosition(value:Int,pos:Int): Unit ={
      super.insertAtPosition(value,pos)

      if(!firstRun)
        check(previousObj,obj.value,previousSeq,this.value)

      firstRun = false
      previousObj = obj.value
      previousSeq = this.value
    }

    override def remove(position: Int): Unit = {
      super.remove(position)

      if(!firstRun)
        check(previousObj,obj.value,previousSeq,this.value)

      firstRun = false
      previousObj = obj.value
      previousSeq = this.value
    }

    override def move(fromIncludedPosition: Int, toIncludedPosition: Int, afterPosition: Int, flip: Boolean): Unit = {
      super.move(fromIncludedPosition, toIncludedPosition, afterPosition, flip)

      if(!firstRun)
        check(previousObj,obj.value,previousSeq,this.value)

      firstRun = false
      previousObj = obj.value
      previousSeq = this.value
    }
  }
}
