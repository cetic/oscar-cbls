package oscar.cbls.test.neighborhood

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.{DistanceHelper, VRP, routeLength}
import oscar.cbls.core.computation.Store
import oscar.cbls.{CBLSSeqVar, Objective, length, sum}
import oscar.cbls._
import oscar.cbls.core.search.Neighborhood
import oscar.examples.cbls.routing.RoutingMatrixGenerator

object NeighborhoodTestBench {

  var obj :Objective = _
  var problem :MockedVRP = null

  def initTest(check :(Long,Long,IntSequence,IntSequence) => Unit) :(Store, MockedVRP,Objective,Array[Iterable[Long]],Long => Long => Boolean) = {
    val nbNode = 30
    val nbVehicle = 15
    val model = Store()
    problem = new MockedVRP(check,model,nbNode,nbVehicle)
    val relevantPredecessorsOfNodes = (node:Long) => problem.nodes
    val (symetricDistanceMatrix,_) = RoutingMatrixGenerator(nbNode)
    val routedPostFilter = (node:Long) => (neighbor:Long) => problem.isRouted(neighbor)
    val closestRelevantNeighbors =
      Array.tabulate(nbNode)(DistanceHelper.lazyClosestPredecessorsOfNode(symetricDistanceMatrix,relevantPredecessorsOfNodes)(_))

    val totalDistance = sum(routeLength(problem.routes, nbNode, nbVehicle, false, symetricDistanceMatrix, true))
    val unroutedPenalty = 1000000
    obj = Objective((nbNode - length(problem.routes)) * unroutedPenalty + totalDistance)

    (model, problem, obj, closestRelevantNeighbors, routedPostFilter)
  }

  class MockedVRP(check :(Long,Long,IntSequence,IntSequence) => Unit, m: Store, n: Int, v: Int, maxPivotPerValuePercent:Long = 4L) extends {
    override val routes = new MockedCBLSSeqVar(check, m, IntSequence(0L until v), n-1L, "routes", maxPivotPerValuePercent=maxPivotPerValuePercent)
  } with VRP(m,n,v,maxPivotPerValuePercent)

  class MockedCBLSSeqVar(val check :(Long,Long,IntSequence,IntSequence) => Unit,
                         givenModel:Store,
                         initialValue:IntSequence,
                         maxVal:Long = Long.MaxValue,
                         n: String = null,
                         maxPivotPerValuePercent:Int = 4,
                         maxHistorySize:Int = 50)
    extends CBLSSeqVar(givenModel,initialValue,maxVal,n,maxPivotPerValuePercent,maxHistorySize){

    var firstRun = true
    var previousObj :Long = _
    var previousSeq :IntSequence = _

    override def insertAtPosition(value:Long,pos:Int): Unit ={
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
