package oscar.cbls.business.routing.invariants.timeWindow

import oscar.cbls.business.routing.forwardCumulativeIntegerIntegerDimensionOnVehicle
import oscar.cbls.business.routing.invariants.capa.ForwardCumulativeIntegerIntegerDimensionOnVehicle
import oscar.cbls.core.computation.{CBLSIntConst, CBLSSeqVar, ChangingIntValue}
import oscar.cbls.core.constraint.ConstraintSystem

object NaiveTimeWindowConstraint {
  def apply(routes: CBLSSeqVar, n: Int, v: Int, singleNodesTransferFunctions: Array[TransferFunction], travelDurationMatrix: Array[Array[Long]]): NaiveTimeWindowConstraint = {
    new NaiveTimeWindowConstraint(routes, n, v, singleNodesTransferFunctions, travelDurationMatrix)
  }

  def maxTransferFunctionWithTravelDurationRestriction(n: Int, v: Int,
                                                       singleNodesTransferFunctions: Array[TransferFunction],
                                                       maxTravelDurationConstraints: List[(Int, Int, Long)],
                                                       chainsOfNodes: List[List[Int]],
                                                       travelDurationMatrix: Array[Array[Long]]): Unit ={
    val reversedChains = chainsOfNodes.map(_.reverse)
    for((from, to, maxTravelDuration) <- maxTravelDurationConstraints){
      // Update the latest arrival time of the target node based on the max travel duration and
      // the latest leaving time of the source node
      singleNodesTransferFunctions(to) = {
        val la = singleNodesTransferFunctions(from).latestLeavingTime + maxTravelDuration
        require(singleNodesTransferFunctions(to).ea <= la,
          "The max travel duration is too short regarding the earliest arrival time at the targeted node.\n" +
            s"Maximum $maxTravelDuration units of time between node : $from and node $to.")
        DefinedTransferFunction(singleNodesTransferFunctions(to).ea, la, singleNodesTransferFunctions(to).el, to, to)
      }

      // Update the latest arrival time of all the node that precede (in chain) the target node
      reversedChains.filter(_.contains(to)).foreach(chain => {
        var nextNodeInChain = to
        chain.dropWhile(_ != to).drop(1).foreach(
          prevNodeInChain => {
            val prevTransferFunction = singleNodesTransferFunctions(prevNodeInChain)
            val la = singleNodesTransferFunctions(nextNodeInChain).la -
              travelDurationMatrix(prevNodeInChain)(nextNodeInChain) -
              prevTransferFunction.taskDuration
            require(prevTransferFunction.ea <= la,
              s"""The max travel duration is too short regarding the earliest arrival time of the node preceding the targeted node.
                 |Maximum $maxTravelDuration units of time between node : $from and node $to.
                 |Problematic chain of nodes : ${chain.reverse}""".stripMargin)

            singleNodesTransferFunctions(prevNodeInChain) =
              DefinedTransferFunction(prevTransferFunction.ea, la,
                prevTransferFunction.el, prevNodeInChain, prevNodeInChain)
            nextNodeInChain = prevNodeInChain
          }
        )
      })
    }
  }
}

class NaiveTimeWindowConstraint(routes: CBLSSeqVar, n: Int, v: Int, singleNodesTransferFunctions: Array[TransferFunction], travelDurationMatrix: Array[Array[Long]]){

  private val naiveTimeWindowInvariant: ForwardCumulativeIntegerIntegerDimensionOnVehicle = forwardCumulativeIntegerIntegerDimensionOnVehicle(
    routes,n,v,
    (fromNode,toNode,arrivalTimeAtFromNode,leaveTimeAtFromNode)=> {
      val toNodeTF = singleNodesTransferFunctions(toNode)
      val arrivalTimeAtToNode = leaveTimeAtFromNode + travelDurationMatrix(fromNode)(toNode)
      val leaveTimeAtToNode =
        if(toNode < v) 0
        else toNodeTF.el + Math.max(arrivalTimeAtToNode - toNodeTF.ea, 0)
      (arrivalTimeAtToNode,leaveTimeAtToNode)
    },
    Array.tabulate(v)(x => new CBLSIntConst(0)),
    Array.tabulate(v)(x => new CBLSIntConst(singleNodesTransferFunctions(x).el)),
    0,
    0,
    contentName = "Time at node"
  )

  val arrivalTimes = Array.tabulate(n)(node => if(node < v) naiveTimeWindowInvariant.content1AtEnd(node) else naiveTimeWindowInvariant.content1AtNode(node))
  val leaveTimes = naiveTimeWindowInvariant.content2AtNode

  private def generateNaiveConstraintSystem(): ConstraintSystem ={
    val naiveTimeWindowConstraint = new ConstraintSystem(routes.model)
    for(i <- 0 until n){
      val ll = singleNodesTransferFunctions(i).latestLeavingTime
      if(i < v && ll != Long.MaxValue) {
        naiveTimeWindowConstraint.post((arrivalTimes(i) le ll).nameConstraint("end of time for vehicle " + i))
      } else {
        if(ll != Long.MaxValue)
          naiveTimeWindowConstraint.post((leaveTimes(i) le ll).nameConstraint("end of time window on node " + i))
      }
    }
    naiveTimeWindowConstraint
  }

  def addMaxTravelDurationConstraint(maxDurationConstraints: List[(Int, Int, Long)]): Unit ={
    for((from, to, maxTravelDuration) <- maxDurationConstraints){
      _violation.post(arrivalTimes(to) - leaveTimes(from) le maxTravelDuration)
    }
  }

  private val _violation = generateNaiveConstraintSystem()
  def violation: ChangingIntValue = {
    if (!_violation.isClosed) _violation.close()
    _violation.Violation
  }
}
