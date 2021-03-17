package oscar.cp.scheduling.precedencegraph

import oscar.cp.CPIntVar
import oscar.cp.scheduling.precedencegraph.nogoods.NoGoodBase

class PrecedenceGraphWithNoGoods(starts:Array[CPIntVar], durations:Array[CPIntVar], ends: Array[CPIntVar], ttMatrix: Array[Array[Int]], initialKnownPrecedences: Array[(Int,Int)] = Array(), var noGoodBase: NoGoodBase, var machineId: Int) extends PrecedenceGraph(starts, durations, ends, ttMatrix, initialKnownPrecedences) {

  override def addNonDetectablePrecAndUpdateTransitiveClosure(from: Int, to: Int): Unit = {
    super.addNonDetectablePrecAndUpdateTransitiveClosure(from,to)
    noGoodBase.inferences(machineId, from, to)
  }
}
