package oscar.cp.scheduling.precedencegraph.branching.decisions

import oscar.algo.reversible.ReversibleArrayStack
import oscar.cp.scheduling.precedencegraph.PrecedenceGraph

class PrecGraphDecisionForNoGood(precGraph: PrecedenceGraph, val machineIndex: Int, val firstTask: Int, val secondTask:Int, machineName: String, failCallBack: () => Unit, val isLeftDecision: Boolean, stack: ReversibleArrayStack[PrecGraphDecisionForNoGood]) extends PrecGraphDecisionFailCallBack(precGraph: PrecedenceGraph,  firstTask: Int, secondTask:Int, machineName: String, failCallBack) {
  override def apply() = {
    stack.push(this)
    super.apply()
  }

  override def toString(): String = super.toString() + s" | isLeftDecision: $isLeftDecision"
}
