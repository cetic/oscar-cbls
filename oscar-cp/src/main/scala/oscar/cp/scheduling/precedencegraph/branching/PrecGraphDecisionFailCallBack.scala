package oscar.cp.scheduling.precedencegraph.branching

import oscar.algo.Inconsistency
import oscar.cp.scheduling.precedencegraph.PrecedenceGraph

/**
  * Created by saschavancauwelaert on 07/07/2017.
  */

class PrecGraphDecisionFailCallBack(precGraph: PrecedenceGraph, firstTask: Int, secondTask:Int, machineName: String, failCallBack: () => Unit) extends PrecGraphDecision(precGraph, firstTask, secondTask,machineName) {
  override def apply() = {
      try {
        super.apply()
      }
    catch {
      case Inconsistency => failCallBack(); throw Inconsistency
    }

  }
  override def toString(): String = s"Machine $machineName task ${firstTask} --> ${secondTask}"
}

