package oscar.cp.scheduling.precedencegraph.branching

import oscar.algo.search.Decision
import oscar.cp.scheduling.precedencegraph.PrecedenceGraph
//import oscar.cp.searches.Decision

/**
  * Created by saschavancauwelaert on 07/07/2017.
  */

class PrecGraphDecision(val precGraph: PrecedenceGraph,  firstTask: Int, secondTask:Int, machineName: String) extends Decision {
  def apply() = {
      precGraph.addNonDetectablePrecAndUpdateTransitiveClosure(firstTask,secondTask)
      precGraph.triggerPropagation()
  }
  override def toString(): String = s"Machine $machineName task ${firstTask} --> ${secondTask}"
}

