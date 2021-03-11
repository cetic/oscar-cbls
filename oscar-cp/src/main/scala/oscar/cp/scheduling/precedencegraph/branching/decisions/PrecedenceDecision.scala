package oscar.cp.scheduling.precedencegraph.branching.decisions

import oscar.algo.search.Decision
import oscar.cp.core.variables.CPIntVar

/**
  * Created by saschavancauwelaert on 07/07/2017.
  */

class PrecedenceDecision(val binaryPrec : CPIntVar, val binaryValue: Int, firstTask: Int, secondTask:Int, machineName: String) extends Decision {
  val cp = binaryPrec.store
  def apply() = {
    cp.post(binaryPrec === binaryValue)
  }

  override def toString(): String = s"Machine $machineName task ${firstTask} --> ${secondTask}"
}

