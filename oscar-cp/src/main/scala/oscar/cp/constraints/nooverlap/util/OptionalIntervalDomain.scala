package oscar.cp.constraints.nooverlap.util

import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.variables.CPIntVar

class OptionalIntervalDomain(start: CPIntVar, duration: CPIntVar, end: CPIntVar, runOnResource: CPIntVar, resourceID: Int) {

  implicit val solver = start.store
  val minStart = ReversibleInt(start.min)
  val maxStart = ReversibleInt(start.max)
  val minDuration = ReversibleInt(duration.min)
  val maxDuration = ReversibleInt(duration.max)
  val minEnd = ReversibleInt(end.min)
  val maxEnd = ReversibleInt(end.max)

  def updateMinStart(newValue: Int) = updateMin(start, minStart, newValue)
  def updateMaxStart(newValue: Int)= updateMax(start, maxStart, newValue)
  def updateMinEnd(newValue: Int)= updateMin(end, minEnd, newValue)
  def updateMaxEnd(newValue: Int)= updateMax(end, maxEnd, newValue)

  private def updateMin(variable: CPIntVar, optionalVariable: ReversibleInt, newValue: Int): Unit ={
    if(runOnResource.hasValue(resourceID)){
      if(runOnResource.isBoundTo(resourceID))
        variable.updateMin(math.max(newValue, optionalVariable.value))
      else if (optionalVariable.value < newValue)
        optionalVariable.setValue(newValue)
    }
  }

  private def updateMax(variable: CPIntVar, optionalVariable: ReversibleInt, newValue: Int): Unit ={
    if(runOnResource.hasValue(resourceID)){
      if(runOnResource.isBoundTo(resourceID))
        variable.updateMax(math.min(newValue, optionalVariable.value))
      else if (optionalVariable.value > newValue)
        optionalVariable.setValue(newValue)
    }
  }

}
