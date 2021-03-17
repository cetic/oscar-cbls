package oscar.cp.scheduling.precedencegraph.branching

import oscar.algo.reversible.ReversibleInt
import oscar.algo.search.Branching
import oscar.cp.scheduling.precedencegraph.PrecedenceGraph
import oscar.cp.{Alternative, noAlternative}

//this branching branches on all precedences (detectable included) because transition times remove the property that all detectable precedences are (fully) propagated
class DynamicPrecedenceGraphBranchingAllPrecedences(precGraph: PrecedenceGraph, machineName: String, varHeuristic: (Int,Int) => Int, isPrecOrderPreferable: (Int,Int) => Boolean) extends Branching {

  private[this] val nTasks = precGraph.nTasks
  implicit private[this] val cp = precGraph.store
  private[this] val nFixedPrec = new ReversibleInt(cp,0)

  private[this] val decisionPrecs : Array[(Int,Int)] = Array.tabulate(nTasks,nTasks)((i,j) => (i,j)).flatten.filter(t => t._1 < t._2) //TODO:use array os size 2 instead of tuples
  private[this] val decisionPrecsIndices : Array[Int] = decisionPrecs.zipWithIndex.map(_._2)
  private[this] val nMaxPrecs : Int = decisionPrecsIndices.length
  private[this] var foundPrecIndexToBranchOn : Int = -1

  final override def alternatives(): Seq[Alternative] = {
    findPrecIndexToBranchOn()
    if(foundPrecIndexToBranchOn == nMaxPrecs)
      return noAlternative
    else {
      val precToBranchOn = decisionPrecs(foundPrecIndexToBranchOn)
      val task1 = precToBranchOn._1
      val task2 = precToBranchOn._2

      if(precGraph.isPrecDetectable(task1,task2)) {
        List(new PrecGraphDecision(precGraph, task1, task2, machineName))
      }
      else if(precGraph.isPrecDetectable(task2,task1)) {
        List(new PrecGraphDecision(precGraph, task2, task1, machineName))
      }
      else if (isPrecOrderPreferable(task1, task2)) {
        val left = new PrecGraphDecision(precGraph, task1, task2, machineName)
        val right = new PrecGraphDecision(precGraph, task2, task1, machineName)
        List(left,right)
      }
      else {
        val left = new PrecGraphDecision(precGraph, task2, task1, machineName)
        val right = new PrecGraphDecision(precGraph, task1, task2, machineName)
        List(left,right)
      }
    }
  }

  def findPrecIndexToBranchOn() : Unit = {

    updateStartingNFixedPrecs()

    var i = nFixedPrec.value
    var bestIndex = nFixedPrec.value
    while(i < nMaxPrecs) {
      val currentPair = decisionPrecs(decisionPrecsIndices(i))
      if(precGraph.hasNonDetectablePrecForPair(currentPair._1,currentPair._2)) {
        swapIndexAndIncrementNFixedPrec(i)
        bestIndex = math.max(nFixedPrec.value, bestIndex)
      }
      else {
        val bestPrec = decisionPrecs(decisionPrecsIndices(bestIndex))
        if(varHeuristic(currentPair._1,currentPair._2) < varHeuristic(bestPrec._1, bestPrec._2))
          bestIndex = i

      }
      i += 1
    }

    if(bestIndex < decisionPrecsIndices.length) {
      foundPrecIndexToBranchOn = decisionPrecsIndices(bestIndex)
      swapIndexAndIncrementNFixedPrec(bestIndex)
    }
    else
      foundPrecIndexToBranchOn = nMaxPrecs
  }

  def updateStartingNFixedPrecs(): Unit = {
    var i = nFixedPrec.value
    var unfixedPrecFound = false
    while (i < nMaxPrecs && !unfixedPrecFound) {
      val currentPair = decisionPrecs(decisionPrecsIndices(i))
      if(precGraph.hasNonDetectablePrecForPair(currentPair._1,currentPair._2)) {
        swapIndexAndIncrementNFixedPrec(i)
      }
      else {
        unfixedPrecFound = true
      }
      i += 1
    }
  }


  //swap a prec that is known
  def swapIndexAndIncrementNFixedPrec(index : Int) = {
    val nFixedValue = nFixedPrec.value
    if(nFixedValue < nMaxPrecs) {
      if(nFixedValue != index) {
        val tmp = decisionPrecsIndices(nFixedValue)
        decisionPrecsIndices(nFixedValue) = decisionPrecsIndices(index)
        decisionPrecsIndices(index) = tmp
      }
      nFixedPrec.incr()
    }
  }
}
