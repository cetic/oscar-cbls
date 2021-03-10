package oscar.cp.scheduling.precedencegraph.branching

import oscar.algo.reversible.ReversibleInt
import oscar.algo.search.Branching
import oscar.cp.scheduling.precedencegraph.PrecedenceGraph
import oscar.cp.{Alternative, noAlternative}

import scala.util.Random

//this branching branches on all precedences (detectable included) because transition times remove the property that all detectable precedences are (fully) propagated
class DynamicPrecedenceGraphBranchingAllPrecedencesAllMachines(precGraphs: Array[_ <: PrecedenceGraph], machineNames: Array[String], varHeuristic: (Int, Int,Int) => Float, isPrecOrderPreferable: (Int, Int,Int) => Boolean) extends Branching {

  protected[this] val nMachines = precGraphs.length
  implicit private[this] val cp = precGraphs(0).store
  private[this] val nFixedPrec = new ReversibleInt(cp,0)

  protected[this] val decisionPrecs : Array[(Int,Int,Int)] = Array.tabulate(nMachines)(m => {
    val nTasksOnMachine = precGraphs(m).nTasks
    Array.tabulate(nTasksOnMachine, nTasksOnMachine)((i,j) => (i,j)).flatten.filter(t => t._1 < t._2).map(t => (m, t._1, t._2))
  }).flatten

  protected[this] val decisionPrecsIndices : Array[Int] = decisionPrecs.zipWithIndex.map(_._2)
  protected[this] val nMaxPrecs : Int = decisionPrecsIndices.length
  protected[this] var foundPrecIndexToBranchOn : Int = -1

  private[this] val bestIndices = new Array[Int](nMaxPrecs)
  private[this] var nbBestIndices = -1

  val store = cp

  val random = new Random(42)

  override def alternatives(): Seq[Alternative] = {
    findPrecIndexToBranchOn()
    if(foundPrecIndexToBranchOn == nMaxPrecs)
      return noAlternative
    else {
      val precToBranchOn = decisionPrecs(foundPrecIndexToBranchOn)
      val machine = precToBranchOn._1
      val task1 = precToBranchOn._2
      val task2 = precToBranchOn._3

      val precGraph = precGraphs(machine)
      if(precGraph.isPrecDetectable(task1,task2)) {
        oneBranch(machine, task1, task2)
      }
      else if(precGraph.isPrecDetectable(task2,task1)) {
        oneBranch(machine, task2, task1)
      }
      else if (isPrecOrderPreferable(machine, task1, task2)) {
        twoBranches(machine, task1, task2)
      }
      else {
        twoBranches(machine, task2, task1)
      }
    }
  }

  protected def oneBranch(machine: Int, task1: Int, task2: Int) = {
    val precGraph = precGraphs(machine)
    val machineName = machineNames(machine)
    List(new PrecGraphDecision(precGraph, task1, task2, machineName))
  }

  protected def twoBranches(machine: Int, task1: Int, task2: Int) = {
    val precGraph = precGraphs(machine)
    val machineName = machineNames(machine)
    val left = new PrecGraphDecision(precGraph, task1, task2, machineName)
    val right = new PrecGraphDecision(precGraph, task2, task1, machineName)
    List(left,right)
  }

  protected def varChoice(machine: Int, task1:Int, task2: Int) = varHeuristic(machine, task1, task2)

  def findPrecIndexToBranchOn() : Unit = {

    updateStartingNFixedPrecs()

    var i = nFixedPrec.value
    var bestIndex = nFixedPrec.value

    while(i < nMaxPrecs) {
      val currentPair = decisionPrecs(decisionPrecsIndices(i))
      val machine = currentPair._1
      if(precGraphs(machine).hasNonDetectablePrecForPair(currentPair._2,currentPair._3)) {
        swapIndexAndIncrementNFixedPrec(i)
        bestIndex = math.max(nFixedPrec.value, bestIndex)
      }
      else {
        val bestPrec = decisionPrecs(decisionPrecsIndices(bestIndex))
        if(varChoice(currentPair._1,currentPair._2, currentPair._3) < varChoice(bestPrec._1, bestPrec._2, bestPrec._3))
          bestIndex = i

      }
      i += 1
    }

    if(bestIndex == decisionPrecsIndices.length){
      foundPrecIndexToBranchOn = nMaxPrecs
    }
    else {
      i = nFixedPrec.value
      //remember all best indices and pick one randomly
      val bestPair = decisionPrecs(decisionPrecsIndices(bestIndex))
      nbBestIndices = 0
      while(i < nMaxPrecs) {
        val currentPair = decisionPrecs(decisionPrecsIndices(i))
        if(varChoice(currentPair._1,currentPair._2, currentPair._3) == varChoice(bestPair._1,bestPair._2, bestPair._3)) {
          bestIndices(nbBestIndices) = i
          nbBestIndices += 1
        }
        i += 1
      }

      bestIndex = bestIndices(random.nextInt(/*bestIndices.length*/nbBestIndices))

      foundPrecIndexToBranchOn = decisionPrecsIndices(bestIndex)
      swapIndexAndIncrementNFixedPrec(bestIndex)
    }
  }

  def updateStartingNFixedPrecs(): Unit = {
    var i = nFixedPrec.value
    var unfixedPrecFound = false
    while (i < nMaxPrecs && !unfixedPrecFound) {
      val currentPair = decisionPrecs(decisionPrecsIndices(i))
      val machine = currentPair._1
      if(precGraphs(machine).hasNonDetectablePrecForPair(currentPair._2,currentPair._3)) {
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

  def lastBranch = Array.tabulate(nFixedPrec.value)(i => decisionPrecs(decisionPrecsIndices(i)))

}
