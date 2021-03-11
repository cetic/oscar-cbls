package oscar.cp.scheduling.precedencegraph.branching

import oscar.algo.reversible.ReversibleArrayStack
import oscar.cp._
import oscar.cp.scheduling.precedencegraph.PrecedenceGraphWithNoGoods

class HebrardHeuristic(val precGraphs: Array[PrecedenceGraphWithNoGoods], machineNames: Array[String], var isPrecOrderPreferable: (Int, Int,Int) => Boolean, startVars: Array[CPIntVar], machineOf: Array[Int]) extends DynamicPrecedenceGraphBranchingAllPrecedencesAllMachines(precGraphs, machineNames, (i:Int, j: Int, k:Int) => i, isPrecOrderPreferable) {

  val nActivities = precGraphs.map(_.nTasks).sum

  val startsByMachine = Array.tabulate(nMachines)(m => (0 until nActivities).filter(a => machineOf(a) == m).map(startVars(_)))

  val weights = Array.tabulate(nMachines)(m => {
    val nTasksOnMachine = precGraphs(m).nTasks
    Array.tabulate(nTasksOnMachine)(t1 => Array.fill(nTasksOnMachine)(1))
  })

  val decisionStackForNoGood = new ReversibleArrayStack[PrecGraphDecisionForNoGood](store)

  def clearWeights() = {
    var m = 0
    while(m < nMachines) {
      val nTasksOnMachine = precGraphs(m).nTasks
      var t1 = 0
      while(t1 < nTasksOnMachine) {
        var t2 = 0
        while(t2 < nTasksOnMachine) {
          weights(m)(t1)(t2) = 1
          t2 += 1
        }
        t1 += 1
      }
      m += 1
    }
  }

  def decayWeights(factor:Double) = {
    var m = 0
    while(m < nMachines) {
      val nTasksOnMachine = precGraphs(m).nTasks
      var t1 = 0
      while(t1 < nTasksOnMachine) {
        var t2 = 0
        while(t2 < nTasksOnMachine) {
          weights(m)(t1)(t2) = math.max(1,weights(m)(t1)(t2)*factor.toInt)
          t2 += 1
        }
        t1 += 1
      }
      m += 1
    }
  }

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
      if (isPrecOrderPreferable(machine, task1, task2)) {
        twoBranches(machine, task1, task2)
      }
      else {
        twoBranches(machine, task2, task1)
      }
    }
  }

  override def oneBranch(machine: Int, task1: Int, task2: Int) = {
    val precGraph = precGraphs(machine)
    val machineName = machineNames(machine)
    List(new PrecGraphDecisionForNoGood(precGraph, machine, task1, task2, machineName, () => incrementWeight(machine,task1,task2), true, decisionStackForNoGood))
  }

  override def twoBranches(machine: Int, task1: Int, task2: Int) = {
    val precGraph = precGraphs(machine)
    val machineName = machineNames(machine)
    val left = new PrecGraphDecisionForNoGood(precGraph, machine, task1, task2, machineName, () => incrementWeight(machine,task1,task2), true, decisionStackForNoGood)
    val right = new PrecGraphDecisionForNoGood(precGraph, machine, task2, task1, machineName, () => incrementWeight(machine,task2,task1), false, decisionStackForNoGood)
    List(left,right)
  }

  override def varChoice(machine: Int, task1:Int, task2: Int) = {
    val w = weights(machine)(task1)(task2) + weights(machine)(task2)(task1)
    (startsByMachine(machine)(task1).size + startsByMachine(machine)(task2).size).toFloat/w
  }

  def incrementWeight(machine: Int, task1: Int, task2: Int) = weights(machine)(task1)(task2) += 1


}
