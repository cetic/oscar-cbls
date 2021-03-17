package oscar.cp.examples

import oscar.cp.{CPIntVar, CPModel, add, binPacking, binaryFirstFail, maximum, minimize, start}

/**
 * Problem statement :
 * Problem coming from
 * http://www.emn.fr/z-info/sdemasse/gccat/Kassignment_to_the_same_set_of_values.html#uid3286
 *
 * Each task, is divided into subtasks and each subtask has an associated weight.
 *
 *
 * We consider 9 bins, each with capacity 5 that are partitioned disjoint groups of bins
 * {2,3,7} , {0,4}, {5,6}, and {1,8}.
 *
 * The objective is to assign the substasks to the bins enforcing the fact that all subtasks
 * that are associated with the same task are assigned the same group of bins.
 * In addition, the maximum sum of the weights of the subtasks that are assigned the same bin
 * should be minimized
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object ResourceAssignment extends CPModel with App {

  val binCapa = 20
  val partition = Array(Set(2, 3, 7), Set(0, 4), Set(5, 6), Set(1, 8))

  val taskWeight = Array((0, 3), (0, 3), (0, 3), (0, 2), (0, 2), (0, 2),
    (1, 3), (1, 3), (1, 2), (1, 2),
    (2, 2), (2, 2), (2, 3), (2, 2), (2, 1),
    (3, 3), (3, 2), (3, 2), (3, 2), (3, 1),
    (4, 3), (4, 2), (4, 1)) // for each subtask: (super task, weight)

  val nbBins = partition.flatten.max + 1
  val nbTasks = taskWeight.map(_._1).max + 1

  // p(t) is the partition chosen for task i
  val p = Array.fill(nbTasks)(CPIntVar(0 until partition.size))

  // x(i) is the bin chosen for subtask i
  val x: Array[CPIntVar] = for ((i, j) <- taskWeight) yield {
    val possbin = Array.tabulate(partition.size)(i => CPIntVar(partition(i)))
    val xij = CPIntVar(0 until nbBins)
    possbin(p(i))
  }

  val load = Array.fill(nbBins)(CPIntVar(0 to binCapa))

  add(binPacking(x, taskWeight.map(_._2), load))

  val objective = maximum(0 until nbBins)(load(_))

  minimize(objective) search {
    binaryFirstFail(x)
  }

  val stats = start()
  println(stats)
}
