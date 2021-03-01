package oscar.cp.scheduling.util

import oscar.algo.DisjointSets
import oscar.cp._
import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPIntVar

/**
 * Created on 18/03/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 * @author Sascha Van Cauwelaert (sascha.vancauwelaert@gmail.com)
 */
class TransitionLowerBounds(transitionTimeMatrix: Array[Array[Int]]) {
  val nElements = transitionTimeMatrix.length

  def getLowerBounds(): Array[Int] = {
    val kruskal = kruskalWeights()
    val dynamic = dynamicProgrammingWeights()

    Array.tabulate(nElements)(i => math.max(kruskal(i), dynamic(i)))
  }

  def minMatrix(): Array[Int] = {
    val sortedTransitions = transitionTimeMatrix.flatten.sorted.drop(nElements)
    Array.tabulate(nElements)(i => sortedTransitions.take(i).sum)
  }

  def minPerLine(): Array[Int] = {
    val minTransitionTimeByActi = Array.tabulate(nElements)(i => transitionTimeMatrix(i).sorted.apply(1))
    val sortedMinTransitionTimeByActi = minTransitionTimeByActi.sorted
    Array.tabulate(nElements)(i => sortedMinTransitionTimeByActi.take(i).sum)
  }

  // Implementation of Kruskal's algorithm in which we get the sum of the weights
  // of the edges contained in the forest at each successful iteration of the algorithm
  def kruskalWeights(): Array[Int] = {
    val forestWeights = Array.fill(nElements)(0)

    // The disjoint set representing the forest used by Kruskal's algorithm
    val disjointSets = new DisjointSets(min=0, max=nElements - 1)

    var curIndex = 0
    val edges = Array.tabulate(nElements)(i => Array.tabulate(nElements)(j => (i, j, transitionTimeMatrix(i)(j)))).flatten.filter(e => e._1 != e._2).sortBy(_._3)
    var sumOfSelectedEdges = 0
    var nEdges = 0
    while(curIndex < edges.length && nEdges < nElements - 1) {
      if (!disjointSets.inSameSet(edges(curIndex)._1, edges(curIndex)._2)) {
        nEdges += 1
        sumOfSelectedEdges += edges(curIndex)._3
        forestWeights(nEdges) = sumOfSelectedEdges
        disjointSets.union(edges(curIndex)._1, edges(curIndex)._2)
      }
      curIndex += 1
    }
    forestWeights
  }

  // Dynamic Program to compute the shortest path of k edges in a directed graph of
  // n nodes (k <= n). Note that this algorithm doesn't avoid paths with loops.
  // http://cs.stackexchange.com/questions/11503/shortest-path-with-exactly-k-edges
  def dynamicProgrammingWeights(): Array[Int] = {
    // D(m)(u) is the weight of the shortest walk of length exactly m from source
    // to u.
    val D = Array.fill(nElements)(Array.fill(nElements)(Int.MaxValue))
    for (u <- 0 until nElements) {
      D(0)(u) = 0
    }

    // Dynamic Programming:
    // D[m+1,u] = min_{x ∈ Pred(u)}(D[m,x]+w[x,u])
    for (m <- 0 until nElements - 1) {
      for (u <- 0 until nElements; x <- 0 until nElements if u != x) {
        val alt = D(m)(x) + transitionTimeMatrix(x)(u)
        if (D(m + 1)(u) > alt) {
          D(m + 1)(u) = alt
        }
      }
    }
    Array.tabulate(nElements)(i => D(i).min)
  }

  def optimalBound(timeLimit : Int = Int.MaxValue) = {
    val bestPossibleBounds = Array.fill(nElements)(0)
    val searchCompleted = Array.fill(nElements)(true)
    for (nNodes <- 2 to nElements) {
      //println("Best bound for " + nNodes + " nodes")
      val solver = CPSolver()
      solver.silent = true
      val nodeVars = Array.tabulate(nNodes)(i => CPIntVar(0 until nElements)(solver))
      val pathCost = sum(0 until nNodes - 1)(i => element(transitionTimeMatrix, nodeVars(i), nodeVars(i + 1)))

      solver.add(allDifferent(nodeVars))

      solver.search {
        binaryLastConflict(nodeVars)
      }

      solver.onSolution{
        bestPossibleBounds(nNodes - 1) = pathCost.value
      }

      solver.minimize(pathCost)
      val stats = solver.start(timeLimit = timeLimit)
      searchCompleted(nNodes - 1) = stats.completed

    }
    (bestPossibleBounds, searchCompleted)
  }

}