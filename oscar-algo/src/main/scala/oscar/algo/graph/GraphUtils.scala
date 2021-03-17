package oscar.algo.graph

import oscar.algo.DisjointSets
import oscar.algo.array.{ArrayHeapDouble, ArrayHeapInt}

import scala.collection.mutable

/**
 * Created on 06/03/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 * @author Charles Thomas (cftmthomas@gmail.com)
 */

/**
 * Helper object to compute various shortest path in directed or
 * undirected graphs
 */
object GraphUtils {
  val UNDEF = -1

  /**
   * @param source The source from which one computes the shortest path
   * @param destination The destination to which one computes the shortest path
   * @param previous An Array where previous(i) is the index of the previous
   *                 node from i of the shortest path from source to i
   * @return An Array containing the ordered indexes of the shortest path from source to destination
   */
  def getPath(source: Int, destination: Int, previous: Array[Int]): Array[Int] = {
    if (previous(destination) != UNDEF) {
      var path = List[Int]()
      var i = destination
      while(i != source) {
        path +:= i
        i = previous(i)
      }
      path +:= source
      path.toArray
    }
    else {
      Array[Int]()
    }
  }

  /**
   * This method computes the shortest path from a source to a sink
   * with regards to the edgeCosts matrix using the famous Dijkstra
   * algorithm (http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm).
   * These edge costs must be positive otherwise the
   * Bellman–Ford–Moore algorithm must be used.
   *
   * @param source Index representing the source node
   * @param edgeCosts Matrix of the size nodes x nodes where each entry
   *                  (i)(j) represents the cost of the edge going from
   *                  node i to node j. If there is no edge from i to j,
   *                  then the entry must be Int.MaxValue.
   * @return An array of pairs (path, cost) where, for index i, path is,
   *         the succession of index of nodes in the shortest path from
   *         the source to the node i;
   *         cost is the sum of the edge costs along this shortest path
   */
  def dijkstra(source: Int, edgeCosts: Array[Array[Int]]): Array[(Array[Int], Int)] = {
    val n = edgeCosts.length
    val distances = Array.fill(n)(Int.MaxValue)
    val previous = Array.fill(n)(UNDEF)
    distances(source) = 0
    previous(source) = source
    val Q = new ArrayHeapInt(n)
    for (i <- 0 until n) {
      Q.enqueue(distances(i), i)
    }

    while(!Q.isEmpty) {
      val u = Q.dequeue()
      for (v <- 0 until n if u != v && edgeCosts(u)(v) < Int.MaxValue) {
        val alt = distances(u) + edgeCosts(u)(v)
        if (alt < distances(v)) {
          Q.changeKey(distances(v), alt, v)
          distances(v) = alt
          previous(v) = u
        }
      }
    }

    Array.tabulate(n)(i => (getPath(source, i, previous), distances(i)))
  }

  /**
   * This method computes the shortest path from a source to a sink
   * with regards to the edgeCosts matrix using the famous Dijkstra
   * algorithm (http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm).
   * These edge costs must be positive otherwise the
   * Bellman–Ford–Moore algorithm must be used.
   *
   * @param source Index representing the source node
   * @param edgeCosts Matrix of the size nodes x nodes where each entry
   *                  (i)(j) represents the cost of the edge going from
   *                  node i to node j. If there is no edge from i to j,
   *                  then the entry must be Int.MaxValue.
   * @return An array of pairs (path, cost) where, for index i, path is,
   *         the succession of index of nodes in the shortest path from
   *         the source to the node i;
   *         cost is the sum of the edge costs along this shortest path
   */
  def dijkstra(source: Int, edgeCosts: Array[Array[Double]]): Array[(Array[Int], Double)] = {
    val n = edgeCosts.length
    val distances = Array.fill(n)(Double.MaxValue)
    val previous = Array.fill(n)(UNDEF)
    distances(source) = 0
    previous(source) = source
    val Q = new ArrayHeapDouble(n)
    for (i <- 0 until n) {
      Q.enqueue(distances(i), i)
    }

    while(!Q.isEmpty) {
      val u = Q.dequeue()
      for (v <- 0 until n if u != v && edgeCosts(u)(v) < Double.MaxValue) {
        val alt = distances(u) + edgeCosts(u)(v)
        if (alt < distances(v)) {
          Q.changeKey(distances(v), alt, v)
          distances(v) = alt
          previous(v) = u
        }
      }
    }

    Array.tabulate(n)(i => (getPath(source, i, previous), distances(i)))
  }

  /**
   * This method computes the shortest path from a source to a sink
   * with regards to the edgeCosts matrix using the famous Bellman-Ford-Moore
   * algorithm (http://en.wikipedia.org/wiki/Bellman%E2%80%93Ford_algorithm).
   * These edge costs can be negative but the graph should not contain any
   * negative-weight cycle.
   * If the graph does not contain any negative edge, Dijkstra algorithm
   * should be preferred (it will be faster).
   *
   * @param source Index representing the source node
   * @param edgeCosts Matrix of the size nodes x nodes where each entry
   *                  (i)(j) represents the cost of the edge going from
   *                  node i to node j. If there is no edge from i to j,
   *                  then the entry must be Int.MaxValue.
   * @return An array of pairs (path, cost) where, for index i, path is,
   *         the succession of index of nodes in the shortest path from
   *         the source to the node i;
   *         cost is the sum of the edge costs along this shortest path
   * @throws NegativeWeightCycleException If there is a negative-weight cycle detected
   */
  def bellmanFordMoore(source: Int, edgeCosts: Array[Array[Int]]): Array[(Array[Int], Int)] = {
    // Step 1: initialize graph
    val n = edgeCosts.length
    val distances = Array.fill(n)(Int.MaxValue)
    val previous = Array.fill(n)(UNDEF)
    val newWeights = Array.fill(n)(-1)
    distances(source) = 0
    previous(source) = source
    newWeights(source) = 0

    // Step 2: relax edges repeatedly
    var weightChanged = true
    var i = 1
    while (i < n && weightChanged) {
      weightChanged = false
      for (u <- 0 until n if newWeights(u) == i - 1; v <- 0 until n if edgeCosts(u)(v) < Int.MaxValue) {
        val alt = distances(u) + edgeCosts(u)(v)
        if (alt < distances(v)) {
          distances(v) = alt
          previous(v) = u
          newWeights(v) = i
          weightChanged = true
        }
      }
      i += 1
    }

    // Step 3: check for negative-weight cycles
    for (u <- 0 until n; v <- 0 until n if edgeCosts(u)(v) < Int.MaxValue &&
      distances(u) < Int.MaxValue && distances(v) < Int.MaxValue) {
      if (distances(u) + edgeCosts(u)(v) < distances(v)) {
        throw new NegativeWeightCycleException(
          "The Bellman-Ford-Moore algorithm detected a negative-weight cycle in the graph")
      }
    }

    Array.tabulate(n)(i => (getPath(source, i, previous), distances(i)))
  }

  /**
   * This method computes the shortest path from a source to a sink
   * with regards to the edgeCosts matrix using the famous Bellman-Ford-Moore
   * algorithm (http://en.wikipedia.org/wiki/Bellman%E2%80%93Ford_algorithm).
   * These edge costs can be negative but the graph should not contain any
   * negative-weight cycle.
   * If the graph does not contain any negative edge, Dijkstra algorithm
   * should be preferred (it will be faster).
   *
   * @param source Index representing the source node
   * @param edgeCosts Matrix of the size nodes x nodes where each entry
   *                  (i)(j) represents the cost of the edge going from
   *                  node i to node j. If there is no edge from i to j,
   *                  then the entry must be Int.MaxValue.
   * @return An array of pairs (path, cost) where, for index i, path is,
   *         the succession of index of nodes in the shortest path from
   *         the source to the node i;
   *         cost is the sum of the edge costs along this shortest path
   * @throws NegativeWeightCycleException If there is a negative-weight cycle detected
   */
  def bellmanFordMoore(source: Int, edgeCosts: Array[Array[Double]]): Array[(Array[Int], Double)] = {
    // Step 1: initialize graph
    val n = edgeCosts.length
    val distances = Array.fill(n)(Double.MaxValue)
    val previous = Array.fill(n)(UNDEF)
    val newWeights = Array.fill(n)(-1)
    distances(source) = 0.0
    previous(source) = source
    newWeights(source) = 0

    // Step 2: relax edges repeatedly
    var weightChanged = true
    var i = 1
    while (i < n && weightChanged) {
      weightChanged = false
      for (u <- 0 until n if newWeights(u) == i - 1; v <- 0 until n if edgeCosts(u)(v) < Double.MaxValue) {
        val alt = distances(u) + edgeCosts(u)(v)
        if (alt < distances(v)) {
          distances(v) = alt
          previous(v) = u
          newWeights(v) = i
          weightChanged = true
        }
      }
      i += 1
    }

    // Step 3: check for negative-weight cycles
    for (u <- 0 until n; v <- 0 until n if edgeCosts(u)(v) < Double.MaxValue&&
      distances(u) < Double.MaxValue && distances(v) < Double.MaxValue) {
      if (distances(u) + edgeCosts(u)(v) < distances(v)) {
        throw new NegativeWeightCycleException(
          "The Bellman-Ford-Moore algorithm detected a negative-weight cycle in the graph")
      }
    }

    Array.tabulate(n)(i => (getPath(source, i, previous), distances(i)))
  }

  /**
   * Kruskal (https://en.wikipedia.org/wiki/Kruskal%27s_algorithm) implementation to compute a MST for a graph
   */
  def kruskal(nodes: Seq[Int], edges: Seq[(Int, Int)], edgeCosts: Array[Array[Int]]): (Seq[(Int, Int)], Int) = {
    implicit object EdgeOrdering extends Ordering[(Int, Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = edgeCosts(y._1)(y._2) compare edgeCosts(x._1)(x._2)
    }
    val pq = mutable.PriorityQueue(edges: _*)
    val uf = new DisjointSets(0, nodes.length)
    val nodeIndex = nodes.zipWithIndex.toMap
    val mst = mutable.ArrayBuffer[(Int, Int)]()
    var weight = 0

    while(pq.nonEmpty && mst.length < nodes.length - 1){
      val (i, j) = pq.dequeue()
      val iIdx = nodeIndex(i)
      val jIdx = nodeIndex(j)
      if(!uf.inSameSet(iIdx, jIdx)){
        uf.union(iIdx, jIdx)
        mst += ((i, j))
        weight += edgeCosts(i)(j)
      }
    }

    (mst.toSeq, weight)
  }

  def kruskal(nodes: Seq[Int], edgeCosts: Array[Array[Int]]): (Seq[(Int, Int)], Int) = kruskal(nodes, for(i <- nodes; j <- nodes if i != j) yield (i, j), edgeCosts)
}

class NegativeWeightCycleException(msg: String) extends IllegalArgumentException

