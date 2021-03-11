package oscar.util.instanceGenerators.utils

/**
 * Created by saschavancauwelaert on 03/04/15.
 */
object MatrixUtils {

  /*
   * Floyd–Warshall algorithm
   */
  def allPairShortestPath(matrix : Array[Array[Int]]) : Array[Array[Int]] = {
    val n = matrix.length
    assert(matrix.forall(_.length == n))
    var i = 0
    var j = 0
    var k = 0

    val dist = Array.ofDim[Int](n, n)
    while (i < n) {
      j = 0
      while (j < n) {
        dist(i)(j) = matrix(i)(j)
        j += 1
      }
      i += 1
    }

    i = 0
    j = 0
    k = 0
    while (k < n) {
      i = 0
      while (i < n) {
        j = 0
        while (j < n) {
          dist(i)(j) = math.min(dist(i)(j), dist(i)(k) + dist(k)(j))
          j += 1
        }
        i += 1
      }
      k += 1
    }
    dist
  }

  def allPairShortestPathSlow(matrix : Array[Array[Int]]) : Array[Array[Int]] = {

    val n = matrix.length
    val vertices = 0 until n
    assert(matrix.forall(_.length == n))

    val dist = matrix.map(_.clone)
    for(k <- vertices ; i <- vertices ; j <- vertices; if dist(i)(j) > dist(i)(k) + dist(k)(j))
      dist(i)(j) = dist(i)(k) + dist(k)(j)
    dist
  }

  def isRespectingTriangularInequality(matrix: Array[Array[Int]]): Boolean = {
    val n = matrix.length
    var i, j, k = 0
    while (k < n) {
      i = 0
      while (i < n) {
        j = 0
        while (j < n) {
          if (matrix(i)(j) > matrix(i)(k) + matrix(k)(j)) {
            return false
          }
          j += 1
        }
        i += 1
      }
      k += 1
    }
    true
  }



}
