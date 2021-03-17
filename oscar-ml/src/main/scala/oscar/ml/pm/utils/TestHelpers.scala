package oscar.ml.pm.utils

import scala.io.Source

object TestHelpers {
  def printMat(mat: Array[Array[Int]]): Unit =
    println(mat.map(_.mkString(", ")).mkString("\n"))

  def checkArray(input: Array[Array[Int]], output: Array[Array[Int]]): Boolean = {
    if (input.length != output.length) return false

    var i = 0
    while (i < input.length) {
      if (!(input(i) sameElements output(i)))
        return false
      i += 1
    }

    true
  }

  def checkArray(input: Array[Array[String]], output: Array[Array[String]]): Boolean = {
    if (input.length != output.length) return false

    var i = 0
    while (i < input.length) {
      if (!(input(i) sameElements output(i)))
        return false
      i += 1
    }

    true
  }

  def readSols(filename: String): Map[String, Int] =
    Source.fromFile(filename).getLines
      .map(line => line.split(" #SUP: "))
      .map { case Array(itemset, support) => itemset -> support.toInt }
      .toMap

  def chi2(a: Int, b: Int, A: Int, B: Int): Double = {

    def p(X: Int, Y: Int, x: Int, y: Int): Double =
      (x + y) * X * 1.0 / (X + Y)

    def q(X: Int, Y: Int, x: Int, y: Int): Double =
      if (p(X, Y, x, y) != 0)
        Math.pow(x - p(X, Y, x, y), 2) / p(X, Y, x, y)
      else
        0.0

    def Q(X: Int, Y: Int, x: Int, y: Int): Double =
      q(X, Y, x, y) + q(Y, X, y, x)

    Q(A, B, a, b) + Q(A, B, A - a, B - b)
  }

  def gain(a: Int, b: Int, A: Int, B: Int): Double = {
    def nlogn(x: Double): Double =
      if (x > 0.0) -Math.log(x) * x else 0.0

    def H(p: Double): Double =
      nlogn(p) + nlogn(1 - p)

    def f(x: Int, y: Int): Double =
      if ((x + y) != 0) x * 1.0 / (x + y) else 0.0

    H(f(A, B)) - ((a + b) * 1.0 / (A + B)) * H(f(a, b)) - ((A + B - a - b) * 1.0 / (A + B)) * H(f(A - a, B - b))
  }


  def gini(a: Int, b: Int, A: Int, B: Int): Double = {
    def G(x: Double): Double =
      1 - x * x - (1 - x) * (1 - x)

    def f(x: Int, y: Int): Double =
      if ((x + y) != 0) x * 1.0 / (x + y) else 0.0

    G(f(A, B)) - ((a + b) * 1.0 / (A + B)) * G(f(a, b)) - ((A + B - a - b) * 1.0 / (A + B)) * G(f(A - a, B - b))
  }

  // Testing precomputed data structures
  def testPrecomputedDatastructures(data: Dataset): (Array[Int], Array[Array[Int]], Array[Array[Int]], Array[Array[Int]]) = {
    val nSeq = data.nbTrans
    val nItems = data.nbItem

    val lastPosMap = Array.ofDim[Int](nSeq,nItems+1)
    val firstPosMap = Array.ofDim[Int](nSeq,nItems+1)
    val itemsSupport = Array.ofDim[Int](nItems)

    var sid = 0
    while (sid < data.nbTrans) {
      var j = 0
      val sequence = data.rawDatas(sid).data
      val len = sequence.length
      val visitedItem = Array.ofDim[Boolean](nItems + 1)
      while (j < len) {
        val item = sequence(j)
        if (lastPosMap(sid)(item) < j + 1) {
          lastPosMap(sid)(item) = j + 1
        }
        j += 1
        if (!visitedItem(item)) {
          firstPosMap(sid)(item) = j //+ 1
          itemsSupport(item) += 1
          visitedItem(item) = true
        }
      }
      sid += 1
    }

    val lastPosList = lastPosMap.map(_.filter(_ > 0).sortWith(_ > _))

    (itemsSupport, firstPosMap, lastPosMap, lastPosList)
  }
}
