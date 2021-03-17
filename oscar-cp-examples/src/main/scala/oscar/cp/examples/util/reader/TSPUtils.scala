package oscar.cp.examples.util.reader

import oscar.util.OutFile

import scala.io.Source

object TSPUtils {

  def readSet(filepath: String): Array[Array[Int]] = {
    val lines = Source.fromFile(filepath).getLines.toArray
    lines.map(l => l.trim.split("[ ,\t]").map(_.toInt))
  }

  def writeSet(filepath: String, set: Array[Array[Int]]): Unit = {
    val out = OutFile(filepath)
    set.foreach(l => out.writeln(l.mkString(" ")))
    out.close()
  }

  def buildSuccFromPred(pred: Array[Int]): Array[Int] = {
    val succ = new Array[Int](pred.size)
    for (i <- 0 until succ.size) succ(pred(i)) = i
    succ
  }

  def buildSuccsFromPreds(preds: Array[Array[Int]]): Array[Array[Int]] = {
    preds.map(pred => buildSuccFromPred(pred))
  }

  def buildRealDistMatrix(instance: String) = {
    val coord = parseCoordinates(instance)
    Array.tabulate(coord.size, coord.size)((i, j) => getDist(coord(i), coord(j)))
  }

  def buildRealDistMatrix(coord: Array[(Int, Int)]) = {
    Array.tabulate(coord.size, coord.size)((i, j) => getDist(coord(i), coord(j)))
  }

  def buildDistMatrix(instance: String) = {
    buildRealDistMatrix(instance).map(_.map(nint(_).toInt))
  }

  def buildDistMatrix(coord: Array[(Int, Int)]) = {
    Array.tabulate(coord.size, coord.size)((i, j) => nint(getDist(coord(i), coord(j))).toInt)
  }

  def computeDist(pred: Array[Int], distMatrix: Array[Array[Int]]): Int = {
    var dist = 0
    for (i <- 0 until pred.size) dist += distMatrix(i)(pred(i))
    dist
  }

  // Computes the distance between two cities
  def getDist(p1: (Int, Int), p2: (Int, Int)): Double = {
    val dx = p2._1 - p1._1
    val dy = p2._2 - p1._2
    math.sqrt(dx * dx + dy * dy)
  }

  def parseCoordinates(filepath: String): Array[(Int, Int)] = {
    var lines = Source.fromFile(filepath).getLines.toList
    lines = lines.drop(6)
    val l = lines.head.trim().split("[ ,\t]+").map(_.toInt).toArray
    val n = lines.size - 1
    val coordinates = Array.tabulate(n)(i => {
      val l = lines.head.trim.split("[ ,\t]+").map(_.toInt).toArray
      val x = l(1)
      val y = l(2)
      lines = lines.drop(1)
      (x, y)
    })
    coordinates
  }

  def nint(x: Double): Int = {
    val i = math.floor(x).toInt
    val d = i + 0.5
    if (x > d) i + 1
    else if (x < d) i
    else if (i % 2 == 0) i
    else i + 1
  }


}
