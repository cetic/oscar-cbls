package oscar.cp.examples.util.reader

import scala.io.Source

object QAPReader {


  def read(dataFile: String, n: Int) = {
    val N = 0 until n
    // Read the data
    var lines = Source.fromFile(dataFile).getLines.toList.filter(_ != "")
    lines = lines.drop(1)
    var w1: Array[Array[Int]] = Array() //weight matrix 1
    var w2: Array[Array[Int]] = Array() //weight matrix 2
    var d: Array[Array[Int]] = Array() //distance matrix
    for (i <- N) {
      d = d :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      lines = lines.drop(1)
    }
    for (i <- N) {
      w1 = w1 :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      lines = lines.drop(1)
    }
    for (i <- N) {
      w2 = w2 :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      lines = lines.drop(1)
    }
    (d,w1,w2)
  }

  def readSolutions(solFile: String, n: Int) = {
    var lines = Source.fromFile(solFile).getLines.toList.filter(_ != "")
    for (l <- lines) yield {
      val ar = l.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      (ar.take(n), ar.drop(n))
    }
  }

  def readSolutions2(solFile: String, n: Int) = {
    var lines = Source.fromFile(solFile).getLines.toList.filter(_ != "")
    for (l <- lines) yield {
      val ar = l.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      (ar.drop(2), ar.take(2))
    }
  }

}
