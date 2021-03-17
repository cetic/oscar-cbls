package oscar.cp.examples.util.reader

import scala.io.Source

object KnapsackReader {

  def read(dataFile: String) = {

    // Read the data
    var lines = Source.fromFile(dataFile).getLines.toList.filter(_ != "")
    val n: Int = lines.head.toInt
    val N = 0 until n
    lines = lines.drop(1)
    val capa = lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray[Int]
    val capa1 = capa(0)
    val capa2 = capa(1)
    lines = lines.drop(1)


    val itemsKnapsack1: Array[(Int,Int)] =
    (for (i <- N) yield {
     val l = lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
     lines = lines.drop(1)
     (l(0),l(1)) // weight, profit
    }).toArray

    val itemsKnapsack2: Array[(Int,Int)] =
    (for (i <- N) yield {
     val l = lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
     lines = lines.drop(1)
     (l(0),l(1))
    }).toArray
    (n,capa1,capa2,itemsKnapsack1,itemsKnapsack2)
  }

  def readSolution(solFile: String): Array[(Int,Int)] = {

    // Read the data
    var lines = Source.fromFile(solFile).getLines.toList.filter(_ != "")
    (for (l <- lines) yield {
     val s = l.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
     (s(0),s(1))
    }).toArray
  }



}
