package oscar.cp.examples

import oscar.cp.{CPIntVar, CPModel, Weak, add, allDifferent, binaryFirstFail, maximize, start, sum, table}

import scala.io.Source

/**
 * Longest Path Problem
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object LongestPath extends CPModel with App {

  // --- reading the data ---

  val lines = Source.fromFile("data/longestpath/planar-n50.ins1.txt_com10_ins1").getLines.reduceLeft(_ + " " + _)

  val vals = lines.split("[ ,\t]").toList.filterNot(_ == "")
  var index = 0
  def next() = {
    index += 1
    vals(index - 1)
  }
  val nbNodes = next().toInt // last one is a dummy start/end to use circuit
  val nbArcs = next().toInt
  val nodes = 0 until nbNodes
  println("nbNodes:" + nbNodes + " nbArcs:" + nbArcs)
  val absent = -10000000
  val distMatrix = Array.fill(nbNodes, nbNodes)(absent)
  for (i <- 1 to nbArcs) {
    val from = next().toInt - 1
    val to = next().toInt - 1
    val w: Int = (next().toDouble * 100).toInt
    distMatrix(from)(to) = w
    distMatrix(to)(from) = w
  }

  def successors(i: Int): Set[Int] = nodes.filter(distMatrix(i)(_) != absent).toSet

  val outNodes = Array.tabulate(nbNodes)(i => nodes.filter(distMatrix(i)(_) != absent).toSet)
  val inNodes = Array.tabulate(nbNodes)(i => nodes.filter(distMatrix(i)(_) != absent).toSet)

  // set of valid transitions pair
  val tuples = (for (i <- nodes; j <- successors(i)) yield (i, j)).toSet

  val distMatrix_ = Array.tabulate(nbNodes, nbNodes)((i, j) => if (distMatrix(i)(j) == absent) 0 else distMatrix(i)(j))

  val len = 12 // path lenth

  println("----------- trying with length:" + len + "-------------")

  val path = Array.fill(len)(CPIntVar(nodes))
  val weight = sum(0 until len - 1)(i => distMatrix_(path(i))(path(i + 1)))

  for (i <- 0 until len - 1) {
    add(table(path(i), path(i + 1), tuples)) // for each consecutive visits, give the possible valid transitions
  }
  add(allDifferent(path), Weak)

  maximize(weight) search { binaryFirstFail(path) }

  val stats = start()
  println(stats)
}
