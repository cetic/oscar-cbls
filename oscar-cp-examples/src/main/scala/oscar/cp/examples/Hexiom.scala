package oscar.cp.examples

import oscar.cp._
import scala.io.Source

/**
 * Hexiom Problem: http://www.kongregate.com/games/Moonkey/hexiom
 * @author Pierre Schaus pschaus@gmail.com
 */
object Hexiom extends CPModel with App {

  /* Here is an Hexagon of dimension 3 (size of first row).
   * Each entry is numbered this way:
   *
   *      00  01  02
   *
   *    03  04  05  06
   *
   *  07  08  09  10  11
   *
   *    12  13  14  15
   *
   *      16  17  18
   *
   * A method neighbors (below) allow to retrieve the set of neighbors for an entry
   * For instance neighbors(7) = {3,8,12}
   */

  val lines = Source.fromFile("data/hexiom16.txt").getLines.toArray
  val oneline = (1 until lines.size).map(lines(_)).foldLeft("")((i, j) => i + j)
  val n = lines(0).toInt // number of entries in the firt row of the hexagon

  // ------------------- build the neighbor structure ---------------------

  // we use a 2D array to represent the hexagon
  // to retrieve easily the neighbors and to pretty-print (but that's not very important)
  val dim = n * 2 - 1 + 2 * n
  val tab = Array.fill(dim, dim)(" ")
  val vals = Array.fill(dim, dim)(-1)
  var start = n
  for (i <- 0 to n - 1) {
    for (j <- 0 to (n - 1 + i)) {
      tab(i * 2)(start + j * 2) = "*"
      tab(dim - i * 2 - 1 - 2)(start + j * 2) = "*"
      vals(i * 2)(start + j * 2) = 0
      vals(dim - i * 2 - 1 - 2)(start + j * 2) = 0
    }
    start -= 1
  }
  tab.foreach(i => println(i.mkString(" ")))

  // number of positions on the map
  var k = 0
  // pos(i) is the index in the 2d array of position i
  val pos = for (i <- 0 until dim; j <- 0 until dim; if (vals(i)(j) == 0)) yield {
    vals(i)(j) = k
    k += 1
    (i, j)
  }

  // return the neighbors of entry v
  def neighbors(v: Int) = {
    val (i, j) = pos(v)
    def inBound1(t: Int) = t >= 0 && t < dim
    def inBound2(t: (Int, Int)) = inBound1(t._1) && inBound1(t._2)
    Set((i, j + 2), (i, j - 2), (i - 2, j + 1), (i - 2, j - 1), (i + 2, j + 1), (i + 2, j - 1)).filter(inBound2(_)).map(t => vals(t._1)(t._2)).filter(_ >= 0)
  }

  // ------------------- CP Model ---------------------

  // compute the number of cardinalities of every type of pawns
  val cardinalities = (0 to 6).map(i => oneline.count((i + '0').toChar ==)).toArray :+ oneline.count(('.').toChar ==)

  // used(i) = true iff there is a pawn at this position
  val used = Array.fill(k)(CPBoolVar())
  val dummy = 7 // dummy value when no pawn in the neighborhood
  // card(i) = if (used(i)): number of pawns in the neighbors else: dummy
  val card = Array.fill(k)(CPIntVar(0 to 7))
  var nbSol = 0

  val tuples = (for (i <- 0 to 6) yield (i, 0, 7)) ++ (for (i <- 0 to 6) yield (i, 1, i))
  println(tuples)
  for (i <- 0 until k) {
    val nbNeighbors = sum(neighbors(i))(used(_))
    add(table(nbNeighbors, used(i), card(i), tuples))
  }
  add(gcc(card, 0 to 6, cardinalities, cardinalities), Strong)

  search { binaryStaticIdx(used,i => used(i).max) }

  onSolution {
    println("++++++++++++++++++ solution ++++++++++++++++++++\n")
    // pretty print
    for (ind <- 0 until k; if (card(ind).value < 7)) {
      val (i, j) = pos(ind)
      tab(i)(j) = card(ind).value.toString
    }
    tab.foreach(i => println(i.mkString(" ")))
    println("++++++++++++++++++++++++++++++++++++++++++++++++\n")
  }

  val stats = solver.start()
  println(stats)
}
