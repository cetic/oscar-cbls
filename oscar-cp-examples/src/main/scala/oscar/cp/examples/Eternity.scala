package oscar.cp.examples


import oscar.cp._
import oscar.util._

/**
 * Eternity Problem = Edge Matching Puzzle
 * A piece has a square shape with a color on each side.
 * The objective is to place all the (n x m) pieces on the n x m board such that two adjacent sides have the same color
 * @author Pierre Schaus pschaus@gmail.com
 */
object Eternity extends CPModel with App {

  val reader = new InFile("data/eternity8x8.txt");
  val n = reader.nextInt()
  val m = reader.nextInt()
  println("n:" + n + " m:" + m)
  val pieces = Array.tabulate(n * m)(i => Array.fill(4)(reader.nextInt()).drop(0));

  val minColor = pieces.flatten.min
  val maxColor = pieces.flatten.max

  // create the variables
  val id = Array.fill(n, m)(CPIntVar(0 until (n * m)))
  val up = Array.fill(n, m)(CPIntVar(minColor to maxColor))
  val right = Array.fill(n, m)(CPIntVar(minColor to maxColor))
  val down = Array.fill(n, m)(CPIntVar(minColor to maxColor))
  val left = Array.fill(n, m)(CPIntVar(minColor to maxColor))

  // make the link between id, orientation and up variable
  val tableData = for (i <- 0 until n * m; r <- 0 until 4) yield {
    val row = pieces(i)
    (i, row((r + 0) % 4), row((r + 1) % 4), row((r + 2) % 4), row((r + 3) % 4))
  }
  println("table size:" + tableData.size)
  for (i <- 0 until n; j <- 0 until m) {
    add(table(id(i)(j), up(i)(j), right(i)(j), down(i)(j), left(i)(j), tableData));
  }

  add(allDifferent(id.flatten), Weak);

  // force 0 on horizontal borders
  for (c <- 0 until m) {
    add(up(0)(c) === 0)
    add(down(n - 1)(c) === 0)
  }

  // force 0 on vertical borders
  for (l <- 0 until n) {
    add(left(l)(0) === 0)
    add(right(l)(m - 1) === 0)
  }

  // horizontal match on adjacent cells
  for (l <- 0 until n; c <- 0 until m - 1)
    add(right(l)(c) === left(l)(c + 1))

  // vertical match on adjacent cells
  for (l <- 0 until n - 1; c <- 0 until m) {
    add(down(l)(c) === up(l + 1)(c));
  }

  def prettyPrint(): Unit = {

    for (l <- 0 until n) {
      println(up(l).mkString("   ", "       ", "\n"))
      println(left(l).zip(right(l)).flatMap(a => Array(a._1.value, a._2.value)).mkString(" ", "   ", "\n"))
      println(down(l).mkString("   ", "       ", "\n"))

    }

    println()

  }

  search {
    binaryFirstFail(id.flatten[CPIntVar]) ++
      binaryFirstFail(up.flatten[CPIntVar]) ++
      binaryFirstFail(right.flatten[CPIntVar]) ++
      binaryFirstFail(down.flatten[CPIntVar]) ++
      binaryFirstFail(left.flatten[CPIntVar])
  }

  onSolution {
    prettyPrint()
  }

  val stat = start()
  println(stat)

}
