package oscar.cp.examples

import oscar.algo.Inconsistency
import oscar.cp._
import oscar.algo.reversible._

import scala.io.Source
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar

/**
 * Binero is a grid game, similar to Sudoku.
 * You get a 2n x 2n grid which must be filled with ones and zeroes.
 * Some of the symbols are given. Empty squares are represented with -1.
 * - On each line and each column there must be as many zeros as ones.
 * - No more than 2 zeros or ones can be placed on a line or a column consecutively.
 * - Identical columns or lines are forbidden.
 *
 * @author VictorJavAdore
 *
 * Input examples :
 * - First line : an integer n, the half dimension of the grid
 * - Next 2n lines : original state of the grid (a dot means a blank square)
 * 	5
 * 	0.11.....1
 * 	..1....0.0
 * 	.0..1.1.1.
 * 	.1........
 * 	1.0..1....
 * 	.0..0...11
 * 	...1.....1
 * 	0..1.1.0..
 * 	..0.......
 * 	11....11.0
 *
 */
object Binero extends CPModel with App {

  val firstLine :: restLines = Source.fromFile("data/binero2.txt").getLines.toList
  val n = firstLine.toInt // The grid's half size

  val range = 0 until 2 * n
  val rangeArr = (0 until 2 * n).toArray

  val origGrid = restLines.map(line => {
    line.map {
      case '0' => 0
      case '1' => 1
      case '.' => -1
    }.toArray
  }).toArray

  val grid = for (i <- rangeArr; j <- rangeArr) yield CPIntVar(0 to 1) // The variable grid

  // Arrays containing the elements of the lines and columns of the variable grids
  val line = for (i <- rangeArr) yield grid.slice(i * 2 * n, (i + 1) * 2 * n)
  val column = for (i <- rangeArr) yield (for (j <- rangeArr) yield grid(j * 2 * n + i))

  onSolution {
    // Printing the solution
    for (i <- range) println(grid.slice(2 * n * i, 2 * n * (i + 1)).mkString(" "))
    println
  }

  // The solution must contain the elements of the input grid
  for (i <- range; j <- range; if (origGrid(i)(j) != -1)) {
    add(grid(2 * n * i + j) === origGrid(i)(j))
  }

  for (i <- range) {
    // Each line must contain exactly n zeroes (and ones)
    add(gcc(line(i), 0 to 1, n, n))
    add(gcc(column(i), 0 to 1, n, n))
    // There can't be more than 2 ones or zeroes consecutively
    add(regular(line(i), stretchAutomaton(line(i), 1, 2)))
    add(regular(column(i), stretchAutomaton(column(i), 1, 2)))
    // All lines and all columns must be different
    for (j <- i + 1 until 2 * n) {
      add(new TabNotEqual(line(i), line(j), 2 * n))
      add(new TabNotEqual(column(i), column(j), 2 * n))
    }
  }

  search { binaryFirstFail(grid) }

  val stat = start() // find all solutions

  println("Number of solutions : " + stat.nSols)
  println(stat)
}

/**
 * Custom constraint which obliges two arrays to be different
 */
class TabNotEqual(val tab1: Array[CPIntVar], val tab2: Array[CPIntVar], val len: Int) extends Constraint(tab1(0).store) {


  val valuesBin = Array(new ReversibleInt(s, 0), new ReversibleInt(s, 0))
  val numBound = Array(new ReversibleInt(s, 0), new ReversibleInt(s, 0))

  override def setup(l: CPPropagStrength): Unit = {
    if (tab1.length != len || tab2.length != len)
      return

    for ((v, i) <- (tab1 ++ tab2).zipWithIndex) {
      if (v.isBound) {
        valBindIdx(v, i)
        if(!isActive)
          return
      }
      else
        v.callValBindIdxWhenBind(this, i)
    }
  }

  override def valBindIdx(x: CPIntVar, i: Int): Unit = {
    valuesBin(i / len).value += x.min * intPow(2, i % len)
    numBound(i / len).incr()
    if (numBound(0).value == len && numBound(1).value == len) {
      if (valuesBin(0).value == valuesBin(1).value)
        throw Inconsistency
      else
        deactivate()
    }
  }

  /**
   * Integer power function
   * @param a an integer
   * @param e the exponent
   * @return a^e, (a up to e)
   */
  def intPow(a: Int, e: Int): Int = if (e == 0) 1 else a * intPow(a, e - 1)

  override def associatedVars(): Iterable[CPVar] = ???
}

