package oscar.cp.examples


import oscar.cp._

/**
 *
 * Where are the dominoes ? (a problem that I found on this blog: http://blog.tanyakhovanova.com/?p=385)
 * A set of all 21 dominoes has been placed in a 7 by 6 rectangular tray.
 * The layout is shown with the pips replaced by numbers and domino edges removed.
 * Draw the edges of the dominoes into the diagram to show how they are positioned (every domino is different).
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object Domino extends CPModel with App {

  val nLines = 6
  val nCols = 7
  val Lines = 0 until nLines
  val Cols = 0 until nCols

  val values = Array(
    Array(2, 4, 6, 4, 5, 2, 5),
    Array(4, 1, 2, 1, 3, 2, 5),
    Array(4, 1, 6, 3, 3, 2, 6),
    Array(5, 6, 6, 1, 4, 3, 6),
    Array(3, 1, 5, 1, 1, 2, 3),
    Array(5, 6, 2, 3, 5, 4, 4))

  // gives a unique integer id to a domino, t = the two numbers on the domino
  def dominoId(t: (Int, Int)) = t._1.min(t._2) * 9 + t._1.max(t._2)
  // convert a coord entry to an integer index
  def toIndex(i: Int, j: Int) = i * nCols + j
  // check if i,j is a valid coord in values
  def inBound(t: (Int, Int)) = t._1 >= 0 && t._1 < nLines && t._2 >= 0 && t._2 < nCols
  // for an entry (i,j) the neighbor entries
  def neighbors(i: Int, j: Int) = Set((i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1)).filter(inBound(_))
  // for an entry (i,j) the neighbor indices
  def neighborIndices(i: Int, j: Int) = neighbors(i, j).map(t => toIndex(t._1, t._2))
  // for an entry (i,j) the neighbor values (up,down, left, right)
  def neighborValues(i: Int, j: Int) = neighbors(i, j).map(t => values(t._1)(t._2))

  // for each entry (i,j), what is the other entry forming a domino with it
  val matchedNeighbor = Array.tabulate(nLines, nCols)((i, j) => CPIntVar(neighborIndices(i, j)))

  // for each domino side (i,j) what is the id of it's domino in the solution
  val id = Array.tabulate(nLines, nCols)((i, j) => CPIntVar(neighborValues(i, j).map(dominoId(_, values(i)(j)))))

  def sameDomino(i: Int, j: Int, k: Int, l: Int) = id(i)(j).value == id(k)(l).value

  onSolution {
    for (i <- Lines) {
      for (j <- Cols) {
        print(values(i)(j))
        if (j != nCols - 1) print(if (sameDomino(i, j, i, j + 1)) "-" else " ")
      }
      println()
      if (i != nLines - 1) {
        for (j <- Cols) {
          print(if (sameDomino(i, j, i + 1, j)) "| " else "  ")
        }
      }
      println()
    }
  }

  for (i <- Lines; j <- Cols) {
    val validTuples = for ((k, l) <- neighbors(i, j)) yield (toIndex(k, l), dominoId(values(i)(j), values(k)(l)))
    // makes the link between the matchedNeighbor and the id of the domino
    add(table(matchedNeighbor(i)(j), id(i)(j), validTuples))
    //  an entry is matched to another neighbor entry iff the other is also matched with it (reciprocity)
    //  This translates as x[i] == j <=> x[j] == i or with element constraint x[x[i]] = i
    add(elementVar(matchedNeighbor.flatten, matchedNeighbor(i)(j), toIndex(i, j)), Strong)
  }
  // each domino can appear at most once so each domino id can appear at most twice
  add(gcc(id.flatten, 0 to 9 * 9, 0, 2))

  search(binaryFirstFail(id.flatten.toSeq))

  val stats = start()

  println(stats)
}
