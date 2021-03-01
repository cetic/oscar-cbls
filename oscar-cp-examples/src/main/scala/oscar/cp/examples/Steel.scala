package oscar.cp.examples

import java.awt.Color

import oscar.cp.{CPIntVar, CPModel, Strong, add, binPacking, branchAll, element, isOr, minimize, noAlternative, onSolution, post, start, startSubjectTo, sum}
import oscar.util.selectMin
import oscar.visual.plot.PlotLine
import oscar.visual.shapes.VisualLine
import oscar.visual.{VisualBinPacking, VisualFrame, VisualUtil}

import scala.io.Source
import scala.util.Random

/**
 * Model for the steel mill slab problem:
 * Steel is produced by casting molten iron into slabs.
 * A steel mill can produce a finite number, sigma, of slab sizes.
 * An order has two properties, a color corresponding to the route required through the steel mill and a weight.
 * Given d input orders, the problem is to assign the orders to slabs,
 * the number and size of which are also to be determined,
 * such that the total weight of steel produced is minimized.
 * This assignment is subject to two further constraints:
 *    - Capacity constraints: The total weight of orders assigned to a slab cannot exceed the slab capacity.
 *    - Color constraints: Each slab can contain at most p of k total colors (p is usually 2).
 * See problem 38 of http://www.csplib.org/ or http://becool.info.ucl.ac.be/steelmillslab
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object Steel extends CPModel with App {

  def readData(): (Array[Int], Array[Int], Array[Int]) = {
    val lines = Source.fromFile("data/steelMillSlab.txt").getLines.reduceLeft(_ + " " + _)
    var vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
    val nbCapa = vals.head
    vals = vals.drop(1)
    var (capa, vals_) = vals splitAt nbCapa
    capa = 0 :: capa
    val maxcapa = capa.max
    val nbCol = vals_.head
    vals_ = vals_.drop(1)
    val nbSlab = vals_.head
    vals_ = vals_.drop(1)
    var weight = Array[Int]()
    var col = Array[Int]()
    for (i <- 1 to nbSlab) {
      vals_ match {
        case w :: c :: v =>
          vals_ = vals_.drop(2)
          weight = weight :+ w
          col = col :+ c - 1 //color starts at 1 in input file
        case Nil => ()
      }
    }
    (capa toArray, weight, col)
  }

  val (capa, weight, col) = readData()
  val (nbCapa, nbSlab, nbCol) = (capa.length, weight.length, col.max + 1)
  val Slabs = 0 until nbSlab
  val Cols = 0 until nbCol
  val loss = (0 to capa.max).map(c => capa.filter(_ >= c).min - c)
  val colorOrders = Cols.map(c => (Slabs).filter(s => col(s) == c))

  val x = (for (s <- Slabs) yield CPIntVar(0 until nbSlab))
  val weightMap = (for (s <- Slabs) yield (x(s) -> weight(s))).toMap
  val l = for (s <- Slabs) yield CPIntVar(0 to capa.max)
  val xsol = Array.fill(nbSlab)(0) //current best solution

  // -------------visual components ------------
  val colors = VisualUtil.getRandomColors(nbCol, true)
  val scale = 7
  val f = VisualFrame("Steel Mill Slab")
  // creates the plot and place it into the frame
  val plot = new PlotLine("", "Solution number", "Loss")
  f.createFrame("Objective").add(plot)
  // creates the tour visu and place it into the frame
  val drawing: VisualBinPacking = VisualBinPacking(binWidth = 12)
  val items = Slabs.map(i => drawing.addItem(i, scale * weight(i))).toArray
  Slabs.foreach(o => items(o).innerCol = colors(col(o)))
  f.createFrame("Steel Mill Slab").add(drawing)
  capa.foreach(c => VisualLine(drawing, 0, c * scale, nbSlab * 12, c * scale).outerCol = Color.red)
  f.pack()
  // ------------------------------------------

  val rnd = new Random(0)
  var nbSol = 0

  val obj = sum(Slabs)(s => element(loss, l(s)))

  onSolution {
    plot.addPoint(nbSol, obj.value)
    nbSol += 1
    Slabs.foreach(o => {
      xsol(o) = x(o).value
      items(o).bin = xsol(o)
    })

  }

  add(binPacking(x, weight, l), Strong)
  for (s <- Slabs) {
    def colPresent(c: Int) = isOr((for (o <- colorOrders(c)) yield x(o) ?=== s)) //return a CPBoolVar telling whether color c is present is slab s
    add(sum(Cols)(c => colPresent(c)) <= 2) //at most two colors present in each slab
  }

  minimize(obj) search {
    selectMin(x)(!_.isBound)(x => 10000 * x.size - weightMap(x)) match {
      case None => noAlternative
      case Some(y) => {
        // dynamic symmetry breaking
        val maxUsed = x.maxBoundOrElse(-1)
        branchAll((0 to maxUsed + 1).filter(y.hasValue(_)))(v => post(y === v))
      }
    }

  }

  val stats = start(nSols = 1) // find firest feasible solution

  for (r <- 1 to 100) {
    startSubjectTo(failureLimit = 200) {
      for (s <- Slabs; if rnd.nextInt(100) > 70) {
        post(x(s) === xsol(s))
      }
    }
  }

  println("end--------------")

  println(stats)
}
