package oscar.anytime.lns.models
import oscar.cp._
import oscar.anytime.lns.Benchmark

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
class Steel(val instance: String, val bestObj: Int = 0) extends CPModel with Benchmark {

  def readData(): (Array[Int], Array[Int], Array[Int]) = {
    val lines = Source.fromFile(instance).getLines.reduceLeft(_ + " " + _)
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


  val rnd = new Random(0)
  var nbSol = 0

  val obj = sum(Slabs)(s => element(loss, l(s)))


  add(binPacking(x, weight, l), Strong)
  for (s <- Slabs) {
    def colPresent(c: Int) = isOr((for (o <- colorOrders(c)) yield x(o) ?=== s)) //return a CPBoolVar telling whether color c is present is slab s
    add(sum(Cols)(c => colPresent(c)) <= 2) //at most two colors present in each slab
  }

  minimize(obj)

  override def decisionVariables: Array[CPIntVar] = x

  override def bestKnownObjective: Option[Int] = Some(bestObj)

  override def problem: String = "Steel"
}
