/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

package oscar.cp.perf



import oscar.cp._
import oscar.util._

import scala.io.Source
import scala.util.Random

/**
 * Model for the steel mill slab problem:
 * Steel is produced by casting molten iron into slabs.
 * A steel mill can produce a finite number, σ, of slab sizes.
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
object Steel {

  def readData(): (Array[Int], Array[Int], Array[Int]) = {
    val lines = Source.fromFile("data/steelMillSlabOrig.txt").getLines.reduceLeft(_ + " " + _)
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

  def main(args: Array[String]): Unit = {
    val (capa, weight, col) = readData()
    val (nbCapa, nbSlab, nbCol) = (capa.length, weight.length, col.max + 1)
    val Slabs = 0 until nbSlab
    val Cols = 0 until nbCol
    val loss = (0 to capa.max).map(c => capa.filter(_ >= c).min - c)
    val colorOrders = Cols.map(c => (Slabs).filter(s => col(s) == c))

    val cp = new CPSolver()
    cp.silent = true
    val x = (for (s <- Slabs) yield CPIntVar(cp, 0 until nbSlab))
    val weightMap = (for (s <- Slabs) yield (x(s) -> weight(s))).toMap
    val l = for (s <- Slabs) yield CPIntVar(cp, 0 to capa.max)

    val rnd = new Random(0)
    var nbSol = 0

    val obj = sum(Slabs)(s => element(loss, l(s)))
    cp.minimize(obj)
    cp.add(binPacking(x, weight, l), Strong)
    for (s <- Slabs) {
      def colPresent(c: Int) = isOr((for (o <- colorOrders(c)) yield x(o) ?=== s)) //return a CPBoolVar telling whether color c is present is slab s
      cp.add(sum(Cols)(c => colPresent(c)) <= 2) //at most two colors present in each slab
    }
    cp.search {
      selectMin(x)(!_.isBound)(x => 10000 * x.size - weightMap(x)) match {
        case None => noAlternative
        case Some(y) => {
          // dynamic symmetry breaking
          val maxUsed = x.maxBoundOrElse(-1)
          branchAll((0 to maxUsed + 1).filter(y.hasValue(_)))(v => cp.add(y === v))
        }
      }
    }

    println(cp.start())

  }
}
