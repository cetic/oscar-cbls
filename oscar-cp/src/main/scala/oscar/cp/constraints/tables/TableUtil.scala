/** *****************************************************************************
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
  * *****************************************************************************/
package oscar.cp.constraints.tables

import oscar.cp.core.variables.CPIntVar
import scala.language.reflectiveCalls
import scala.collection.mutable.ArrayBuffer

/**
 * Class containing utility methods for tables
 * @author Pierre Schaus pschaus@gmail.com
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 */
object TableUtil {

  val lexicoArrayOrdering = new Ordering[Array[Int]] {
    def compare(x: Array[Int], y: Array[Int]): Int = {
      var i = 0
      while (i < x.length) {
        val a = x(i)
        val b = y(i)
        if (a > b)
          return 1
        if (a < b)
          return -1
        i += 1
      }
      0
    }

    def isEquals(x: Array[Int], y: Array[Int]): Boolean = {
      var i = 0
      while (i < x.length) {
        val a = x(i)
        val b = y(i)
        if (a != b)
          return false
        i += 1
      }
      true
    }
  }

  /**
   * Method taking a short table and return the equivalent table with no more star in it
   * @param x array of variables
   * @param table tuples forming the table
   * @param star integer value for the star
   * @return equivalent table containing only ground tuples
   */
  // TODO this method does not remove redundant tuples
  def decompressToGroundTable(x: Array[CPIntVar], table: Array[Array[Int]], star: Int = -1): Array[Array[Int]] = {
    val buff = new ArrayBuffer[Array[Int]]()
    val size = x.length
    def addToBuff(tuple: Array[Int], index: Int): Unit = {
      if (index < size) {
        if (tuple(index) == star) {
          for (v <- x(index).iterator) {
            val newTuple = tuple.clone()
            newTuple(index) = v
            addToBuff(newTuple, index + 1)
          }
        } else {
          addToBuff(tuple, index + 1)
        }
      } else {
        buff += tuple
      }
    }
    var i = table.length
    while (i > 0) {
      i -= 1
      addToBuff(table(i), 0)
    }
    buff.toArray
  }

  /**
   * Method taking a basic smart table and return the equivalent table with no more basic smart element in it
   * @param x array of variables
   * @param table tuples forming the table
   * @return equivalent table containing only ground tuples
   */
  // TODO this method does not remove redundant tuples
  def decompressToGroundTable(x: Array[CPIntVar], table: Array[Array[BasicSmartElement]]) = {
    val buff = new ArrayBuffer[Array[Int]]()
    val size = x.length
    def addToBuff(tupleBS: Array[BasicSmartElement], tuple: Array[Int], index: Int): Unit = {
      if (index < size) {
        tupleBS(index).foreach(x(index), v => {
          val newTuple = tuple.clone()
          newTuple(index) = v
          addToBuff(tupleBS, newTuple, index + 1)
        })
      } else {
        buff += tuple
      }
    }
    var i = table.length
    while (i > 0) {
      i -= 1
      addToBuff(table(i), Array.fill(size)(0), 0)
    }
    buff.toArray
  }

  /**
   * Method taking a short table and return the equivalent table with basic smart element instead
   * No compression nor decompression is applied
   * @param x array of variables
   * @param table tuples forming the table
   * @param star integer value for the star
   * @return equivalent table as an Array[Array[BasicSmartElement] ]
   */
  def mapToBasicSmartTable(x: Array[CPIntVar], table: Array[Array[Int]], star: Int = -1): Array[Array[BasicSmartElement]] = {
    val tableBs = Array.fill(table.length)(new Array[BasicSmartElement](x.length))
    for (i <- table.indices; j <- x.indices) {
      if (table(i)(j) == star)
        tableBs(i)(j) = Star()
      else
        tableBs(i)(j) = Equal(table(i)(j))
    }
    tableBs
  }

  def removeDuplicate(table: Array[Array[Int]]): Array[Array[Int]] = {
    if (table.length <= 1) table
    else {
      val sortedTable = sortTable(table)
      val buff = new ArrayBuffer[Array[Int]]()
      var prev = sortedTable(0)
      buff += prev
      for (i <- 1 until table.length) {
        if (!lexicoArrayOrdering.isEquals(sortedTable(i), prev)) {
          prev = sortedTable(i)
          buff += prev
        }
      }
      buff.toArray
    }
  }

  def sortTable(array: Array[Array[Int]]) = {
    scala.util.Sorting.quickSort(array)(lexicoArrayOrdering)
    array
  }


}

object testtttttt extends App {

  val z = Array(Array(1, 2), Array(1, 2))
  val t = z.distinct
  println(t.map(_.mkString(",")).mkString("\n"))


}

