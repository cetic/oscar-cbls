/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.constraints.tables

import oscar.algo.Inconsistency

import scala.collection.mutable.ArrayBuffer

class TableData(val arity: Int) {

  /*
  def this[T <: Product](items: Iterable[T]) {
    this(items.head.arity)
    items.foreach(t => add(t.iterator:_*))
  }
  */

  val data = Array.fill(arity)(ArrayBuffer[Int]())

  val firstSup = Array.fill(arity)(Array[Int]())

  val nextSup = Array.fill(arity)(Array[Int]())

  val minVal = Array.fill(arity)(0)
  val maxVal = Array.fill(arity)(0)

  def min(i: Int): Int = minVal(i)
  def max(i: Int): Int = maxVal(i)

  /**
   * Adds a possible tuple
   * @param tuple must have a length arity
   */
  def add(tuple: Int*): Unit = {
    assert(arity == tuple.length, { println("wrong arrity:" + tuple.length) })
    tuple.zipWithIndex.foreach { case (v, i) => data(i) += tuple(i) }
  }

  /**
   * retrieve entry i of tuple t
   */
  def apply(t: Int, i: Int) = {
    data(i)(t)
  }

  def setup() = {
    if (arity <= 0 || data(0).size <= 0)
      throw Inconsistency
    for (i <- 0 until arity) {
      minVal(i) = data(i).min
      maxVal(i) = data(i).max
      val nVal = maxVal(i) - minVal(i) + 1
      firstSup(i) = Array.fill(nVal)(-1)
      nextSup(i) = Array.fill(data(i).size)(-1)
      for ((v, j) <- data(i).zipWithIndex) {
        nextSup(i)(j) = firstSup(i)(v - min(i))
        firstSup(i)(v - min(i)) = j
      }
    }
  }

  def firstSupport(i: Int, value: Int) = firstSup(i)(value - min(i))

  def hasFirstSupport(i: Int, value: Int) = firstSupport(i, value) != -1

  def nextSupport(i: Int, t: Int) = {
    if (t < 0) {
      t
    } else {
      nextSup(i)(t)
    }
  }

  def hasNextSupport(i: Int, t: Int) = {
    if (t < 0) false
    else nextSup(i)(t) != -1
  }
}
