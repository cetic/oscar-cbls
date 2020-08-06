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
package oscar.cbls.algo.boundedArray

import scala.reflect.ClassTag

/**
  * This class represents a bounded array
  *
  * @param maxElements the maxumum number of elements in the array
  * @param postInserting a function to be executed after an element is inserted
  * @tparam T The type of the elements in the array
  */
class BoundedArray[T: ClassTag](maxElements: Int,
                                postInserting: (T, Int) => Unit = (_:T, _:Int) => {}) {
  // The actual array
  private val actualArray: Array[T] = new Array[T](maxElements)
  // the actual number of elements in the array
  private var nbElements = 0

  def size: Int = nbElements

  def maxSize: Int = maxElements

  def isEmpty: Boolean = nbElements == 0

  @inline final def apply(pos: Int): T = {
    require(0 <= pos)
    require(pos < nbElements)
    actualArray(pos)
  }

  def :+(elem: T): Unit = {
    require(nbElements < maxElements)
    actualArray(nbElements) = elem
    postInserting(elem, nbElements)
    nbElements += 1
  }

  def :::(elems: List[T]): Unit = {
    for {e <- elems} {
      this :+ e
    }
  }

  def insertAt(elem: T, pos: Int): Unit = {
    require(nbElements < maxElements)
    require(0 <= pos)
    require(pos < nbElements)
    for { i <- nbElements to pos+1 by -1} {
      actualArray(i) = actualArray(i-1)
    }
    actualArray(pos) = elem
    postInserting(elem, pos)
    nbElements += 1
  }

  def +:(elem: T): Unit = {
    if (nbElements == 0) {
      actualArray(0) = elem
      postInserting(elem, 0)
      nbElements = 1
    }
    else {
      insertAt(elem, 0)
    }
  }

  def deleteAt(pos: Int): Unit = {
    require(0 <= pos)
    require(pos < nbElements)
    for { i <- pos until nbElements-1 } {
      actualArray(i) = actualArray(i+1)
    }
    nbElements -= 1
  }

  def toIterable: Iterable[T] = {
    for { i <- 0 until nbElements } yield actualArray(i)
  }

}
