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

package oscar.algo


import scala.annotation.tailrec

/**
  * @author Pierre Schaus pschaus@gmail.com
  */
class DisjointSets[A](min: Int, max: Int) {

  val all = Array.tabulate(max - min + 1)(i => makeSet(min + i))

  def reset(): Unit = {
    var i = 0
    while (i < all.size) {
      all(i).reset
      i += 1
    }
  }

  /**
    * for each value in min..max, store some initial data(i) in the sets
    */
  def resetAndSetData(data: Int => A): Unit = {
    var i = 0
    while (i < all.size) {
      all(i).reset
      all(i).data = Some(data(i + min))
      i += 1
    }
  }

  def union(v1: Int, v2: Int, data: A): Unit = {
    union(all(v1 - min), all(v2 - min), Some(data))
  }

  def union(v1: Int, v2: Int): Unit = {
    union(all(v1 - min), all(v2 - min), None)
  }

  def inSameSet(v1: Int, v2: Int) = {
    findSet(all(v1 - min)) == findSet(all(v2 - min))
  }

  def find(v: Int) = {
    findSet(all(v - min))
  }

  class Set(private val elem: Int) {
    var max = elem
    var min = elem
    var rank: Int = 0
    var parent: Set = this
    var data: Option[A] = None

    def reset(): Unit = {
      max = elem
      min = elem
      rank = 0
      parent = this
      data = None
    }

  }

  def union(x: Set, y: Set, data: Option[A]): Unit = {
    link(findSet(x), findSet(y), data)
  }

  private def link(x: Set, y: Set, data: Option[A]): Unit = {
    if (x == y) return
    if (x.rank > y.rank) {
      y.parent = x
      x.data = data
      // x becomes root
      x.max = x.max max (y.max)
      x.min = x.min min (y.min)
    } else {
      // y becomes root
      y.max = y.max max (x.max)
      y.min = y.min min (x.min)
      x.parent = y
      y.data = data
      if (x.rank == y.rank) {
        y.rank += 1
      }
    }
  }


  private def makeSet(x: Int) = {
    new Set(x)
  }

  private def findSet(x: Set): Set = {
    if (x != x.parent) {
      x.parent = findSet(x.parent)
    }
    x.parent
  }

}


