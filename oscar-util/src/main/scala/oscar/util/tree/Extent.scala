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
package oscar.util.tree

class Extent(var extentList: List[(Double, Double)] = List[(Double, Double)]()) {
  
  def this(x1: Double, x2: Double) = this(List((x1, x2)))
  
  def moveExtent(x: Double): Extent = {
    new Extent(extentList.map(elem => (elem._1 + x, elem._2 + x)))
  }
  
  def add(x1: Double, x2: Double) = {
    extentList :+= (x1, x2)
  }
  
  def append(other: Extent) = {
    extentList = extentList ::: other.extentList
  }
  
  def head1 = extentList.head._1
  def head2 = extentList.head._2
  def tail = {
    new Extent(extentList.tail)
  }
  
  def isEmpty: Boolean = extentList.isEmpty 
}

object Extent {
  var minDist = 1.0
  def apply(extentList: List[(Double, Double)] = List[(Double, Double)]()) = new Extent(extentList)
  
  def apply(x1: Double, x2: Double) = new Extent(x1, x2)
  
  def merge(first: Extent, second: Extent): Extent = {
    if (first.isEmpty) second
    else if (second.isEmpty) first
    else {
      val mergedExtent = new Extent(first.head1, second.head2)
      mergedExtent.append(merge(first.tail, second.tail))
      mergedExtent
    }
  }
  
  def mergeList(extentList: List[Extent]): Extent = {
    extentList.foldLeft(new Extent())((e1, e2) => merge(e1, e2))
  }
  
  def rMax(p: Double, q: Double): Double = Math.max(p, q)
  
  def fit(first: Extent, second: Extent): Double = {
    if (first.isEmpty || second.isEmpty) 0.0
    else rMax(fit(first.tail, second.tail), first.head2 - second.head1 + minDist)
  }
  
  def fitListLeft(extentList: List[Extent]): List[Double] = {
    def fitListLeftAux(acc: Extent, subList: List[Extent]): List[Double] = {
      if(subList.isEmpty) List[Double]()
      else {
        val x = fit(acc, subList.head)
        x :: fitListLeftAux(merge(acc, subList.head.moveExtent(x)), subList.tail)
      }
    }
    fitListLeftAux(Extent(), extentList)
  }
  
  def fitListRight(extentList: List[Extent]): List[Double] = {
    def fitListRightAux(acc: Extent, subList: List[Extent]): List[Double] = {
      if(subList.isEmpty) List[Double]()
      else {
        val x = - fit(subList.head, acc)
        x :: fitListRightAux(merge(subList.head.moveExtent(x), acc), subList.tail)
      }
    }
    fitListRightAux(Extent(), extentList.reverse).reverse
  }
  
  def mean(x: Double, y: Double): Double = (x + y) / 2.0
  
  def fitList(extentList: List[Extent]): List[Double] = {
    fitListLeft(extentList).zip(fitListRight(extentList)).map(e => mean(e._1, e._2))
  }
}
