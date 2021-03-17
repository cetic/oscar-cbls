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
package oscar.cbls.algo.accList

object AccList{
  def base[T](l:T*):AccList[T] = new LeafAccList[T](l.toList)
  def acc[T](l:AccList[T]*):AccList[T] = new AccNode[T](l.toList)
  def empty[T]():AccList[T] = base[T]()
  def fromList[T](l:List[T]) = new LeafAccList[T](l)
}

abstract class AccList[T]{
  def toList:List[T] = toListAcc(List.empty)
  def toListAcc(tail:List[T]):List[T]
}

class LeafAccList[T](content:List[T]) extends AccList[T]{
  override def toListAcc(tail: List[T]): List[T] = {
    content ::: tail
  }
}

class AccNode[T](content:List[AccList[T]]) extends AccList[T]{
  override def toListAcc(tail: List[T]): List[T] = {
    def myToListAcc(myList:List[AccList[T]]):List[T] = {
      myList match{
        case Nil => tail
        case myHead::myTail => myHead.toListAcc(myToListAcc(myTail))
      }
    }
    myToListAcc(content)
  }
}
