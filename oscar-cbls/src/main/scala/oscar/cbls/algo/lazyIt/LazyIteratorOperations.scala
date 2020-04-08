package oscar.cbls.algo.lazyIt

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


class NextIterator[T](base:Iterator[T]) extends Iterator[T]{
  var nextToReturn:T = null.asInstanceOf[T]
  var anyNextToReturn:Boolean = false

  override def hasNext(): Boolean = anyNextToReturn || base.hasNext

  override def next(): T = {
    if (anyNextToReturn){
      anyNextToReturn = false
      nextToReturn
    } else{
      base.next()
    }
  }

  def pushBack(t:T){
    require(!anyNextToReturn,"can only push back one element")
    anyNextToReturn = true
    nextToReturn = t
  }
}

class LazyMap[T](over:Iterable[Int],map:Int => T) extends Iterable[T]{
  override def iterator: Iterator[T] = new LazyMapIterator(over.iterator,map)
}

class LazyMapIterator[T](over:Iterator[Int],map:Int => T) extends Iterator[T] {
  override def hasNext: Boolean = over.hasNext
  override def next(): T = map(over.next())
}

class LazyFilter(over:Iterable[Int],filter:Int => Boolean) extends Iterable[Int]{
  override def iterator: Iterator[Int] = new LazyFilteredIterator(over.iterator,filter)
}

class LazyFilteredIterator(over:Iterator[Int],filter:Int => Boolean) extends Iterator[Int] {
  val overPB = new NextIterator[Int](over)
  var nextExistsAndIsFiltered = false

  override def hasNext: Boolean = {
    prepareNextTrueIfExists()
  }

  private[this] def prepareNextTrueIfExists(): Boolean = {
    if(nextExistsAndIsFiltered) return true
    while (overPB.hasNext()) {
      val potentialNext = overPB.next()
      if (filter(potentialNext)) {
        overPB.pushBack(potentialNext)
        return true
      }
    }
    false
  }

  override def next(): Int = {
    require(prepareNextTrueIfExists(),"next does nt exist")
    nextExistsAndIsFiltered = false
    overPB.next()
  }
}
