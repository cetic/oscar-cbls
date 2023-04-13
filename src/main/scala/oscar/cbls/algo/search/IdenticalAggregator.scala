package oscar.cbls.algo.search

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

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

/**
 * a generic algorithm for aggregating identical stuff
 * @author renaud.delandtsheer@cetic.be
 * */
object IdenticalAggregator{

  def removeIdenticals[T](l:List[T], isIdentical:(T,T) => Boolean):List[T] =
    removeIdenticals[T](l, isIdentical, Nil)

  @tailrec
  private def removeIdenticals[T](l:List[T],
                                  isIdentical:(T,T) => Boolean,
                                  canonicals:List[T]):List[T] = {
    l match{
      case Nil => canonicals
      case h :: t =>
        if(canonicals.exists(c => isIdentical(c,h)))
          removeIdenticals(t, isIdentical, canonicals)
        else removeIdenticals(t, isIdentical, h::canonicals)
    }
  }

  /**
   * @param l a list of items such that we want to discard items of identical class
   * @param itemClass a function that gives a class for a given item.
   *                  Class Int.MinValue is considered as different from itself
   * @tparam T
   * @return a maximal subset of l such that
   *         all items are of different class according to itemClass (with Long.MinValue exception)
   */
  def removeIdenticalClasses[T](l:Iterable[T], itemClass:T => Int):List[T] = {
    val a: Set[Int] = SortedSet.empty
    removeIdenticalClasses[T](l.iterator, itemClass, Nil, a)
  }

  @tailrec
  private def removeIdenticalClasses[T](l:Iterator[T],
                                        itemClass:T => Int,
                                        canonicals:List[T],
                                        classes:Set[Int]):List[T] = {
    if (l.hasNext) {
      val h = l.next()
      val classOfH:Int = itemClass(h)
      if(classOfH != Int.MinValue && classes.contains(classOfH))
        removeIdenticalClasses(l, itemClass, canonicals,classes)
      else removeIdenticalClasses(l, itemClass, h::canonicals, classes+classOfH)
    }else {
      canonicals
    }
  }

  /** class Int.MinValue is considered different from itself
    *
    * @param it
    * @param itemClass
    * @tparam T
    * @return
    */
  def removeIdenticalClassesLazily[T,C](it:Iterable[T], itemClass:T => C)(implicit A:Ordering[C]):Iterable[T] = {
    new IdenticalSuppressedIterable(it,itemClass)
  }

  class IdenticalSuppressedIterable[T,C](it:Iterable[T], itemClass:T => C)(implicit A:Ordering[C]) extends Iterable[T]{
    override def iterator: Iterator[T] = new IdenticalSuppressedIterator[T,C](it.iterator, itemClass)
  }

  class IdenticalSuppressedIterator[T,C](it:Iterator[T], itemClass:T => C)(implicit A:Ordering[C]) extends Iterator[T]{
    var coveredClasses:Set[C] = SortedSet.empty

    private def advanceToNextOne:Option[T] = {
      while(it.hasNext) {
        val toReturn = it.next()
        val theClass = itemClass(toReturn)
        if (theClass ==  Long.MinValue || !coveredClasses.contains(theClass)){
          coveredClasses += theClass
          return Some(toReturn)
        }
      }
      None
    }

    //this is the element to return next
    var theNextOne:Option[T] = advanceToNextOne

    override def hasNext: Boolean = theNextOne.isDefined

    override def next(): T =
      theNextOne match{
        case Some(s) => theNextOne = advanceToNextOne; s
        case _ => it.next() //to crash moreless transparently
      }
  }
}
