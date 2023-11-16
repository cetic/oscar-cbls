// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.algo.search

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

/** a generic algorithm for aggregating identical stuff
  * @author
  *   renaud.delandtsheer@cetic.be
  */
object IdenticalAggregator {

  def removeIdenticals[A](l: List[A], isIdentical: (A, A) => Boolean): List[A] =
    removeIdenticals[A](l, isIdentical, Nil)

  @tailrec
  private def removeIdenticals[A](
    l: List[A],
    isIdentical: (A, A) => Boolean,
    canonicals: List[A]
  ): List[A] = {
    l match {
      case Nil => canonicals
      case h :: t =>
        if (canonicals.exists(c => isIdentical(c, h)))
          removeIdenticals(t, isIdentical, canonicals)
        else removeIdenticals(t, isIdentical, h :: canonicals)
    }
  }

  /** @param l
    *   a list of items such that we want to discard items of identical class
    * @param itemClass
    *   a function that gives a class for a given item. Class Int.MinValue is considered as
    *   different from itself
    * @tparam A
    * @return
    *   a maximal subset of l such that all items are of different class according to itemClass
    *   (with Long.MinValue exception)
    */
  def removeIdenticalClasses[A](l: Iterable[A], itemClass: A => Int): List[A] = {
    val a: Set[Int] = SortedSet.empty
    removeIdenticalClasses[A](l.iterator, itemClass, Nil, a)
  }

  @tailrec
  private def removeIdenticalClasses[A](
    l: Iterator[A],
    itemClass: A => Int,
    canonicals: List[A],
    classes: Set[Int]
  ): List[A] = {
    if (l.hasNext) {
      val h             = l.next()
      val classOfH: Int = itemClass(h)
      if (classOfH != Int.MinValue && classes.contains(classOfH))
        removeIdenticalClasses(l, itemClass, canonicals, classes)
      else removeIdenticalClasses(l, itemClass, h :: canonicals, classes + classOfH)
    } else {
      canonicals
    }
  }

  /** class Int.MinValue is considered different from itself
    *
    * @param it
    * @param itemClass
    * @tparam A
    * @return
    */
  def removeIdenticalClassesLazily[A, C](it: Iterable[A], itemClass: A => C)(implicit
    A: Ordering[C]
  ): Iterable[A] = {
    new IdenticalSuppressedIterable(it, itemClass)
  }

  class IdenticalSuppressedIterable[A, C](it: Iterable[A], itemClass: A => C)(implicit
    A: Ordering[C]
  ) extends Iterable[A] {
    override def iterator: Iterator[A] =
      new IdenticalSuppressedIterator[A, C](it.iterator, itemClass)
  }

  class IdenticalSuppressedIterator[A, C](it: Iterator[A], itemClass: A => C)(implicit
    A: Ordering[C]
  ) extends Iterator[A] {
    var coveredClasses: Set[C] = SortedSet.empty

    private def advanceToNextOne: Option[A] = {
      while (it.hasNext) {
        val toReturn = it.next()
        val theClass = itemClass(toReturn)
        if (theClass == Long.MinValue || !coveredClasses.contains(theClass)) {
          coveredClasses += theClass
          return Some(toReturn)
        }
      }
      None
    }

    // this is the element to return next
    var theNextOne: Option[A] = advanceToNextOne

    override def hasNext: Boolean = theNextOne.isDefined

    override def next(): A =
      theNextOne match {
        case Some(s) => theNextOne = advanceToNextOne; s
        case _       => it.next() // to crash moreless transparently
      }
  }
}
