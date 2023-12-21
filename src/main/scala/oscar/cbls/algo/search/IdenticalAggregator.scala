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

/** This object collects several generic algorithms for processing collections with generalized
  * duplicates, meaning they are either "identical" according to an associative boolean predicate,
  * or they belong to the same class (as defined by a provided function).
  *
  * All elements that either satisfy this predicate or belong to the same class of another element
  * (depending on the method invoked) are then removed, save for a single element. In other words,
  * the resulting collection includes only a single element per induced equivalence class.
  *
  * @author
  *   renaud.delandtsheer@cetic.be
  */
object IdenticalAggregator {

  /** This function removes the elements in a list that satisfy an associative predicate with
    * another element in the list, except for a single element. In the case where the predicate is
    * the identity, duplicate entries are removed. Ordering of elements is not preserved.
    *
    * Example:
    * {{{
    * scala> removeIdenticals[Int](List(1, 2, 3, 1), (a, b) => a == b)
    * val res1: List[Int] = List(3, 2, 1)
    * }}}
    *
    * @param l
    *   the input list
    * @param isIdentical
    *   the associative predicate
    * @tparam A
    *   the type of the element in the list
    * @return
    *   a list with elements filtered out
    */
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

  /** This function removes the elements in a collection belonging to the same class, except for a
    * single element. The class of an element is an integer defined by a function parameter.
    * Elements associated to Int.MinValue are not discarded.
    *
    * @param it
    *   an iterable collection of items such that we want to discard items of identical class
    * @param itemClass
    *   a function returning the class of a given element
    * @tparam A
    *   the element type
    * @return
    *   a list containing a maximal subset of the original collection such that all items are of
    *   different classes according to itemClass, excluding items associated to Int.MinValue
    */
  def removeIdenticalClasses[A](it: Iterable[A], itemClass: A => Int): List[A] = {
    val a: Set[Int] = SortedSet.empty
    removeIdenticalClasses[A](it.iterator, itemClass, Nil, a)
  }

  @tailrec
  private def removeIdenticalClasses[A](
    it: Iterator[A],
    itemClass: A => Int,
    canonicals: List[A],
    classes: Set[Int]
  ): List[A] = {
    if (it.hasNext) {
      val next             = it.next()
      val classOfNext: Int = itemClass(next)
      if (classOfNext != Int.MinValue && classes.contains(classOfNext))
        removeIdenticalClasses(it, itemClass, canonicals, classes)
      else removeIdenticalClasses(it, itemClass, next :: canonicals, classes + classOfNext)
    } else {
      canonicals
    }
  }

  /** This function lazily removes the elements in a collection belonging to the same class, except
    * for a single element. The class of an element is a value defined by a function parameter. If
    * the type of the class is integer, elements associated to Int.MinValue are not discarded.
    *
    * @param it
    *   an iterable collection of items such that we want to discard items of identical class
    * @param itemClass
    *   a function returning the class of a given element
    * @tparam A
    *   the type of the element
    * @tparam C
    *   the type of the element classes
    * @return
    *   a maximal subset of the original collection such that all items are of different classes
    *   according to itemClass, excluding items associated to Int.MinValue if the class type is
    *   integer
    */
  def removeIdenticalClassesLazily[A, C](it: Iterable[A], itemClass: A => C)(implicit
    A: Ordering[C]
  ): Iterable[A] = {
    new collection.AbstractIterable[A] {
      def iterator: Iterator[A] = new collection.AbstractIterator[A] {

        private[this] var iter = it.iterator

        private[this] var coveredClasses: Set[C] = SortedSet.empty

        private[this] def advanceToNextOne: Option[A] = {
          while (iter.hasNext) {
            val toReturn = iter.next()
            val theClass = itemClass(toReturn)

            val isExempt = theClass match {
              case Int.MinValue => true
              case _            => false
            }

            if (isExempt || !coveredClasses.contains(theClass)) {
              coveredClasses += theClass
              return Some(toReturn)
            }
          }
          None
        }

        // this is the element to return next
        private[this] var theNextOne: Option[A] = advanceToNextOne

        override def hasNext: Boolean = theNextOne.isDefined

        override def next(): A =
          theNextOne match {
            case Some(s) => theNextOne = advanceToNextOne; s
            case _       => iter.next() // to crash more or less transparently
          }
      }
    }
  }
}
