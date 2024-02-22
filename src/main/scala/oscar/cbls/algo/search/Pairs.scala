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

/** This object provides utility methods that generate lists of pairs of elements from a given list.
  */
object Pairs {

  /** This method returns a list containing all possible pairs of elements in the input list.
    *
    * @param l
    *   a list of elements
    * @tparam A
    *   the type of element
    * @return
    *   a list of all pairs of elements made from two elements in l
    */
  def makeAllUnsortedPairs[A](l: List[A]): List[(A, A)] = {
    @tailrec
    def makeAllUnsortedPairsWithHead(
      head: A,
      tail: List[A],
      toAppend: List[(A, A)]
    ): List[(A, A)] = {
      tail match {
        case other :: newTail =>
          makeAllUnsortedPairsWithHead(head, newTail, (head, other) :: toAppend)
        case Nil => toAppend
      }
    }

    l match {
      case Nil          => List.empty
      case head :: tail => makeAllUnsortedPairsWithHead(head, tail, makeAllUnsortedPairs(tail))
    }
  }

  /** This method returns a list containing all possible pairs of elements in the input list, with
    * the added condition that the order in which the elements appear in l is preserved.
    *
    * E.g.: {{{makeAllSortedPairs(List(1, 2, 3))}}} evaluates to: {{{List((1,2), (1,3), (2,3))}}}
    *
    * @param l
    *   a list of elements
    * @tparam A
    *   the type of element
    * @return
    *   a list of all pairs of elements made from two elements in l, preserving the order in which
    *   those elements are in l
    */
  @tailrec
  def makeAllSortedPairs[A](
    l: List[A],
    filter: (A, A) => Boolean = (_: A, _: A) => true,
    toReturn: List[(A, A)] = List.empty
  ): List[(A, A)] = {
    def makeAllSortedPairsWithHead(head: A, tail: List[A], toAppend: List[(A, A)]): List[(A, A)] = {
      tail match {
        case Nil => toAppend
        case other :: newTail =>
          if (filter(head, other))
            (head, other) :: makeAllSortedPairsWithHead(head, newTail, toAppend)
          else
            makeAllSortedPairsWithHead(head, newTail, toAppend)
      }
    }
    l match {
      case Nil => toReturn
      case h :: t =>
        makeAllSortedPairs(t, filter, toReturn ::: makeAllSortedPairsWithHead(h, t, List.empty))
    }
  }

  /** Given a list of elements l, this method returns a list of pairs (x, xs) where x is an element
    * in l and xs is the list of elements in l that follow x.
    *
    * E.g.: {{{makeAllHeadAndTails(List(1, 2, 3))}}} evaluates to:
    * {{{List((1,List(2, 3)), (2,List(3)),(3,List()))}}}.
    *
    * @param l
    *   a list of elements
    * @tparam A
    *   the type of element
    * @return
    *   list of pairs (x, xs) where x is in l and xs is the list of elements following x in l
    */
  def makeAllHeadAndTails[A](l: List[A]): List[(A, List[A])] = {
    l match {
      case Nil    => Nil
      case h :: t => (h, t) :: makeAllHeadAndTails(t)
    }
  }

  /** Given two lists of elements of possibly two different types and an optional filter, this
    * method returns a list of all possible pairs (a, b) where a is in l, b is in t, and (a, b)
    * satisfies the filtering condition if present.
    *
    * E.g.:
    * {{{
    *  zipIntoAllPossiblePairs(
    *   List(1, 2, 3), List("1", "2", "3"),
    *   (a: Int, b: String) => a.toString == b)
    * }}}
    * evaluates to {{{List((3,3), (2,2), (1,1))}}}.
    *
    * @param l
    *   a list of elements
    * @param t
    *   a list of elements
    * @tparam A
    *   the type of element in l
    * @tparam B
    *   the type of element in t
    * @param filter
    *   an optional filter
    * @return
    *   a list containing all the possible pairs (a, b) where a is in l, b is in t and (a, b)
    *   satisfies the filter
    */
  @tailrec
  def zipIntoAllPossiblePairs[A, B](
    l: List[A],
    t: List[B],
    filter: (A, B) => Boolean = (_: A, _: B) => true,
    toReturn: List[(A, B)] = List.empty
  ): List[(A, B)] = {
    l match {
      case Nil => toReturn
      case hl :: tl =>
        @tailrec
        def myAggregate(lh: A, rt: List[B], toReturn: List[(A, B)]): List[(A, B)] = {
          rt match {
            case ht :: tt if filter(lh, ht) => myAggregate(lh, tt, (lh, ht) :: toReturn)
            case ht :: tt                   => myAggregate(lh, tt, toReturn)
            case Nil                        => toReturn
          }
        }
        zipIntoAllPossiblePairs(tl, t, filter, myAggregate(hl, t, List.empty) ::: toReturn)
    }
  }

  /** Given a list l with at least two elements, this method returns a list of all consecutive pairs
    * of elements in l. If l has fewer than two element, the empty list is returned.
    *
    * E.g.: {{{nextPair(List(1, 2, 3))}}} evaluates to: {{{List((1,2), (2,3))}}}
    *
    * @param l
    *   a list of elements
    * @tparam A
    *   the type of element
    * @return
    *   a list of consecutive pairs
    */
  def nextPair[A](l: List[A]): List[(A, A)] = {
    l match {
      case Nil         => Nil
      case _ :: Nil    => Nil
      case a :: b :: t => (a, b) :: nextPair(b :: t)
    }
  }
}
