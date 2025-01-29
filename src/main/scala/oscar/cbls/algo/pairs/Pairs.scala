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

package oscar.cbls.algo.pairs

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}

import scala.annotation.tailrec

/** Collection of utility functions that make pairs from list or sequences. */
object Pairs {

  /** Given a list `[x,,0,,, x,,1,,, ..., x,,n,,]`, returns the list containing the pairs `(x,,i,,,`
    * `x,,i+1,,)` for all `0 <= i < n + 1`.
    * @param l
    *   The list on which pairs of adjacent elements are made.
    */
  def pairsOfAdjacent[T](l: List[T]): List[(T, T)] = {
    l match {
      case Nil            => Nil
      case _ :: Nil       => Nil
      case a :: b :: tail => (a, b) :: pairsOfAdjacent(b :: tail)
    }
  }

  /** Given a sequence `x,,0,, -> x,,1,, -> ... -> x,,n,,`, returns the list containing the pairs
    * `(x,,i,,, x,,i+1,,)` for all `0 <= i < n + 1`.
    * @param seq
    *   The sequence of integer on which pairs of adjacent nodes are made.
    */
  def pairsOfAdjacent(seq: IntSequence): List[(Int, Int)] = pairsOfAdjacent(seq.toList)

  /** Given a sequence used in a VRP, returns the list of adjacent nodes following the route of each
    * vehicle including the return to the depot. <br>
    *
    * For example, with two vehicles and the route `0 -> 2 -> 4 -> 1 -> 3 -> 5`, the method returns:
    * `[(0, 2), (2, 4), (4, 0), (1, 3), (3, 5), (5, 1)]`.
    *
    * @param route
    *   The sequence associated to the route of a VRP.
    * @param v
    *   The number of vehicles in the associated VRP.
    */
  def pairsOfAdjacentInRoute(route: IntSequence, v: Int): List[(Int, Int)] = {

    def makePairs(exp: IntSequenceExplorer): List[(Int, Int)] = {
      val next = exp.next
      if (next.position == route.size) List((exp.value, v - 1)) // End of the sequence
      else if (next.value < v)
        (exp.value, next.value - 1) :: makePairs(next) // End of the vehicle route
      else (exp.value, next.value) :: makePairs(next)
    }

    val expOfZero = route.explorerAtPosition(0).get // Always exist
    require(
      expOfZero.value == 0,
      s""" Given sequence does not respect routing convention constraints.
         |Node at position 0 must be 0.
         |Got ${expOfZero.value}.
         |""".stripMargin
    )

    makePairs(expOfZero)

  }

  /** From the elements of the input list, returns a list of all the pairs of these elements that
    * respect the given filter. By default, all the pairs are accepted.
    *
    * @note
    *   The returned list does not contain symmetric pairs.
    *
    * @param l
    *   The list on which pairs are made.
    * @param filter
    *   The function selecting which pairs of elements to keep.
    */
  def allPairs[T](l: List[T], filter: (T, T) => Boolean = (_: T, _: T) => true): List[(T, T)] = {
    @tailrec
    def makeAllPairsWithHead(head: T, tail: List[T], toAppend: List[(T, T)]): List[(T, T)] = {
      tail match {
        case a :: t if filter(head, a) => makeAllPairsWithHead(head, t, (head, a) :: toAppend)
        case _ :: t                    => makeAllPairsWithHead(head, t, toAppend)
        case Nil                       => toAppend
      }
    }

    l match {
      case Nil          => List.empty
      case head :: tail => makeAllPairsWithHead(head, tail, allPairs(tail, filter))
    }

  }

  /** Returns all the pairs `(head, tail)` from the input list.
    * @param l
    *   The list on which all the pairs `(head, tail)` are created.
    */
  def headAndTail[T](l: List[T]): List[(T, List[T])] = {
    l match {
      case Nil          => Nil
      case head :: tail => (head, tail) :: headAndTail(tail)
    }
  }

  /** Returns a list of all the pairs composed of one element of the first list and another element
    * of the second list. The formed pairs have to respect the input filter. By default, all the
    * pairs are accepted.
    *
    * @note
    *   The returned list does not contain symmetric pairs.
    *
    * @param left
    *   The first list to zip.
    * @param right
    *   The second list to zip.
    * @param filter
    *   The function selecting which pairs of elements from `left` and `right` to keep.
    */
  def zipAllPairs[L, R](
    left: List[L],
    right: List[R],
    filter: (L, R) => Boolean = (_: L, _: R) => true
  ): List[(L, R)] = {
    @tailrec
    def aggregate(leftHead: L, right: List[R], toReturn: List[(L, R)]): List[(L, R)] = {
      right match {
        case rightHead :: rightTail if filter(leftHead, rightHead) =>
          aggregate(leftHead, rightTail, (leftHead, rightHead) :: toReturn)
        case _ :: rightTail => aggregate(leftHead, rightTail, toReturn)
        case Nil            => toReturn
      }
    }

    left match {
      case Nil                  => List.empty
      case leftHead :: leftTail => aggregate(leftHead, right, zipAllPairs(leftTail, right, filter))
    }
  }

}
