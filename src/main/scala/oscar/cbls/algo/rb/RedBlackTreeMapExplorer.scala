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

package oscar.cbls.algo.rb

import scala.annotation.tailrec

// In the (tree node, boolean) pair, the boolean flag signifies that the node
// has already been shown, when traversing from left to right

/** This immutable class provides an efficient way to navigate through the elements of a
  * [[RedBlackTreeMap]] by exploiting the underlying tree structure.
  *
  * @param position
  *   a list of flagged tree nodes, representing the path from the root of the underlying tree to
  *   the node associated to the current key-value pair
  * @tparam A
  *   the type of the values in the associated map. Keys are of type [[Int]]
  * @note
  *   This class cannot be instantiated directly, instances are provided by methods in
  *   [[RedBlackTreeMap]]
  */
class RedBlackTreeMapExplorer[@specialized(Int) A] private[rb] (position: List[(T[A], Boolean)]) {

  /** The current key. */
  def key: Int = position.head._1.pk

  /** The current value. */
  def value: A = position.head._1.pv.get

  override def toString: String =
    "RBPosition(key:" + key + " value:" + value + " stack:" + position + ")"

  /** Optionally returns the next key-value pair in the map, according to the ordering on keys
    * (i.e., with the next larger integer), encapsulated in a new explorer.
    */
  def next: Option[RedBlackTreeMapExplorer[A]] = {

    // helper tail-recursive methods
    @tailrec
    def unstack1(position: List[(T[A], Boolean)]): List[(T[A], Boolean)] = {
      if (position.isEmpty) return position
      val head = position.head
      if (!head._2) {
        // not presented yet, so we present this one
        (head._1, true) :: position.tail
      } else {
        // already presented, so unstack
        unstack1(position.tail)
      }
    }

    @tailrec
    def descendToLeftMost(position: List[(T[A], Boolean)]): List[(T[A], Boolean)] = {
      val headTree = position.head._1
      headTree.pl match {
        case t: T[A] => descendToLeftMost((t, false) :: position)
        case _       => (headTree, true) :: position.tail
      }
    }

    val newStack = position.head._1.pr match {
      case t: T[A] => descendToLeftMost((t, false) :: position)
      case _       => unstack1(position)
    }

    if (newStack == Nil) None
    else Some(new RedBlackTreeMapExplorer[A](newStack))
  }

  /** Optionally returns the previous key-value pair in the map, according to the ordering on keys
    * (i.e., with the previous smaller integer), encapsulated in a new explorer.
    */
  def prev: Option[RedBlackTreeMapExplorer[A]] = {

    // helper tail-recursive methods
    @tailrec
    def unstack1(position: List[(T[A], Boolean)]): List[(T[A], Boolean)] = {
      if (position.isEmpty) return position
      val head = position.head
      if (head._2) {
        // already presented, so roll back to it.
        (head._1, true) :: position.tail
      } else {
        // already presented, so unstack
        unstack1(position.tail)
      }
    }

    @tailrec
    def descendToRightMost(position: List[(T[A], Boolean)]): List[(T[A], Boolean)] = {
      val headTree = position.head._1
      headTree.pr match {
        case t: T[A] => descendToRightMost((t, true) :: position)
        case _       => (headTree, true) :: position.tail
      }
    }

    val newStack = position.head._1.pl match {
      case t: T[A] =>
        descendToRightMost((t, true) :: ((position.head._1, false) :: position.tail))
      case _ => unstack1(position.tail)
    }

    if (newStack.isEmpty) None
    else {
      assert(
        new RedBlackTreeMapExplorer[A](newStack).next.head.key == this.key,
        "prev.next.key != this.key; this:" + this + " prev:" + new RedBlackTreeMapExplorer[A](
          newStack
        )
      )
      Some(new RedBlackTreeMapExplorer[A](newStack))
    }
  }
}
