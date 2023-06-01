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

package oscar.cbls.algo.heap

class FibonacciHeap[T] {

  type Node = FibonacciHeap.Node[T]
  def isEmpty: Boolean  = min == null
  private[heap] var min = null: Node

  /** The minimum node in the heap. */
  def minNode: Node = min

  private[heap] var n = 0

  /** Number of nodes in the heap. */
  def size: Int = n

  /** Removes all elements from this heap. */
  def dropAll(): Unit = {
    min = null
    n = 0
  }

  /** Decreases the key value for a heap node, given the new value to take on. The structure of the
    * heap may be changed, but will not be consolidated.
    *
    * @param x
    *   node to decrease the key of
    * @param k
    *   new key value for node x
    */
  def decreaseKey(x: Node, k: Long): Unit = {

    require(!isEmpty, "Attempt to decrease key on empty heap")
    require(k < x.privateKey, s"New key is bigger than old key ($k >= ${x.privateKey})")
    x.privateKey = k
    val y = x.parent
    if (y != null && x.privateKey < y.privateKey) {
      y.cut(x, min)
      y.cascadingCut(min)
    }
    if (k < min.privateKey) {
      min = x
    }
  }

  /** Deletes a node from the heap given the reference to the node. The trees in the heap will be
    * consolidated, if necessary.
    *
    * @param x
    *   node to remove from heap.
    */
  def delete(x: Node): Unit = {
    decreaseKey(x, Long.MinValue)
    popMin()
  }

  /** Inserts a new data element into the heap. No heap consolidation is performed at this time, the
    * new node is simply inserted into the root list of this heap.
    *
    * @param x
    *   data object to insert into heap.
    * @return
    *   newly created heap node.
    */
  def insert(x: T, key: Long): Node = {
    val node = new Node(x, key)
    if (min != null) {
      node.right = min
      node.left = min.left
      min.left = node
      node.left.right = node
      if (key < min.privateKey) {
        min = node
      }
    } else {
      min = node
    }
    n += 1
    node
  }

  /** Removes the smallest element from the heap. This will cause the trees in the heap to be
    * consolidated, if necessary.
    *
    * @return
    *   data object with the smallest key.
    */
  def popMin(): Option[T] = {
    val z = min
    if (z == null) {
      None
    } else {
      if (z.child != null) {
        z.child.parent = null
        var x = z.child.right
        while (x != z.child) {
          x.parent = null
          x = x.right
        }
        val minleft    = min.left
        val zchildleft = z.child.left
        min.left = zchildleft
        zchildleft.right = min
        z.child.left = minleft
        minleft.right = z.child
      }
      z.left.right = z.right
      z.right.left = z.left
      if (z == z.right) {
        min = null
      } else {
        min = z.right
        consolidate()
      }
      n -= 1
      Some(z.value)
    }
  }

  private[this] def consolidate(): Unit = {
    val A = new Array[Node](n)

    var start = min
    var w     = min
    do {
      var x     = w
      var nextW = w.right
      var d     = x.degree
      while (A(d) != null) {
        var y = A(d)
        if (x.privateKey > y.privateKey) {
          val tmp = y
          y = x
          x = tmp
        }
        if (y == start) {
          start = start.right
        }
        if (y == nextW) {
          nextW = nextW.right
        }
        y.link(x)
        A(d) = null
        d += 1
      }
      A(d) = x
      w = nextW
    } while (w != start)

    min = start
    for (a <- A; if a != null) {
      if (a.privateKey < min.privateKey) min = a
    }
  }

  /** Returns the current min value
    * @return
    */
  def getMin: T = minNode.value

  /** Pops all the elements with their key equal to the current min
    * @return
    *   the list of mins
    */
  def popMins: List[T] = {
    var list = List[T]()
    if (min != null) {
      var savedMin = min.key
      while (!isEmpty && min.key == savedMin) {
        list = min.value :: list
        popMin()
      }
    }
    list
  }
}

object FibonacciHeap {

  /** Implements a node of the Fibonacci heap. */
  class Node[T](val value: T, originalKey: Long) {

    private[heap] var privateKey = originalKey
    private[heap] var parent     = null: Node[T]
    private[heap] var child      = null: Node[T]
    private[heap] var right      = this
    private[heap] var left       = this

    private[heap] var degree = 0
    private[heap] var mark   = false

    def key: Long = privateKey

    private[heap] def cascadingCut(min: Node[T]): Unit = {
      val z = parent
      if (z != null) {
        if (mark) {
          z.cut(this, min)
          z.cascadingCut(min)
        } else {
          mark = true
        }
      }
    }

    private[heap] def cut(x: Node[T], min: Node[T]): Unit = {
      x.left.right = x.right
      x.right.left = x.left
      degree -= 1
      if (degree == 0) {
        child = null
      } else if (child == x) {
        child = x.right
      }
      x.right = min
      x.left = min.left
      min.left = x
      x.left.right = x
      x.parent = null
      x.mark = false
    }

    private[heap] def link(prt: Node[T]): Unit = {
      left.right = right
      right.left = left
      parent = prt
      if (prt.child == null) {
        prt.child = this
        right = this
        left = this
      } else {
        left = prt.child
        right = prt.child.right
        prt.child.right = this
        right.left = this
      }
      prt.degree += 1
      mark = false
    }
  }

  /** Joins two Fibonacci heaps into a new one. No heap consolidation is performed at this time. The
    * two root lists are simply joined together.
    *
    * @param H1
    *   first heap
    * @param H2
    *   second heap
    * @return
    *   new heap containing H1 and H2
    */
  def union[T](H1: FibonacciHeap[T], H2: FibonacciHeap[T]): FibonacciHeap[T] = {
    val H = new FibonacciHeap[T]()
    H.min = H1.min
    if (H.min != null) {
      if (H2.min != null) {
        H.min.right.left = H2.min.left
        H2.min.left.right = H.min.right
        H.min.right = H2.min
        H2.min.left = H.min
        if (H2.min.privateKey < H1.min.privateKey) {
          H.min = H2.min
        }
      }
    } else {
      H.min = H2.min
    }
    H.n = H1.n + H2.n
    H
  }
}
