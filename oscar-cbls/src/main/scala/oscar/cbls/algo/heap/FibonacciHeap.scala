package oscar.cbls.algo.heap



/**
  *
  * Copyright (c) 2014, Ran Tian (tianran@nii.ac.jp)
  * All rights reserved.
  *
  * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
  * following conditions are met:
  *
  * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the
  *    following disclaimer.
  *
  * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the
  *    following disclaimer in the documentation and/or other materials provided with the distribution.
  *
  * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
  * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  */

class FibonacciHeap[T] {

  type Node = FibonacciHeap.Node[T]
  def isEmpty: Boolean = (min == null)
  private[heap] var min = null: Node

  /** The minimum node in the heap. */
  def minNode: Node = min

  private[heap] var n = 0

  /** Number of nodes in the heap. */
  def size: Int = n

  /**
    * Removes all elements from this heap.
    */
  def dropAll() {
    min = null
    n = 0
  }

  /**
    * Decreases the key value for a heap node, given the new value
    * to take on. The structure of the heap may be changed, but will
    * not be consolidated.
    *
    * @param  x  node to decrease the key of
    * @param  k  new key value for node x
    */
  def decreaseKey(x: Node, k: Long) {

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

  /**
    * Deletes a node from the heap given the reference to the node.
    * The trees in the heap will be consolidated, if necessary.
    *
    * @param  x  node to remove from heap.
    */
  def delete(x: Node){
    decreaseKey(x,Long.MinValue)
    popMin()
  }

  /**
    * Inserts a new data element into the heap. No heap consolidation
    * is performed at this time, the new node is simply inserted into
    * the root list of this heap.
    *
    * @param  x    data object to insert into heap.
    * @return newly created heap node.
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

  /**
    * Removes the smallest element from the heap. This will cause
    * the trees in the heap to be consolidated, if necessary.
    *
    * @return  data object with the smallest key.
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
        val minleft = min.left
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

  private[this] def consolidate() {
    val A = new Array[Node](n)

    var start = min
    var w = min
    do {
      var x = w
      var nextW = w.right
      var d = x.degree
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

  /**
    * Returns the current min value
    * @return
    */
  def getMin:T = minNode.value

  /**
    * Pops all the elements with their key equal to the current min
    * @return the list of mins
    */
  def popMins :List[T] = {
    var list = List[T]()
    if(min != null){
      var savedMin = min.key
      while(!isEmpty && min.key == savedMin){
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
    private[heap] var parent = null: Node[T]
    private[heap] var child = null: Node[T]
    private[heap] var right = this
    private[heap] var left = this

    private[heap] var degree = 0
    private[heap] var mark = false

    def key: Long = privateKey

    private[heap] def cascadingCut(min: Node[T]) {
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

    private[heap] def cut(x: Node[T], min: Node[T]) {
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

    private[heap] def link(prt: Node[T]) {
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

  /**
    * Joins two Fibonacci heaps into a new one. No heap consolidation is
    * performed at this time. The two root lists are simply joined together.
    *
    * @param  H1  first heap
    * @param  H2  second heap
    * @return  new heap containing H1 and H2
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

