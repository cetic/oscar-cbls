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

// Object providing several helper methods.
// Code partly inspired from https://tinyurl.com/2s3vczpd
private object RedBlackTreeMapLib {
  val R = true
  val B = false

  // blacken: Turn a node black.
  def blacken[V](n: RedBlackTreeMap[V]): RedBlackTreeMap[V] = {
    n match {
      case L()              => n
      case T(_, l, k, v, r) => T(B, l, k, v, r)
    }
  }

  // balance: Balance a tree with balanced subtrees.
  def balance[V](
    c: Boolean,
    l: RedBlackTreeMap[V],
    k: Int,
    v: Option[V],
    r: RedBlackTreeMap[V]
  ): RedBlackTreeMap[V] = {
    (c, l, k, v, r) match {
      case (B, T(R, T(R, a, xK, xV, b), yK, yV, c), zK, zV, d) =>
        T(R, T(B, a, xK, xV, b), yK, yV, T(B, c, zK, zV, d))
      case (B, T(R, a, xK, xV, T(R, b, yK, yV, c)), zK, zV, d) =>
        T(R, T(B, a, xK, xV, b), yK, yV, T(B, c, zK, zV, d))
      case (B, a, xK, xV, T(R, T(R, b, yK, yV, c), zK, zV, d)) =>
        T(R, T(B, a, xK, xV, b), yK, yV, T(B, c, zK, zV, d))
      case (B, a, xK, xV, T(R, b, yK, yV, T(R, c, zK, zV, d))) =>
        T(R, T(B, a, xK, xV, b), yK, yV, T(B, c, zK, zV, d))
      case (c, a, xK, xV, b) => T(c, a, xK, xV, b)
    }
  }
}

// helper class for a key-value pair.
private class IntVCouple[@specialized V](val k: Int, val value: V)

import oscar.cbls.algo.rb.RedBlackTreeMapLib._

/** Companion object providing factory methods.
  *
  * Keys in this map are always of type [[Int]], only values are parametrized.
  */
object RedBlackTreeMap {

  /** Returns an empty map.
    *
    * @tparam V
    *   type of the values
    */
  def empty[@specialized(Int) V]: RedBlackTreeMap[V] = L[V]()

  /** Constructs a map from a given collection of key-value pairs.
    *
    * @tparam V
    *   type of the values
    * @param args
    *   the collection of key-value pairs
    */
  def apply[@specialized(Int) V](args: Iterable[(Int, V)]): RedBlackTreeMap[V] = {
    var currentMap: RedBlackTreeMap[V] = L()
    for ((k, v) <- args) {
      currentMap = currentMap.insert(k, v)
    }
    currentMap
  }

  /** Create a red-black tree out of already sorted key-value pairs, which must be sorted by
    * increasing order of key, and where a key can only be present once.
    *
    * WARNING: No check of these properties on the inputs are performed, either at compile or at
    * runtime. Use caution when invoking.
    *
    * @note
    *   Performance is O(n); thus faster than a n*log(n) when building from unsorted pairs
    * @param args
    *   the key-value collection, assumed to be sorted
    * @tparam V
    *   Type of the values
    */
  def makeFromSorted[@specialized(Int) V](args: Iterable[(Int, V)]): RedBlackTreeMap[V] = {
    // root is to be black, beside alternate red and black
    val a = args.toArray
    if (args.size <= 3) this.apply(args)
    else myMakeFromSorted(a, 0, a.length - 1, targetIsRed = false)
  }

  /** Create a red-black tree out of already sorted key-value pairs, which must be sorted by
    * increasing order of key, and where a key can only be present once. Collection must be a
    * continuous array.
    *
    * WARNING: No check of these properties on the inputs are performed, either at compile or at
    * runtime. Use caution when invoking.
    *
    * @note
    *   Performance is O(n); thus faster than a n*log(n) when building from unsorted pairs
    * @param args
    *   the key-value collection, assumed to be sorted
    * @tparam V
    *   Type of the values
    */
  def makeFromSortedContinuousArray[@specialized V](args: Array[V]): RedBlackTreeMap[V] = {
    if (args.length == 0) RedBlackTreeMap.empty[V]
    else myMakeFromContinuousSorted(args, 0, args.length - 1, targetIsRed = false)
  }

  // helper recursive method
  private def myMakeFromContinuousSorted[@specialized(Int) V](
    args: Array[V],
    fromIncluded: Int,
    toIncluded: Int,
    targetIsRed: Boolean
  ): RedBlackTreeMap[V] = {
    // root is to be black, afterwards alternate red and black
    if (fromIncluded == toIncluded) {
      val value = args(fromIncluded)
      T(targetIsRed, L(), fromIncluded, Some(value), L())
    } else if (fromIncluded + 1 == toIncluded) {
      val valueL = args(fromIncluded)
      val valueH = args(toIncluded)
      T(
        targetIsRed,
        T(!targetIsRed, L(), fromIncluded, Some(valueL), L()),
        toIncluded,
        Some(valueH),
        L()
      )
    } else {
      // there is a middle point
      val middlePoint = (fromIncluded + toIncluded) / 2
      val left  = myMakeFromContinuousSorted(args, fromIncluded, middlePoint - 1, !targetIsRed)
      val right = myMakeFromContinuousSorted(args, middlePoint + 1, toIncluded, !targetIsRed)
      val value = args(middlePoint)
      T(targetIsRed, left, middlePoint, Some(value), right)
    }
  }

  /** Create a red-black tree out of already sorted key-value pairs, which must be sorted by
    * increasing order of key, and where a key can only be present once. Collection must be an
    * array.
    *
    * WARNING: No check of these properties on the inputs are performed, either at compile or at
    * runtime. Use caution when invoking.
    *
    * @note
    *   Performance is O(n); thus faster than a n*log(n) when building from unsorted pairs
    * @param args
    *   the key-value collection, assumed to be sorted
    * @tparam V
    *   Type of the values
    */
  def makeFromSortedArray[@specialized(Int) V](args: Array[(Int, V)]): RedBlackTreeMap[V] = {
    // root is to be black, afterwards alternate red and black
    if (args.length <= 1) this.apply(args)
    else myMakeFromSorted(args, 0, args.length - 1, targetIsRed = false)
  }

  // helper recursive method
  private def myMakeFromSorted[@specialized(Int) V](
    args: Array[(Int, V)],
    fromIncluded: Int,
    toIncluded: Int,
    targetIsRed: Boolean
  ): RedBlackTreeMap[V] = {
    // root is to be black, beside alternate red and black
    if (fromIncluded == toIncluded) {
      val (key, value) = args(fromIncluded)
      T(targetIsRed, L(), key, Some(value), L())
    } else if (fromIncluded + 1 == toIncluded) {
      val (keyL, valueL) = args(fromIncluded)
      val (keyH, valueH) = args(toIncluded)

      assert(keyH > keyL, "Unsorted array")

      T(targetIsRed, T(!targetIsRed, L(), keyL, Some(valueL), L()), keyH, Some(valueH), L())
    } else {
      // there is a middle point
      val middlePoint  = (fromIncluded + toIncluded) / 2
      val left         = myMakeFromSorted(args, fromIncluded, middlePoint - 1, !targetIsRed)
      val right        = myMakeFromSorted(args, middlePoint + 1, toIncluded, !targetIsRed)
      val (key, value) = args(middlePoint)

      assert(left.asInstanceOf[T[V]].pk < key, "Unsorted array")
      assert(right.asInstanceOf[T[V]].pk > key, "Unsorted array")

      T(targetIsRed, left, key, Some(value), right)
    }
  }
}

// Must use trait here because of specialization, a trait is needed here.
// We ensure that this trait is compiled into a java interface by avoiding method code in the trait.
// As a consequence, there are duplicates in the classes implementing this trait.

/** This trait provides an implementation of a red-black tree-backed immutable map, where the keys
  * are exclusively of type [[scala.Int]]. This trait is not meant to replace
  * [[collection.immutable.SortedMap]], but rather to provide an implementation that allows
  * efficient exploration of the underlying tree by a [[RedBlackTreeMapExplorer]].
  *
  * @tparam V
  *   the type of the values (keys are [[Int]])
  */
trait RedBlackTreeMap[@specialized(Int) V] {

  // modWith: Helper method; top node could be red.
  protected[rb] def modWith(k: Int, f: (Int, Option[V]) => Option[V]): RedBlackTreeMap[V]

  /** Optionally retrieve the value for a given key.
    * @param k
    *   the key
    */
  def get(k: Int): Option[V]

  /** Retrieves the value for a given key if it exists, otherwise, return a default value.
    *
    * @param k
    *   the key
    * @param default
    *   the default value
    */
  def getOrElse(k: Int, default: => V): V

  /** Returns true if the key is contained in the map, and false otherwise.
    *
    * @param k
    *   the key
    */
  def contains(k: Int): Boolean

  /** Optionally provides the largest key-value pair whose key is smaller or equal than the given
    * reference key.
    *
    * @param k
    *   the reference key
    */
  def biggestLowerOrEqual(k: Int): Option[(Int, V)]

  // helper method
  protected[rb] def getBiggestLowerAcc(k: Int, bestKSoFar: Int, bestVSoFar: V): IntVCouple[V]

  /** Optionally provides the smallest key-value pair whose key is larger or equal than the given
    * reference key.
    *
    * @param k
    *   the reference key
    */
  def smallestBiggerOrEqual(k: Int): Option[(Int, V)]

  // helper method
  protected[rb] def getSmallestBiggerAcc(k: Int, bestKSoFar: Int, bestVSoFar: V): IntVCouple[V]

  /** Optionally return the smallest key-value pair. */
  def smallest: Option[(Int, V)]

  /** Optionally return the largest key-value pair. */
  def biggest: Option[(Int, V)]

  /** Optionally returns a [[RedBlackTreeMapExplorer]] anchored at the largest key-value pair. */
  def biggestPosition: Option[RedBlackTreeMapExplorer[V]]

  /** Optionally returns a [[RedBlackTreeMapExplorer]] anchored at the smallest key-value pair. */
  def smallestPosition: Option[RedBlackTreeMapExplorer[V]]

  /** Returns a new map with the addition of the given key and value. If the key is already present,
    * its value is updated.
    * @param k
    *   key
    * @param v
    *   value
    */
  def insert(k: Int, v: V): RedBlackTreeMap[V]

  /** Returns a new map deprived of the given key. If the key is absent, returns the same map.
    *
    * @param k
    *   the key to remove
    */
  def remove(k: Int): RedBlackTreeMap[V]

  /** The size of this map. */
  def size: Int

  /** Whether or not this map is empty. */
  def isEmpty: Boolean

  /** Returns a list of the values in this map. */
  def values: List[V]

  /** Returns a list of the key-value pairs in this map. */
  def content: List[(Int, V)]

  /** Returns a list of the keys in this map. */
  def keys: List[Int]

  /** Optionally returns a [[RedBlackTreeMapExplorer]] anchored at the given key.
    *
    * @param k
    *   the key
    */
  def positionOf(k: Int): Option[RedBlackTreeMapExplorer[V]]

  // helper recursive methods
  protected[rb] def keysAcc(keysAfter: List[Int]): List[Int]
  protected[rb] def valuesAcc(valuesAfter: List[V]): List[V]
  protected[rb] def contentAcc(valuesAfter: List[(Int, V)]): List[(Int, V)]

  protected[rb] def positionOfAcc(
    k: Int,
    positionAcc: List[(T[V], Boolean)]
  ): Option[RedBlackTreeMapExplorer[V]]

  /** Optionally returns an undefined value contained in this map. */
  def anyValue: Option[V]

  /** Updates a set of values in the tree, defined by an inclusive interval on the keys.
    *
    * Besides providing a method to transform a value in the interval to another value, the caller
    * can provide an integer delta by which the keys in the range will be shifted.
    *
    * WARNING: It is required that the deltaKey must not transform a key in the interval in such a
    * way that it becomes larger or smaller than another key outside of the interval, if this was
    * not the case before the update. This is required in order to keep the identical structure of
    * the tree, while maintain the same colouring and balance of the tree, which ensures that good
    * performance.
    *
    * @param fromKeyIncluded
    *   the start of the interval defining the set of keys to update
    * @param toKeyIncluded
    *   the end of the interval defining the set of keys to update
    * @param deltaKey
    *   the delta to apply to the keys in the interval
    * @param transform
    *   the transform to apply on the values stored in the interval
    * @return
    *   a new map with updated entries
    */
  def updateDelta(
    fromKeyIncluded: Int,
    toKeyIncluded: Int,
    deltaKey: Int,
    transform: V => V
  ): RedBlackTreeMap[V]

  /** Updates a set of values in the tree, defined by an inclusive interval on the keys.
    *
    * Values in the interval are transformed to another through a given method.
    *
    * @param fromKeyIncluded
    *   the start of the interval defining the set of keys to update
    * @param toKeyIncluded
    *   the end of the interval defining the set of keys to update
    * @param transform
    *   the transform to apply on the values stored in the interval
    * @return
    *   a new map with updated values
    */
  def update(
    fromKeyIncluded: Int,
    toKeyIncluded: Int,
    transform: (Int, V) => (Int, V)
  ): RedBlackTreeMap[V]

  def updateAll(deltaKey: Int, transform: V => V): RedBlackTreeMap[V] = {
    (this.smallest, this.biggest) match {
      case (None, None) => this
      case (Some((smallestKey, _)), Some((biggestKey, _))) =>
        updateDelta(smallestKey, biggestKey, deltaKey, transform)
      case _ => throw new Error("unexpected error")
    }
  }
}

// A leaf node.
private[rb] case class L[@specialized(Int) V]() extends RedBlackTreeMap[V] {

  def anyValue: Option[V] = None

  def get(k: Int): Option[V] = None

  def getOrElse(k: Int, default: => V): V = get(k) match {
    case None    => default
    case Some(x) => x
  }

  override def contains(k: Int): Boolean = false

  override protected[rb] def modWith(
    k: Int,
    f: (Int, Option[V]) => Option[V]
  ): RedBlackTreeMap[V] = {
    f(k, None) match {
      case None      => this
      case something => T(R, this, k, something, this)
    }
  }

  def biggestLowerOrEqual(k: Int): Option[(Int, V)] = None

  override protected[rb] def getBiggestLowerAcc(
    k: Int,
    bestKSoFar: Int,
    bestVSoFar: V
  ): IntVCouple[V] = new IntVCouple[V](bestKSoFar, bestVSoFar)

  override def smallestBiggerOrEqual(k: Int): Option[(Int, V)] = None

  override protected[rb] def getSmallestBiggerAcc(k: Int, bestKSoFar: Int, bestVSoFar: V) =
    new IntVCouple(bestKSoFar, bestVSoFar)

  override def size: Int = 0
  override def isEmpty   = true

  protected[rb] def valuesAcc(valuesAfter: List[V]): List[V]                = valuesAfter
  protected[rb] def contentAcc(valuesAfter: List[(Int, V)]): List[(Int, V)] = valuesAfter
  protected[rb] def keysAcc(keysAfter: List[Int]): List[Int]                = keysAfter

  protected[rb] override def positionOfAcc(
    k: Int,
    positionAcc: List[(T[V], Boolean)]
  ): Option[RedBlackTreeMapExplorer[V]] = None

  // duplicates
  def values: List[V] = Nil

  def content: List[(Int, V)] = Nil

  override def keys: List[Int] = Nil

  override def positionOf(k: Int): Option[RedBlackTreeMapExplorer[V]] = None

  // insert: Insert a value at a key.
  override def insert(k: Int, v: V): RedBlackTreeMap[V] = T(B, L(), k, Some(v), L())

  // remove: Delete a key.
  override def remove(k: Int): RedBlackTreeMap[V] = this

  override def smallest: Option[(Int, V)] = None

  override def biggest: Option[(Int, V)] = None

  override def biggestPosition: Option[RedBlackTreeMapExplorer[V]] = None

  override def smallestPosition: Option[RedBlackTreeMapExplorer[V]] = None

  override def updateDelta(
    fromKeyIncluded: Int,
    toKeyIncluded: Int,
    deltaKey: Int,
    transform: V => V
  ): RedBlackTreeMap[V] = this

  override def update(
    fromKeyIncluded: Int,
    toKeyIncluded: Int,
    transform: (Int, V) => (Int, V)
  ): RedBlackTreeMap[V] = this
}

// helper object for the tree node
private[rb] object T {
  def unapply[V](
    t: T[V]
  ): Option[(Boolean, RedBlackTreeMap[V], Int, Option[V], RedBlackTreeMap[V])] = {
    t.unapply
  }

  def apply[V](c: Boolean, l: RedBlackTreeMap[V], k: Int, v: Option[V], r: RedBlackTreeMap[V]) =
    new T(c, l, k, v, r)
}

// A tree node.
private[rb] class T[@specialized(Int) V](
  private[this] val c: Boolean,
  private[this] val l: RedBlackTreeMap[V],
  private[this] val k: Int,
  private[this] val v: Option[V],
  private[this] val r: RedBlackTreeMap[V]
) extends RedBlackTreeMap[V] {

  def anyValue: Option[V] = v

  def unapply: Option[(Boolean, RedBlackTreeMap[V], Int, Option[V], RedBlackTreeMap[V])] =
    Some(c, l, k, v, r)

  private[rb] def pk = k
  private[rb] def pl = l
  private[rb] def pr = r
  private[rb] def pv = v

  assert(v.nonEmpty)

  override val size: Int = l.size + r.size + 1
  override def isEmpty   = false

  def get(k: Int): Option[V] = {
    if (k < this.k) l.get(k)
    else if (k > this.k) r.get(k)
    else v
  }

  def getOrElse(k: Int, default: => V): V = get(k) match {
    case None    => default
    case Some(x) => x
  }

  override def contains(k: Int): Boolean = {
    if (k < this.k) l.contains(k)
    else if (k > this.k) r.contains(k)
    else true
  }

  def biggestLowerOrEqual(k: Int): Option[(Int, V)] = {
    if (k < this.k) l.biggestLowerOrEqual(k)
    else if (this.k < k) {
      val result = r.getBiggestLowerAcc(k, this.k, v.get)
      Some((result.k, result.value))
    } else Some(k, v.get)
  }

  override protected[rb] def getBiggestLowerAcc(
    k: Int,
    bestKSoFar: Int,
    bestVSoFar: V
  ): IntVCouple[V] = {
    if (k < this.k) l.getBiggestLowerAcc(k, bestKSoFar, bestVSoFar)
    else if (this.k < k) r.getBiggestLowerAcc(k, this.k, v.get)
    else new IntVCouple(k, v.get)
  }

  override def smallestBiggerOrEqual(k: Int): Option[(Int, V)] = {
    if (k < this.k) {
      val result = l.getSmallestBiggerAcc(k, this.k, v.get)
      Some((result.k, result.value))
    } else if (this.k < k) r.smallestBiggerOrEqual(k)
    else Some(k, v.get)
  }

  override protected[rb] def getSmallestBiggerAcc(
    k: Int,
    bestKSoFar: Int,
    bestVSoFar: V
  ): IntVCouple[V] = {
    if (k < this.k) l.getSmallestBiggerAcc(k, this.k, v.get)
    else if (this.k < k) r.getSmallestBiggerAcc(k, bestKSoFar: Int, bestVSoFar: V)
    else new IntVCouple(k, v.get)
  }

  override protected[rb] def modWith(
    k: Int,
    f: (Int, Option[V]) => Option[V]
  ): RedBlackTreeMap[V] = {
    if (k < this.k) balance(c, l.modWith(k, f), this.k, this.v, r)
    else if (k == this.k) {
      f(this.k, this.v) match {
        case None =>
          if (l.isEmpty) r
          else if (r.isEmpty) l
          else {
            r.smallest match {
              case Some((rk, rv)) => T(c, l, rk, Some(rv), r.remove(rk))
              case None           => throw new Error("non smallest on non-empty RB?")
            }
          }
        case x => T(c, l, k, x, r)
      }
    } else {
      balance(c, l, this.k, this.v, r.modWith(k, f))
    }
  }

  override protected[rb] def valuesAcc(valuesAfter: List[V]): List[V] =
    l.valuesAcc(v.get :: r.valuesAcc(valuesAfter))

  override protected[rb] def contentAcc(valuesAfter: List[(Int, V)]): List[(Int, V)] =
    l.contentAcc((k, v.get) :: r.contentAcc(valuesAfter))

  override protected[rb] def keysAcc(keysAfter: List[Int]): List[Int] =
    l.keysAcc(k :: r.keysAcc(keysAfter))

  protected[rb] override def positionOfAcc(
    k: Int,
    positionAcc: List[(T[V], Boolean)]
  ): Option[RedBlackTreeMapExplorer[V]] = {
    if (k < this.k) l.positionOfAcc(k, (this, false) :: positionAcc)
    else if (k > this.k) r.positionOfAcc(k, (this, true) :: positionAcc)
    else Some(new RedBlackTreeMapExplorer[V]((this, true) :: positionAcc))
  }

  // unused; keeping here in case required in the future
  // def hasLeft: Boolean  = l.isInstanceOf[T[V]]
  // def hasRight: Boolean = r.isInstanceOf[T[V]]

  // duplicates
  override def values: List[V] = valuesAcc(Nil)

  override def content: List[(Int, V)] = contentAcc(Nil)

  override def keys: List[Int] = keysAcc(Nil)

  override def positionOf(k: Int): Option[RedBlackTreeMapExplorer[V]] = positionOfAcc(k: Int, Nil)

  // insert: Insert a value at a key.
  override def insert(k: Int, v: V): RedBlackTreeMap[V] = blacken(modWith(k, (_, _) => Some(v)))

  // remove: Delete a key.
  override def remove(k: Int): RedBlackTreeMap[V] = blacken(modWith(k, (_, _) => None))

  override def smallest: Option[(Int, V)] = smallestBiggerOrEqual(Int.MinValue)

  override def biggest: Option[(Int, V)] = biggestLowerOrEqual(Int.MaxValue)

  override def biggestPosition: Option[RedBlackTreeMapExplorer[V]] = {
    biggestLowerOrEqual(Int.MaxValue) match {
      case Some((rk, _)) => positionOf(rk)
      case None          => None
    }
  }

  override def smallestPosition: Option[RedBlackTreeMapExplorer[V]] = {
    smallestBiggerOrEqual(Int.MinValue) match {
      case Some((rk, _)) => positionOf(rk)
      case None          => None
    }
  }

  override def update(
    fromKeyIncluded: Int,
    toKeyIncluded: Int,
    transform: (Int, V) => (Int, V)
  ): RedBlackTreeMap[V] = {
    val newLeft = if (fromKeyIncluded < k) {
      l.update(fromKeyIncluded, toKeyIncluded, transform)
    } else {
      l
    }
    // this method ensures that the keys are traversed in ascending order,
    // so the code is structures in this  way with identical fragments of code that must not not be factorized
    if (fromKeyIncluded <= k && k <= toKeyIncluded) {
      // this one must be transformed as well
      val (newK, newV) = transform(k, v.get)
      val newRight = if (k < toKeyIncluded) {
        r.update(fromKeyIncluded, toKeyIncluded, transform)
      } else {
        r
      }
      new T(c, newLeft, newK, Some(newV), newRight)
    } else {
      // this one does not need transform
      val newRight = if (k < toKeyIncluded) {
        r.update(fromKeyIncluded, toKeyIncluded, transform)
      } else {
        r
      }
      if (newLeft == l && newRight == r) {
        this
      } else {
        new T(c, newLeft, k, v, newRight)
      }
    }
  }

  override def updateDelta(
    fromKeyIncluded: Int,
    toKeyIncluded: Int,
    deltaKey: Int,
    transform: V => V
  ): RedBlackTreeMap[V] = {
    val newLeft = if (fromKeyIncluded < k) {
      l.updateDelta(fromKeyIncluded, toKeyIncluded, deltaKey, transform)
    } else {
      l
    }
    val newRight = if (k < toKeyIncluded) {
      r.updateDelta(fromKeyIncluded, toKeyIncluded, deltaKey, transform)
    } else {
      r
    }
    if (fromKeyIncluded <= k && k <= toKeyIncluded) {
      // this one must be transformed as well
      new T(c, newLeft, k + deltaKey, Some(transform(v.get)), newRight)
    } else {
      // this one does not need transform
      if (newLeft == l && newRight == r) {
        this
      } else {
        new T(c, newLeft, k, v, newRight)
      }
    }
  }
}
