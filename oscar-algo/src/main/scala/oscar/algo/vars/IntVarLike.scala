package oscar.algo.vars

import oscar.algo.search.IntConstrainableContext

import scala.util.Random

/**
 * A trait that all objects that behave like an IntVar should implement
 */
trait IntVarLike extends Iterable[Int] {
  def context: IntConstrainableContext

  def isContinuous: Boolean = size == (max - min + 1)

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  def isBound: Boolean

  /**
   * @param v: value to test
   * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
   */
  def isBoundTo(v: Int): Boolean = isBound && hasValue(v)

  /**
   * Test if a value is in the domain
   * @param value: value to test
   * @return  true if the domain contains the value val, false otherwise
   */
  def hasValue(value: Int): Boolean

  /**
   * @param value
   * @return the smallest value > val in the domain
   */
  def valueAfter(value: Int): Int

  /**
   * @param value
   * @return the largest value < val in the domain
   */
  def valueBefore(value: Int): Int

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  def randomValue(rand: Random): Int

  /**
   * @return the size of the domain
   */
  def size: Int

  /**
   * @return true if domain is empty, false else
   */
  def isEmpty: Boolean

  /**
   * @return  the minimum value in the domain
   */
  def min: Int

  /**
   * @return  the maximum value in the domain
   */
  def max: Int

  /**
    * The built-in minBy will return the first minimum value
    * encountered by the iterator. Due to backtracking and
    * reversible data-structures, the order of values returned
    * by the iterator can change over time
    * (even when the values in the domain are unchanged).
    * Therefore it is better to return the smallest,
    * rather than first, minimum value.
    *
    * This makes the minBy function stable.
    *
    * The only overhead is the code
    * "|| (cmp.lteq(fx, minF) && elem < minElem)"
    * which should be insignificant in terms of speed.
    * The rest of the code is the native scala implementation.
    *
    * @return  the smallest minimum value in the domain by function f
    */
  override def minBy[B](f: Int => B)(implicit cmp: Ordering[B]): Int = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.minBy")

    var minF: B = null.asInstanceOf[B]
    var minElem: Int = null.asInstanceOf[Int]
    var first = true

    for (elem <- iterator) {
      val fx = f(elem)
      if (first || cmp.lt(fx, minF) || (cmp.lteq(fx, minF) && elem < minElem)) {
        minElem = elem
        minF = fx
        first = false
      }
    }
    minElem
  }

  /**
    * See minBy for description.
    *
    * @return  the smallest maximum value in the domain by function f
    */
  override def maxBy[B](f: Int => B)(implicit cmp: Ordering[B]): Int = minBy(f)(cmp.reverse)

  def iterator: Iterator[Int]

  def foreach[U](f: Int => U): Unit

  /**
   * @return an (not sorted) array representation of the domain.
   */
  def toArray: Array[Int] = iterator.toArray

  /**
   * @param array.length >= this.size
   * @return Fills the array with the domain.
   *         returns the number of values (this.size).
   *         The array is not sorted.
   */
  def fillArray(array: Array[Int]): Int= {
    val ite = iterator
    var i = 0
    while (ite.hasNext) {
      array(i) = ite.next()
      i += 1
    }
    i
  }

  /**
   * Return a representative name for this var(-like), if one was given
   */
  def name: String
}