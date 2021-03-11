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

package oscar.modeling.vars.domainstorage

import oscar.algo.vars.IntVarLike
import oscar.modeling.vars.IntVarImplem

import scala.collection.immutable.SortedSet
import scala.util.Random

class IntDomainStorage(val content: Iterable[Int], val name: String) extends DomainStorage with IntVarImplem with IntVarLike
{
  override def isContinuous: Boolean = (content.max - content.min) == (content.size-1)
  override def context = throw new Exception("This variable is not instantiated and thus has no context")

  /**
    * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
    */
  override def isBound: Boolean = content.size == 1

  /**
    * @param v : value to test
    * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
    */
  override def isBoundTo(v: Int): Boolean = isBound && content.head == v

  /**
    * Test if a value is in the domain
    *
    * @param value : value to test
    * @return true if the domain contains the value val, false otherwise
    */
  override def hasValue(value: Int): Boolean = {
    content match {
      case a: Set[Int] => a.contains(value)
      case b: Range => b.contains(value)
      case _ => content.toSet.contains(value)
    }
  }

  /**
    * @param value
    * @return the smallest value > val in the domain
    */
  override def valueAfter(value: Int): Int = {
    content match {
      case b: Range => if(b.contains(value+1)) value+1 else Int.MaxValue
      case c: SortedSet[Int] => c.rangeFrom(value + 1).headOption.get
      case _ => (SortedSet[Int]() ++ content).rangeFrom(value + 1).headOption.get
    }
  }

  /**
    * @param value
    * @return the largest value < val in the domain
    */
  override def valueBefore(value: Int): Int = {
    content match {
      case b: Range => if(b.contains(value-1)) value-1 else Int.MinValue
      case c: SortedSet[Int] => c.rangeTo(value - 1).lastOption.get
      case _ => (SortedSet[Int]() ++ content).rangeTo(value - 1).lastOption.get
    }
  }

  /**
    * @return A random value in the domain of the variable (uniform distribution)
    */
  override def randomValue(rand: Random): Int = content.toVector(rand.nextInt(size))

  /**
    * @return the minimum value in the domain
    */
  override def min: Int = content.min

  /**
    * @return the maximum value in the domain
    */
  override def max: Int = content.max

  override def iterator: Iterator[Int] = content.iterator

  /**
    * @return an (not sorted) array representation of the domain.
    */
  override def toArray: Array[Int] = content.toArray
}

object IntDomainStorage {
  /**
   * Create a new IntDomainStorage from a Set of values
   * @param s
   */
  def apply(s: Set[Int], name: String) = new IntDomainStorage(s, name)
  def apply(s: Set[Int]) = new IntDomainStorage(s, "")

  /**
   * Create a new IntDomainStorage containing only a single value
   * @param v
   */
  def apply(v: Int, name: String) = new IntDomainStorage(Array(v), name)
  def apply(v: Int) = new IntDomainStorage(Array(v), "")

  /**
   * Create a new IntDomainStorage, with domain from min to max, inclusive
   * @param min
   * @param max
   */
  def apply(min: Int, max: Int, name: String) = new IntDomainStorage(Range(min, max+1), name)
  def apply(min: Int, max: Int) = new IntDomainStorage(Range(min, max+1), "")
}