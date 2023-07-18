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

package oscar.cbls.algo.fun

/** The companion object of [[LinearTransform]] */
object LinearTransform {

  /** The linear transformation that doesn't change the value */
  val identity = new LinearTransform(0, false)

  /** Returns a [[LinearTransform]] instance with the specified offset and operation
    * @param offset
    *   The value to add
    * @param opposite
    *   Whether or not we get the opposite of the value before adding the offset
    * @return
    *   A [[LinearTransform]]
    */
  def apply(offset: Int, opposite: Boolean) = new LinearTransform(offset, opposite)
}

/** A linear transformation class executing the transformation defined by its two parameters.
  * @param offset
  *   The value to add as an [[scala.Int]]
  * @param opposite
  *   Whether or not we get the opposite of the value before adding the offset
  */
class LinearTransform(val offset: Int, val opposite: Boolean) {

  /** Creates the composition of the this [[LinearTransform]] and that [[LinearTransform]].
    *
    * @param that
    *   the other [[LinearTransform]]
    * @return
    *   The composition of this and that [[LinearTransform]]
    */
  def apply(that: LinearTransform): LinearTransform = {
    new LinearTransform(this(that.offset), this.opposite != that.opposite)
  }

  /** Returns the inverse of this LinearTransform. Meaning, the [[LinearTransform]] that un-apply
    * this [[LinearTransform]]. See unApply() method for more details
    */
  def invert: LinearTransform = new LinearTransform(if (opposite) offset else -offset, opposite)

  /** Applies the linear transformation on a value */
  def apply(value: Int): Int = if (opposite) -value + offset else value + offset

  /** Un-applies the linear transformation on a value */
  def unApply(value: Int): Int = if (opposite) -value + offset else value - offset

  /** Checks if this [[LinearTransform]] is an identity transformation, meaning it doesn't change
    * the passed value.
    */
  def isIdentity: Boolean = offset == 0 && !opposite

  override def toString: String =
    if (offset == 0 && opposite) "(x=>-x)"
    else if (offset == 0) "(x=>x)"
    else s"(x=> $offset ${if (opposite) "-" else "+"} + x"

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: LinearTransform => this.offset == that.offset && this.opposite == that.opposite
      case _                     => false
    }
  }
}
