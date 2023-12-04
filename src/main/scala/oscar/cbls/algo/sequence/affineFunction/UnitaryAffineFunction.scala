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

package oscar.cbls.algo.sequence.affineFunction


/** The companion object of [[UnitaryAffineFunction]] */
object UnitaryAffineFunction {

  /** The sequence affine function that doesn't change the sequence */
  val identity = new UnitaryAffineFunction(0, false)

  /** Returns a [[UnitaryAffineFunction]] instance with the specified offset and flip flag
    *
    * @param offset
    *   The offset value
    * @param flip
    *   Whether or not we need to flip the subsequence
    * @return
    *   A [[UnitaryAffineFunction]]
    */
  def apply(offset: Int, flip: Boolean) = new UnitaryAffineFunction(offset, flip)
}

/** An affine function used to track the changes of position after a movement in an [[IntSequence]]
  *
  * It works as a bijection :
  *   - apply() &rarr; old position to new position
  *   - unapply() &rarr; new position to old position
  * @param offset
  *   The offset value as a [[scala.Int]]
  * @param flip
  *   Flipping flag
  *   - If true : -x + b
  *   - If false : x + b
  */
class UnitaryAffineFunction(val offset: Int, val flip: Boolean) {

  /** Creates the composition of the this [[UnitaryAffineFunction]] and that
    * [[UnitaryAffineFunction]].
    *
    * @param that
    *   the other [[UnitaryAffineFunction]]
    * @return
    *   The composition of this and that [[UnitaryAffineFunction]]
    */
  def apply(that: UnitaryAffineFunction): UnitaryAffineFunction = {
    new UnitaryAffineFunction(this(that.offset), this.flip != that.flip)
  }

  /** Returns the reverse [[UnitaryAffineFunction]] of this [[UnitaryAffineFunction]].
    *
    * Basically the reversing the apply(value: Int) and unApply(value: Int) methods
    */
  def invert: UnitaryAffineFunction =
    new UnitaryAffineFunction(if (flip) offset else -offset, flip)

  /** Applies the linear transformation on a value */
  def apply(value: Int): Int = if (flip) -value + offset else value + offset

  /** Un-applies the linear transformation on a value */
  def unApply(value: Int): Int = if (flip) -value + offset else value - offset

  /** Checks if this [[UnitaryAffineFunction]] is an identity affine function */
  def isIdentity: Boolean = offset == 0 && !flip

  override def toString: String =
    if (offset == 0 && flip) "(x=>-x)"
    else if (offset == 0) "(x=>x)"
    else s"(x=> $offset ${if (flip) "-" else "+"} x)"

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: UnitaryAffineFunction =>
        this.offset == that.offset && this.flip == that.flip
      case _ => false
    }
  }
}
