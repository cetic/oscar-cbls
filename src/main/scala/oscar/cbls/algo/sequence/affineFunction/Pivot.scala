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

/** Represents the shifting bijection of a subsequence.
  *
  * With each moved subsequence comes a [[Pivot]]. The subsequence starts at fromValue and end at
  * the next [[Pivot]]'s start (if one) We can get the new position of each element of the
  * subsequence by using the [[UnitaryAffineFunction]]
  *
  * @param fromValue
  *   The starting position of the subsequence
  * @param f
  *   The [[UnitaryAffineFunction]] attached to this subsequence
  */
class Pivot(val fromValue: Int, val f: UnitaryAffineFunction) {
  override def toString: String =
    "Pivot(from:" + fromValue + " " + f + " f(from)=" + f(fromValue) + "))"

  /** Flip this pivot */
  def flip(endX: Int): Pivot = {
    if (f.flip) {
      new Pivot(fromValue, UnitaryAffineFunction(f(endX) - fromValue, flip = false))
    } else {
      new Pivot(fromValue, UnitaryAffineFunction(fromValue + f(endX), flip = true))
    }
  }
}
