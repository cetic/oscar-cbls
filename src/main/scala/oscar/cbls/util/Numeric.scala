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

package oscar.cbls.util

/** A collection of various numeric utility methods. */
object Numeric {

  /** Truncates a bigInt number within the range of [[Long]] values. For instance, if the number is
    * larger than [[Long.MaxValue]], the output will be [[Long.MaxValue]]. Use with caution.
    */
  def limitToLong(bi: BigInt): Long = ((bi min Long.MaxValue) max Long.MinValue).toLong
}
