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

package oscar.cbls.algo.generator

import scala.math.{pow, round, sqrt}
import scala.util.Random

private[generator] object GeneratorUtil {

  val rng: Random = Random

  def distance(from: (Long, Long), to: (Long, Long)): Long =
    round(sqrt(pow((from._1 - to._1).toDouble, 2.0) + pow((from._2 - to._2).toDouble, 2.0)))

  /** Returns a random tuple of coordinates.
    *
    * @param xMin
    *   Inclusive lower bound of the X coordinate.
    * @param xMax
    *   Inclusive upper bound of the X coordinate.
    * @param yMin
    *   Inclusive lower bound of the Y coordinate.
    * @param yMax
    *   Inclusive upper bound of the Y coordinate.
    * @return
    *   A tuple `(x, y)` such that `xMin <= x <= xMax` and `yMin <= y <= yMax`
    */
  def randomPosition(xMin: Long, xMax: Long, yMin: Long, yMax: Long): (Long, Long) =
    (rng.between(xMin, xMax + 1), rng.between(yMin, yMax + 1))

  /** Tests if `x` is in `[a, b]` */
  def inInterval(x: Long, a: Long, b: Long): Boolean = a <= x && x <= b
}
