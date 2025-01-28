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

package oscar.cbls.visual.generator

import scalafx.scene.paint.Color

import scala.util.Random

/** Generates pseudo-random Color.
  *
  * For the same amount of color needed, the returned array will always contain the same colors but
  * in a different order.
  */
object ColorGenerator {
  private val randomValueGenerator = new Random()

  /** Sets the seed of random generator
    * @param seed
    *   value of seed
    */
  def setSeed(seed: Long): Unit = randomValueGenerator.setSeed(seed)

  /** Generates an array of random colors with a fixed size and opacity.
    *
    * @param number
    *   The number of colors to generate.
    * @param alpha
    *   The opacity level, between 0 and 1.
    * @return
    *   The array of colors.
    */
  def generateRandomColors(number: Int, alpha: Float = 1.0f): Array[Color] = {
    Array.fill(number)(
      Color(
        randomValueGenerator.nextFloat(),
        randomValueGenerator.nextFloat(),
        randomValueGenerator.nextFloat(),
        alpha
      )
    )
  }

  /** Generates a color from a hashcode. */
  def generateColorFromHash(hash: Int): Color = {
    val absHash = Math.abs(hash)
    val r       = absHash                 % 255
    val g       = 255 - (absHash / 255)   % 255
    val b       = ((absHash / 255) / 255) % 255
    Color.rgb(r, g, b)
  }

  /** Generates a color by mixing a list of colors.
    * @param colors
    *   The list of color.
    * @return
    *   A color whose rgb components are the average of the rgb components of the list.
    */

  def getMixedColor(colors: List[Color]): Color = {
    var (r, g, b) = (0d, 0d, 0d)
    for (c <- colors) {
      r += c.getRed
      g += c.getGreen
      b += c.getBlue
    }
    Color.rgb((r / colors.size).toInt, (g / colors.size).toInt, (b / colors.size).toInt)
  }
}
