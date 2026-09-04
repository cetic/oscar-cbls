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

/** Generates Colors evenly spaced around the hue wheel (HSB color space), so that a fixed number of
  * colors are mutually well-contrasted.
  *
  * Hues are laid out starting from a fixed base hue then spaced evenly by 360° / number,
  * guaranteeing the maximum possible minimum angular distance between any two of the generated
  * colors' hues.
  */
object ColorGenerator {
  // Starting hue, in degrees (0 = red). The exact value doesn't affect contrast — saturation and
  // brightness do — so a fixed anchor is enough; no need for run-to-run randomness here.
  private final val BASE_HUE_DEGREES: Double   = 0.0
  private final val DEFAULT_SATURATION: Double = 0.85
  private final val DEFAULT_BRIGHTNESS: Double = 0.85

  private val randomValueGenerator = new Random()

  /** Sets the seed of random generator
    * @param seed
    *   value of seed
    */
  def setSeed(seed: Long): Unit = randomValueGenerator.setSeed(seed)

  /** Generates an array of colors, evenly spaced around the hue wheel so that any two of them are
    * maximally distinguishable, with a fixed saturation/brightness.
    *
    * @param number
    *   The number of colors to generate.
    * @param alpha
    *   The opacity level, between 0 and 1.
    * @param saturation
    *   The HSB saturation of the generated colors, between 0 and 1. Defaults to a value suited for
    *   a white background.
    * @param brightness
    *   The HSB brightness of the generated colors, between 0 and 1. Defaults to a value suited for
    *   a white background.
    * @return
    *   The array of colors.
    */
  def generateContrastingColors(
    number: Int,
    alpha: Float = 1.0f,
    saturation: Double = DEFAULT_SATURATION,
    brightness: Double = DEFAULT_BRIGHTNESS
  ): Array[Color] = {
    val hueStep = 360.0 / number
    Array.tabulate(number) { i =>
      val hue = (BASE_HUE_DEGREES + i * hueStep) % 360.0
      Color.hsb(hue, saturation, brightness, alpha)
    }
  }

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
