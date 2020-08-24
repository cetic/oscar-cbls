/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.visual

import java.awt.Color

/**
 * Provides some tools for the visual package.
 *
 * @author Renaud Hartert ren.hartert@gmail.com
 * @author Pierre Schaus  pierre.schaus@gmail.com
 */

object VisualUtil {

  private val rand = new scala.util.Random(2001)
  private val pastelCoef: Float = 0.65f
  private val brightCoef: Float = 1f

  // Introduces variance in the saturation
  private def randSat: Float = 0.75f + rand.nextFloat() * 0.25f

  // Builds a new color according to its hue, saturation, and brightness.
  private def hsbColor(h: Float, s: Float, b: Float): Color = new Color(Color.HSBtoRGB(h, s, b))

  /** Returns an array of n random colors. */
  def getRandomColors(n: Int, pastel: Boolean = false): Array[Color] = {
    val hStep: Float = 1f / n
    val coef: Float = if (pastel) pastelCoef else brightCoef
    val colors = Array.tabulate(n)(i => hsbColor(i * hStep, coef * randSat, randSat))
    colors.sortBy(_ => rand.nextInt())
  }

  /** Returns an array of n hue-uniform colors. */
  def getUniformColors(n: Int, pastel: Boolean = false): Array[Color] = {
    val hStep: Float = 1f / n
    val coef: Float = if (pastel) pastelCoef else brightCoef
    Array.tabulate(n)(i => hsbColor(i * hStep, coef, 1f))
  }

  /** Returns a random bright color. */
  def getRandomColor: Color = getRandomColor(false)

  /** Returns a random pastel of bright color. */
  def getRandomColor(pastel: Boolean) = {
    val coef: Float = if (pastel) pastelCoef else brightCoef
    hsbColor(rand.nextFloat(), coef, 1f)
  }

  // Old system of colors
  // --------------------

  /** Returns an array of n colors generated with getRandomLegacyColor. */
  def getRandomLegacyColors(n: Int): Array[Color] = Array.fill(n)(getRandomLegacyColor)

  /** Returns a random color (previous implementation of getRandomColor). */
  def getRandomLegacyColor: Color = new Color((next * 255).toInt, (next * 255).toInt, (next * 255).toInt)

  private def next: Double = rand.nextDouble() * 0.5 + 0.4
}
