package oscar.cbls.visual.generator

import scalafx.scene.paint.Color

import scala.annotation.tailrec
import scala.util.Random

/**
 * The utility of this object is to generate a pseudo-random array of Color objects.
 * For the same amount of color needed, the returned array will always contain the same colors
 * but in a different order.
 *
 * @author fabian.germeau@cetic.be
 */
object ColorGenerator {
  private val randomValueGenerator = new Random()

  /**
   * Sets the seed of random generator
   * @param seed value of seed
   */
  def setSeed(seed: Long): Unit = randomValueGenerator.setSeed(seed)

  /**
   * Generates a list of random Color objects with a fixed size and opacity
   * @param number the number of colors to generate
   * @param alpha the opacity level, between 0 and 1
   * @return the list of Colors
   */
  def generateRandomColors(number:Int, alpha:Float = 1.0f): Array[Color] = {
    Array.fill(number)(
      Color(
        randomValueGenerator.nextFloat(),
        randomValueGenerator.nextFloat(),
        randomValueGenerator.nextFloat(),
        alpha
      )
    )
  }

  /**
   * Obtaining the maximum number of generable colors
   * @param number the number of colors needed
   * @param exp the current base value
   * @return the maximum number of colors that can be generated
   */
  @tailrec
  def getMaxColorNumber(number:Int, exp:Int = 1):Int = {
    if (Math.pow(exp,3) < number)
      getMaxColorNumber(number,exp+1)
    else
      exp
  }

  /**
   * Generates a color from a hashcode
   * @param hash the hashcode
   * @return the color object
   */
  def generateColorFromHash(hash:Int): Color = {
    val absHash = Math.abs(hash)
    val r = absHash%255
    val g = 255 - (absHash/255)%255
    val b = ((absHash/255)/255)%255
    Color.rgb(r,g,b)
  }

  /**
   * Generate the "average" Color object from a list of Color objects
   * @param colors a list of Color objects
   * @return a Color object whose rgb components are the average of the rgb
   *         components of the list
   */

  def getAverageColor(colors:List[Color]): Color = {
    var (r,g,b) = (0d,0d,0d)
    for(c <- colors){
      r += c.getRed
      g += c.getGreen
      b += c.getBlue
    }
    Color.rgb((r/colors.size).toInt, (g/colors.size).toInt, (b/colors.size).toInt)
  }
}
