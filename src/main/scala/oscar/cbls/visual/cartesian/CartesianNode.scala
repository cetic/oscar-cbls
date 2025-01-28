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

package oscar.cbls.visual.cartesian

// Companion object of CartesianNode
object CartesianNode {

  /** Generates a CartesianNode.
    *
    * It's mainly use to fit adjust the node coordinate to the size of the window.
    *
    * @param coordinates
    *   The coordinates as defined in the Local Search problem.
    */
  def apply(coordinates: Array[(Long, Long)]): Array[CartesianNode] = {
    val maxXDiff: Long = coordinates.maxBy(_._1)._1 - coordinates.minBy(_._1)._1
    val maxYDiff: Long = coordinates.maxBy(_._2)._2 - coordinates.minBy(_._2)._2
    coordinates.map(c => new CartesianNode(c, maxXDiff, maxYDiff))
  }
}

/** Representation of a node in a cartesian plan.
  *
  * It's mainly use to fit adjust the node coordinate to the size of the window.
  *
  * @param realCoordinates
  *   The coordinates as defined in the Local Search problem.
  * @param maxXDiff
  *   The maximum distance between two nodes regarding the X axis.
  * @param maxYDiff
  *   The maximum distance between two nodes regarding the Y axis.
  */
class CartesianNode(val realCoordinates: (Long, Long), maxXDiff: Long, maxYDiff: Long) {
  // Keeping some margin to avoid having node sticking to the borders.
  private final val MARGIN: Int = 20

  private var _resizedCoordinates: (Double, Double) =
    (realCoordinates._1.toDouble, realCoordinates._2.toDouble)

  def resizedCoordinates: (Double, Double) = _resizedCoordinates

  /** Given the width and the height, replaces the resizedCoordinate value of this CartesianNode.
    *
    * @param wWidth
    *   The width of the window in which the node is displayed.
    * @param wHeight
    *   The height of the window in which the node is displayed.
    */
  def resize(wWidth: Int, wHeight: Int): Unit = {
    val scale = Math.min(
      (wWidth - 2 * MARGIN) / maxXDiff.toDouble,
      (wHeight - 2 * MARGIN) / maxYDiff.toDouble
    )
    _resizedCoordinates = (MARGIN + realCoordinates._1 * scale, MARGIN + realCoordinates._2 * scale)
  }
}
