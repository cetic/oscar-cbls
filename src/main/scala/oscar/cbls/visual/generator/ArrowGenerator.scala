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

/** The geometry of an arrow drawn from one point to another.
  *
  * @param shaftEndX
  *   The X coordinate where the shaft (the line part of the arrow) should end.
  * @param shaftEndY
  *   The Y coordinate where the shaft (the line part of the arrow) should end.
  * @param headPoints
  *   The 6 flattened (x, y, x, y, x, y) coordinates of the 3-point arrowhead triangle. Empty if
  *   the arrow is degenerate (origin and destination are the same point).
  */
case class Arrow(shaftEndX: Double, shaftEndY: Double, headPoints: Seq[Double])

/** Computes the geometry of an arrow, made of a shaft and a triangular arrowhead, used to display
  * the direction of travel of an edge in a routing visualisation.
  */
object ArrowGenerator {

  // Length of the arrowhead triangle, from base to tip, in pixels.
  private final val HEAD_LENGTH: Double = 8.0
  // Half-width of the arrowhead triangle's base, in pixels.
  private final val HEAD_HALF_WIDTH: Double = 4.0

  /** Computes the shaft end-point and arrowhead triangle points for an edge drawn from (x1,y1) to
    * (x2,y2), with the arrowhead tip landing `nodeRadius` pixels short of (x2,y2) along the
    * origin-to-destination direction, so the tip touches the destination node's circle boundary
    * rather than its center.
    *
    * @param nodeRadius
    *   The radius, in pixels, of the destination node's circle.
    * @return
    *   The [[Arrow]] geometry, or an [[Arrow]] with empty `headPoints` if (x1,y1) == (x2,y2) (a
    *   degenerate, zero-length edge) — callers should skip drawing anything in that case.
    */
  def computeArrow(x1: Double, y1: Double, x2: Double, y2: Double, nodeRadius: Double): Arrow = {
    val dx     = x2 - x1
    val dy     = y2 - y1
    val length = Math.hypot(dx, dy)

    if (length == 0.0) {
      Arrow(x1, y1, Seq.empty)
    } else {
      val ux = dx / length
      val uy = dy / length

      val tipX = x2 - ux * nodeRadius
      val tipY = y2 - uy * nodeRadius

      val shaftEndX = tipX - ux * HEAD_LENGTH
      val shaftEndY = tipY - uy * HEAD_LENGTH

      val px = -uy
      val py = ux

      Arrow(
        shaftEndX,
        shaftEndY,
        Seq(
          tipX,
          tipY,
          shaftEndX + px * HEAD_HALF_WIDTH,
          shaftEndY + py * HEAD_HALF_WIDTH,
          shaftEndX - px * HEAD_HALF_WIDTH,
          shaftEndY - py * HEAD_HALF_WIDTH
        )
      )
    }
  }
}
