/*******************************************************************************
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
 ******************************************************************************/
package oscar.util

import scala.io.{BufferedSource, Source}

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class InFile(filepath: String) {
  val bufSource: BufferedSource = Source.fromFile(filepath)
  val lines: String = bufSource.getLines.reduceLeft(_ + " " + _)
  val vals: Seq[Int] = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
  var index = 0
    
  def nextInt(): Int = {
    index += 1
    vals(index - 1)
  }
}

object InFile {
  def apply(filepath: String) = new InFile(filepath)
}
