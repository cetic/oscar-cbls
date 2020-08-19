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

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class IncrementalStatistics {
  
  private[this] var n = 0
  private[this] var  s = 0.0 // maintain sum_i xi
  private[this] var  s2 = 0.0 // maintain sum_i xi^2
  private[this] var vari = 0.0 // maintain sum_i (xi-s/n)^2
  
  def reset(): Unit = {
    n = 0
    s = 0
    s2 = 0
    vari = 0
  }
  
  def addPoint(x: Double): Unit = {
    s += x
    n += 1
    if (n >= 1) {
      vari = s*s / n + x*x - 2 * x * s / n + s2 - 2 * s / n * (s - x)    
    }
    s2 += x*x
  }
  
  def sum: Double = s
  
  def average: Double = sum/n
  
  def variance: Double = vari/n

}