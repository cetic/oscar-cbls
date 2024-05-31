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

package oscar.cbls.core.search.profiling.profilingData

/** Profiles and computes the summed value of a variable during the search.
  *
  * Ex : the number of time Guard prevent a movement
  */
case class SummedValue() extends CombinatorProfilingData {
  var _sum: Long = 0L

  def sum: Long = _sum

  def plus(value: Long): Unit = _sum += value

  override def merge(other: CombinatorProfilingData): Unit = {
    other match {
      case o: SummedValue => _sum += o.sum
    }
  }

  override def collectStatisticsHeaders(): Array[String] = Array("Sum")
  override def collectStatisticsData(): Array[String]    = Array(_sum.toString)
}
