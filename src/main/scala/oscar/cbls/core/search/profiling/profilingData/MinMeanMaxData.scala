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

/** Profiles and computes the min, mean and max value of a variable during a search.
  *
  * ex: Exhaust time
  */
case class MinMeanMaxData(initValue: Long) extends CombinatorProfilingData {
  private var _min: Long  = initValue
  private var _max: Long  = initValue
  private var _count: Int = 1
  private var _sum: Long  = initValue

  def min: Long  = _min
  def max: Long  = _max
  def count: Int = _count
  def sum: Long  = _sum

  def add(value: Long): Unit = {
    _min = Math.min(value, _min)
    _max = Math.max(value, _max)
    _count += 1
    _sum += value
  }

  override def merge(other: CombinatorProfilingData): Unit = {
    other match {
      case o: MinMeanMaxData =>
        _min = Math.min(_min, o.min)
        _max = Math.max(_max, o.max)
        _sum += o.sum
        _count += o.count
    }
  }

  override def collectStatisticsHeaders(): List[String] =
    List("Min", "Mean", "Max", "Sum", "Count")

  override def collectStatisticsData(): List[String] = {
    List(
      _min.toString,
      if (_count != 0) ((_sum * 1000 / _count) / 1000.0).toString else "NA",
      _max.toString,
      _sum.toString,
      _count.toString
    )
  }
}
