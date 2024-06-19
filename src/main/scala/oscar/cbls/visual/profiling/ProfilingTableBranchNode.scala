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

package oscar.cbls.visual.profiling

import oscar.cbls.core.search.profiling.SearchProfiler

case class ProfilingTableBranchNode(
  override val profiler: SearchProfiler,
  parent: Option[ProfilingTableBranchNode],
  rowPrefix: List[String]
) extends ProfilingTableNode(profiler, parent, rowPrefix) {

  var children: List[ProfilingTableNode] = List.empty

  var isExpanded: Boolean = false

  def hasChildren: Boolean = true

  def addChildren(children: List[ProfilingTableNode]): Unit = {
    this.children = children
  }

  def expand(): Unit = {
    isExpanded = true
    children.foreach(_.isDisplayed = true)
  }

  def expandAllUnder(): Unit = {
    isExpanded = true
    children.foreach(_.isDisplayed = true)
    children.foreach {
      case b: ProfilingTableBranchNode => b.expandAllUnder()
      case _                           =>
    }
  }

  def collapse(): Unit = {
    isExpanded = false
    children.foreach(_.isDisplayed = false)
  }
}
