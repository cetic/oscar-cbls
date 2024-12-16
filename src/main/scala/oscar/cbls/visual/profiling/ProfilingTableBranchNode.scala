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

/** Used to represent a profiled Neighborhood that has children, usually a
  * [[oscar.cbls.core.search.Neighborhood]].
  *
  * @param profiler
  *   The SearchProfiler whose information are displayed by this node.
  * @param parent
  *   The ProfilingTableNode above this one. Usually the parent represents a
  *   [[oscar.cbls.core.search.NeighborhoodCombinator]] profiler.
  * @param rowPrefix
  *   The prefix is the sequence of characters used to represent the hierarchy of this node.
  */
class ProfilingTableBranchNode(
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

  /** Expands a [[oscar.cbls.core.search.profiling.CombinatorProfiler]]'s node to reveal its
    * children.
    *
    * Used for graphical display. Just tagging this node as "expanded" and the children as
    * "displayed". The graphical modification has to be made by the visualization code.
    */
  def expand(): Unit = {
    isExpanded = true
    children.foreach(_.isDisplayed = true)
  }

  /** Recursively expands a [[oscar.cbls.core.search.profiling.CombinatorProfiler]]'s node to reveal
    * its children.
    *
    * Used for graphical display. Just tagging this node as "expanded" and all the children as
    * "displayed". The graphical modification has to be made by the visualization code.
    */
  def expandAllUnder(): Unit = {
    isExpanded = true
    children.foreach(_.isDisplayed = true)
    children.foreach {
      case b: ProfilingTableBranchNode => b.expandAllUnder()
      case _                           =>
    }
  }

  /** Collapses this [[oscar.cbls.core.search.profiling.CombinatorProfiler]]'s node to hide all its
    * children.
    *
    * Used for graphical display. Just tagging this node as "not expanded" and all the children as
    * "not displayed". The graphical modification has to be made by the visualization code.
    */
  def collapse(): Unit = {
    isExpanded = false
    children.foreach(_.isDisplayed = false)
  }
}
