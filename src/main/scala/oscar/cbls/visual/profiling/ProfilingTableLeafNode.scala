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

/** Used to represent a profiled Neighborhood that has no children, usually a
  * [[oscar.cbls.core.search.SimpleNeighborhood]]
  *
  * @param profiler
  *   The SearchProfiler whose information are displayed by this node.
  * @param parent
  *   The ProfilingTableNode above this one. Usually the parent represents a
  *   [[oscar.cbls.core.search.NeighborhoodCombinator]] profiler.
  * @param rowPrefix
  *   The prefix is the sequence of characters used to represent the hierarchy of this node.
  */
class ProfilingTableLeafNode(
  override val profiler: SearchProfiler,
  parent: Option[ProfilingTableBranchNode],
  rowPrefix: List[String]
) extends ProfilingTableNode(profiler, parent, rowPrefix) {
  override var isExpanded: Boolean = false

  override def hasChildren: Boolean = false
}
