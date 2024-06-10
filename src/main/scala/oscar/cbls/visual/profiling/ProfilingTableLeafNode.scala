package oscar.cbls.visual.profiling

import oscar.cbls.core.search.profiling.SearchProfiler

case class ProfilingTableLeafNode(
  override val profiler: SearchProfiler,
  parent: Option[ProfilingTableBranchNode],
  rowPrefix: Array[String]
) extends ProfilingTableNode(profiler, parent, rowPrefix) {
  override var isExpanded: Boolean = false

  override def hasChildren: Boolean = false
}
