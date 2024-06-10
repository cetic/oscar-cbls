package oscar.cbls.visual.profiling

import oscar.cbls.core.search.profiling.SearchProfiler

case class ProfilingTableBranchNode(
  override val profiler: SearchProfiler,
  parent: Option[ProfilingTableBranchNode],
  rowPrefix: Array[String]
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
