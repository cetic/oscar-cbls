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

import oscar.cbls.core.search.profiling.{CombinatorProfiler, SearchProfiler}

/** Build the Profiling table node hierarchy 
  *
  * ==Note :==
  * This object creates an hierarchy used both by graphical and "on-console" display. It may need
  * some modification
  */
object ProfilingTableNode {

  // Character used to draw a hierarchy graph on the left of the display
  private val childChar          = Character.toString(9568)
  private val lastChildChar      = Character.toString(9562)
  private val verticalLineChar   = Character.toString(9553)
  private val horizontalLineChar = Character.toString(9552)

  /** Initiates the hierarchy build.
    *
    * @param profiler
    *   The root profiler
    * @return
    *   An ordered list of ProfilingTableNode.
    */
  def apply(profiler: SearchProfiler): List[ProfilingTableNode] = {
    val root = ProfilingTableNode(profiler, None, List.empty)
    treeToListDFS(root)
  }

  private def treeToListDFS(node: ProfilingTableNode): List[ProfilingTableNode] = {
    node match {
      case b: ProfilingTableBranchNode =>
        List(node) ::: b.children.flatMap(treeToListDFS)
      case _: ProfilingTableLeafNode => List(node)
    }
  }

  private def apply(
    profiler: SearchProfiler,
    parent: Option[ProfilingTableBranchNode],
    rowPrefix: List[String]
  ): ProfilingTableNode = {
    profiler match {
      case _: CombinatorProfiler =>
        val profilingNode = ProfilingTableBranchNode(profiler, parent, rowPrefix)
        val children = profiler.subProfilers.map(child => {
          val charsToAdd: String =
            if (profiler.subProfilers.last == child) lastChildChar + horizontalLineChar
            else childChar + horizontalLineChar
          val newPrefix =
            if (rowPrefix.isEmpty) List(charsToAdd)
            else if (rowPrefix.last == lastChildChar + horizontalLineChar)
              rowPrefix.dropRight(1) ++ List("  ", charsToAdd)
            else rowPrefix.dropRight(1) ++ List(verticalLineChar + " ", charsToAdd)
          ProfilingTableNode(child, Some(profilingNode), newPrefix)
        })
        profilingNode.addChildren(children)
        profilingNode
      case _ =>
        ProfilingTableLeafNode(profiler, parent, rowPrefix)
    }
  }
}

/** Interface that provides methods to ease the display of the profiling.
  *
  * Each node represent a different SearchProfiler. It has a link to it's parent, and has direct
  * access to the profiler. Some methods are meant for graphical display.
  *
  * @param profiler
  *   The SearchProfiler whose information are displayed by this node.
  * @param parent
  *   The ProfilingTableNode above this one. Usually the parent represents a
  *   [[oscar.cbls.core.search.NeighborhoodCombinator]] profiler.
  * @param rowPrefix
  *   The prefix is the sequence of characters used to represent the hierarchy of this node.
  */
abstract class ProfilingTableNode(
  val profiler: SearchProfiler,
  parent: Option[ProfilingTableBranchNode],
  rowPrefix: List[String]
) {

  val depth: Int = if (parent.nonEmpty) parent.get.depth + 1 else 0
  // Whether or not this node should be displayed (graphical display context)
  var isDisplayed: Boolean = parent.isEmpty

  private val profilerCommonData: List[String] = profiler.collectThisProfileData

  // Whether or not this node is expanded (graphical display context)
  var isExpanded: Boolean
  private def profilerAndCombinatorStatistics: Array[String] = Array(profilerCommonData.head)
  private val commonStatistics: List[String]                 = profilerCommonData.tail
  def data(): List[String] =
    List((rowPrefix ++ profilerAndCombinatorStatistics).mkString("")) ++ commonStatistics

  def hasChildren: Boolean
}
