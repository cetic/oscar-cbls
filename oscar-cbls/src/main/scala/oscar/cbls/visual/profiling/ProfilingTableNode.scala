package oscar.cbls.visual.profiling

import oscar.cbls.core.search.profiling.{CombinatorProfiler, Profiler}

object ProfilingTableNode{

  private val childChar = Character.toString(9568)
  private val lastChildChar = Character.toString(9562)
  private val verticalLineChar = Character.toString(9553)
  private val horizontalLineChar = Character.toString(9552)

  def apply(profiler: Profiler): Array[ProfilingTableNode] ={
    val root = ProfilingTableNode(profiler,None,Array.empty)
    def treeToListDFS(node: ProfilingTableNode): List[ProfilingTableNode] ={
      node match {
        case b: ProfilingTableBranchNode =>
          List(node) ::: b.children.flatMap(treeToListDFS)
        case _: ProfilingTableLeafNode => List(node)
      }
    }
    treeToListDFS(root).toArray
  }

  def apply(profiler: Profiler, parent: Option[ProfilingTableBranchNode], rowPrefix: Array[String]): ProfilingTableNode ={
    val profilingNode = profiler match {
      case _: CombinatorProfiler =>
        val profilingNode = ProfilingTableBranchNode(profiler, parent, rowPrefix)
        val children = profiler.subProfilers.map(child =>{
          val charsToAdd: String = if(profiler.subProfilers.last == child) lastChildChar+horizontalLineChar else childChar+horizontalLineChar
          val newPrefix =
            if(rowPrefix.isEmpty)Array(charsToAdd)
            else if(rowPrefix.last == lastChildChar+horizontalLineChar) rowPrefix.dropRight(1) ++ Array("  ",charsToAdd)
            else rowPrefix.dropRight(1) ++ Array(verticalLineChar+" ",charsToAdd)
          ProfilingTableNode(child, Some(profilingNode), newPrefix)
        })
        profilingNode.addChildren(children)
        profilingNode
      case _ =>
        ProfilingTableLeafNode(profiler, parent, rowPrefix)
    }
    profilingNode
  }
}

abstract class ProfilingTableNode(val profiler: Profiler, parent: Option[ProfilingTableBranchNode], rowPrefix: Array[String]){

  val depth: Int = if(parent.nonEmpty)parent.get.depth+1 else 0
  var isDisplayed: Boolean = parent.isEmpty

  val profilerCommonData: Array[String] = profiler.collectThisProfileData

  var isExpanded: Boolean
  protected def profilerAndCombinatorStatistics: Array[String] = Array(profilerCommonData.head)
  protected val commonStatistics: Array[String] = profilerCommonData.tail
  def data(): Array[String] =
    Array((rowPrefix++profilerAndCombinatorStatistics).mkString(""))++commonStatistics

  def hasChildren: Boolean
}

case class ProfilingTableBranchNode(override val profiler: Profiler, parent: Option[ProfilingTableBranchNode], rowPrefix: Array[String]) extends ProfilingTableNode(profiler, parent, rowPrefix){

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
    println("expanding all")
    children.foreach {
      case b: ProfilingTableBranchNode => b.expandAllUnder()
      case _ =>
    }
  }

  def collapse(): Unit ={
    isExpanded = false
    children.foreach(_.isDisplayed = false)
  }
}

case class ProfilingTableLeafNode(override val profiler: Profiler, parent: Option[ProfilingTableBranchNode], rowPrefix: Array[String])
  extends ProfilingTableNode(profiler, parent, rowPrefix) {
  override var isExpanded: Boolean = false

  override def hasChildren: Boolean = false
}