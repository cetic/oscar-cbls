package oscar.cbls.visual.profiling

import oscar.cbls.core.search.{CombinatorProfiler, Neighborhood, Profiler}
import oscar.cbls.util.Properties
import oscar.cbls.visual.BetterVisualDrawing
import oscar.visual.{VisualDrawing, VisualTable}
import oscar.visual.shapes.{VisualLine, VisualRectangle, VisualText}

import java.awt.event.{MouseEvent, MouseListener}
import java.awt.{Color, Font}
import javax.swing.SwingUtilities

class ProfilingTree(search: Neighborhood, visualConfig: ProfilingTreeVisualConfig = ProfilingTreeVisualConfig()) extends BetterVisualDrawing(false,false) {


  private val drawing = this

  var allProfilingNodes: List[ProfilingNode] = List.empty

  def drawProfilerBoxes(currentProfiler: Profiler = search.profiler,
                        parentRect: Option[ProfilingNode] = None,
                        depth: Int = 0): ProfilingNode = {
    val isCombinator = currentProfiler match {
      case _: CombinatorProfiler => true
      case _ => false
    }
    val profilingNode = ProfilingNode(currentProfiler, parentRect,this,visualConfig)
    val profilingNodeDrawingHeight = allProfilingNodes.size*(visualConfig.PROFILER_HEIGHT+visualConfig.HEIGHT_BETWEEN_PROFILERS)
    profilingNode.draw(isCombinator, profilingNodeDrawingHeight)
    allProfilingNodes :+= profilingNode

    if (isCombinator) {
      val children = currentProfiler.subProfilers.map(child =>
        drawProfilerBoxes(child, Some(profilingNode), depth + 1))
      profilingNode.addChildren(children)
    }
    profilingNode
  }

  private def displayToolTip(text: String): Unit = this.showToolTip(text)

  def draw(): Unit ={
    drawProfilerBoxes()
    allProfilingNodes.foreach(_.drawLinks(this))
    justifyAllStatisticsToTheRight()
    resize()
  }

  def justifyAllStatisticsToTheRight(): Unit ={
    val rightMostStatMinX = allProfilingNodes.map(_.nodeDisplay.statistics.getBounds._1).max
    allProfilingNodes.foreach(pn => {
      val bounds = pn.nodeDisplay.statistics.getBounds
      pn.nodeDisplay.statistics.translate(rightMostStatMinX - bounds._1, 0)
    })
  }

}

case class ProfilingTreeVisualConfig(
                                      PROFILER_HEIGHT: Int = 40,
                                      HEIGHT_BETWEEN_PROFILERS: Int = 10,
                                      WIDTH_BETWEEN_PROFILERS: Int = 20,
                                      TEXT_PADDING: Int = 5,
                                      linksColor: Color = Color.BLACK,
                                      combinatorColor: Color = new Color(215,230,204),
                                      combinatorTextColor: Color = new Color(123,172,87),
                                      neighborhoodColor: Color = new Color(204,204,204),
                                      neighborhoodTextColor: Color = new Color(150,150,150)){
  def fontColor(isCombinator: Boolean): Color = {
    if(isCombinator)combinatorColor else neighborhoodColor
  }

  def textColor(isCombinator: Boolean): Color = {
    if(isCombinator)combinatorTextColor else neighborhoodTextColor
  }

}


case class ProfilingNodeDisplay(rectangle: VisualRectangle, header: VisualText,
                                statistics: VisualText, depth: Int){

  var _links: List[VisualLine] = List.empty

  def position(): (Int,Int) = (rectangle.x.toInt, rectangle.y.toInt)

  def setLinks(links: List[VisualLine]): Unit = _links = links
}


case class ProfilingNode(profiler: Profiler, parent: Option[ProfilingNode], drawing: VisualDrawing, visualConfig: ProfilingTreeVisualConfig){

  var nodeDisplay: ProfilingNodeDisplay = _
  var children: List[ProfilingNode] = List.empty

  def hasChildren: Boolean = children.nonEmpty

  // LET THE LAZY, the tree is build from leaf to root. So the parent may not already have it's depth.
  lazy val depth: Int = if(parent.nonEmpty)parent.get.depth+1 else 0

  // WARNING use this method only when the tree is fully build
  def draw(isCombinator: Boolean, height: Int): Unit ={
    val x = depth*visualConfig.WIDTH_BETWEEN_PROFILERS
    val y = height
    val name: String = profiler.profiledNeighborhood
    val statistics: String = Properties.justifyLeftArray(profiler.collectThisProfileStatistics).mkString("\n")

    val header = new VisualText(drawing,x,y,name,false)
    header.moveAt(header.getBounds._1+visualConfig.TEXT_PADDING, header.getBounds._3+header.font.getSize+visualConfig.TEXT_PADDING)
    header.setFont(new Font(Font.MONOSPACED, Font.BOLD, header.font.getSize))
    val rectangleWidth = 1.5*(header.fm.stringWidth(name)+2*visualConfig.TEXT_PADDING)

    val rectangle = new VisualRectangle(drawing, x, y, rectangleWidth, visualConfig.PROFILER_HEIGHT)
    rectangle.fill = true
    rectangle.innerCol = visualConfig.fontColor(isCombinator)
    drawing.moveBackward(rectangle)

    val text = new VisualText(drawing,x+rectangleWidth.toInt,y,statistics)
    text.moveAt(text.getBounds._1+visualConfig.TEXT_PADDING, text.getBounds._3+text.font.getSize+visualConfig.TEXT_PADDING)
    text.setFont(new Font(Font.MONOSPACED, Font.BOLD, text.font.getSize))
    text.fontColor = visualConfig.textColor(isCombinator)

    nodeDisplay = ProfilingNodeDisplay(rectangle,header,text,depth)
  }

  def drawLinks(drawing: VisualDrawing): Unit ={
    for(childId <- children.indices){
      val childNodeDisplay = children(childId).nodeDisplay
      val to = childNodeDisplay.rectangle
      // Drawn as if the destination rectangle is set at Y pos 0
      val downwardStroke = VisualLine(drawing,
        nodeDisplay.rectangle.x+visualConfig.WIDTH_BETWEEN_PROFILERS/2, nodeDisplay.rectangle.y+visualConfig.PROFILER_HEIGHT,
        nodeDisplay.rectangle.x+visualConfig.WIDTH_BETWEEN_PROFILERS/2, to.y+to.height/4)
      downwardStroke.borderWidth = 3
      downwardStroke.outerCol = visualConfig.linksColor
      val toDestStroke = VisualLine(drawing, downwardStroke.dest._1,downwardStroke.dest._2,to.x,downwardStroke.dest._2)
      toDestStroke.borderWidth = 3
      toDestStroke.outerCol = visualConfig.linksColor
      childNodeDisplay.setLinks(List(downwardStroke,toDestStroke))
    }
  }

  def addChildren(children: List[ProfilingNode]): Unit ={
    this.children = children
  }

  def moveStatRight(at: Long): Unit ={
    nodeDisplay.statistics.moveAt(
      (at+visualConfig.TEXT_PADDING).toDouble,
      (nodeDisplay.statistics.font.getSize+visualConfig.TEXT_PADDING).toDouble
    )
  }

  def clicked(x: Int, y: Int): Boolean =
    nodeDisplay.rectangle.rect.contains(x,y)
}