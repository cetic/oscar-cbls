package oscar.cbls.visual.profiling

import oscar.cbls.core.search.{CombinatorProfiler, Neighborhood, Profiler}
import oscar.cbls.util.Properties
import oscar.cbls.visual.BetterVisualDrawing
import oscar.visual.{VisualDrawing, VisualTable}
import oscar.visual.shapes.{VisualLine, VisualRectangle, VisualText}

import java.awt.{Color, Font}

class ProfilingTree(search: Neighborhood) extends BetterVisualDrawing(false,false) {

  private val PROFILER_HEIGHT = 40
  private val HEIGHT_BETWEEN_PROFILERS = 10
  private val WIDTH_BETWEEN_PROFILERS = 20
  private val TEXT_PADDING = 5

  // COLORS
  private val linksColor = Color.BLACK
  private val combinatorColor = new Color(215,230,204)
  private val combinatorTextColor = new Color(123,172,87)
  private val neighborhoodColor = new Color(204,204,204)
  private val neighborhoodTextColor = new Color(150,150,150)

  private val drawing = this

  var allProfilingNodes: List[ProfilingNode] = List.empty

  def drawProfilerBoxes(currentProfiler: Profiler = search.profiler,
                        parentRect: Option[ProfilingNode] = None,
                        depth: Int = 0): ProfilingNode = {
    val isCombinator = currentProfiler match {
      case _: CombinatorProfiler => true
      case _ => false
    }
    val profilingNode = ProfilingNode(currentProfiler, parentRect)
    val profilingNodeDrawingHeight = allProfilingNodes.size*(PROFILER_HEIGHT+HEIGHT_BETWEEN_PROFILERS)
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

  case class ProfilingNodeDisplay(rectangle: VisualRectangle, header: VisualText,
                                  statistics: VisualText, depth: Int){

    var _links: List[VisualLine] = List.empty

    def position(): (Int,Int) = (rectangle.x.toInt, rectangle.y.toInt)

    def setLinks(links: List[VisualLine]): Unit = _links = links
  }


  case class ProfilingNode(profiler: Profiler, parent: Option[ProfilingNode]){

    var nodeDisplay: ProfilingNodeDisplay = _
    var children: List[ProfilingNode] = List.empty

    def hasChildren: Boolean = children.nonEmpty

    // LET THE LAZY, the tree is build from leaf to root. So the parent may not already have it's depth.
    lazy val depth: Int = if(parent.nonEmpty)parent.get.depth+1 else 0

    // WARNING use this method only when the tree is fully build
    def draw(isCombinator: Boolean, height: Int): Unit ={
      val x = depth*WIDTH_BETWEEN_PROFILERS
      val y = height
      val name: String = profiler.profiledNeighborhood
      val statistics: String = Properties.justifyLeftArray(profiler.collectThisProfileStatistics).mkString("\n")

      val header = new VisualText(drawing,x,y,name,false)
      header.moveAt(header.getBounds._1+TEXT_PADDING, header.getBounds._3+header.font.getSize+TEXT_PADDING)
      header.setFont(new Font(Font.MONOSPACED, Font.BOLD, header.font.getSize))
      val rectangleWidth = 1.5*(header.fm.stringWidth(name)+2*TEXT_PADDING)

      // Remove the header and insert it after the rectangle
      // So that the header is drawn after the rectangle and thus in front of it
      shapes.remove(shapes.indexOf(header))
      val rectangle = new VisualRectangle(drawing, x, y, rectangleWidth, PROFILER_HEIGHT)
      rectangle.fill = true
      rectangle.innerCol = if(isCombinator)combinatorColor else neighborhoodColor
      shapes.addOne(header)

      val text = new VisualText(drawing,x+rectangleWidth.toInt,y,statistics)
      text.moveAt(text.getBounds._1+TEXT_PADDING, text.getBounds._3+text.font.getSize+TEXT_PADDING)
      text.setFont(new Font(Font.MONOSPACED, Font.BOLD, text.font.getSize))
      text.fontColor = if(isCombinator)combinatorTextColor else neighborhoodTextColor

      nodeDisplay = ProfilingNodeDisplay(rectangle,header,text,depth)
    }

    def drawLinks(drawing: VisualDrawing): Unit ={
      for(childId <- children.indices){
        val childNodeDisplay = children(childId).nodeDisplay
        val to = childNodeDisplay.rectangle
        // Drawn as if the destination rectangle is set at Y pos 0
        val downwardStroke = VisualLine(drawing,
          nodeDisplay.rectangle.x+WIDTH_BETWEEN_PROFILERS/2, nodeDisplay.rectangle.y+PROFILER_HEIGHT,
          nodeDisplay.rectangle.x+WIDTH_BETWEEN_PROFILERS/2, to.y+to.height/4)
        downwardStroke.borderWidth = 3
        downwardStroke.outerCol = linksColor
        val toDestStroke = VisualLine(drawing, downwardStroke.dest._1,downwardStroke.dest._2,to.x,downwardStroke.dest._2)
        toDestStroke.borderWidth = 3
        toDestStroke.outerCol = linksColor
        childNodeDisplay.setLinks(List(downwardStroke,toDestStroke))
      }
    }

    def addChildren(children: List[ProfilingNode]): Unit ={
      this.children = children
    }

    def moveStatRight(at: Long): Unit ={
      nodeDisplay.statistics.moveAt(at+TEXT_PADDING,nodeDisplay.statistics.font.getSize+TEXT_PADDING)
    }
  }

}
