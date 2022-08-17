package oscar.cbls.visual.profiling

import oscar.cbls.core.search.{Neighborhood, NeighborhoodCombinator, Profiler}
import oscar.cbls.util.Properties
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualLine, VisualRectangle, VisualShape, VisualText}

import java.awt.{Color, Font}
import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import javax.swing.{JScrollBar, SwingUtilities}

class ProfilingTree(search: Neighborhood) extends VisualDrawing(false,false) {

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

  def drawProfilerBoxes(currentNeighborhood: Neighborhood = search,
                        parentRect: Option[ProfilingNode] = None,
                        depth: Int = 0): ProfilingNode = {
    val isCombinator = currentNeighborhood match {
      case _: NeighborhoodCombinator => true
      case _ => false
    }
    val profilingNode = ProfilingNode(currentNeighborhood, parentRect)
    profilingNode.draw(isCombinator)
    profilingNode.setVisible(parentRect.isEmpty)
    allProfilingNodes :+= profilingNode

    if (isCombinator) {
      val children = currentNeighborhood.asInstanceOf[NeighborhoodCombinator].subNeighborhoods.map(child =>
        drawProfilerBoxes(child, Some(profilingNode), depth + 1))
      profilingNode.addChildren(children)
    }
    profilingNode
  }

  def resize(): Unit ={
    val bounds = this.findBounds(this.shapes)
    this.setPreferredSize(new java.awt.Dimension((scale*bounds._2).toInt,(scale*bounds._4).toInt))
    this.repaint()
    this.revalidate()
  }

  this.getMouseListeners.foreach(this.removeMouseListener)
  this.addMouseListener(new MouseListener() {
    override def mouseClicked(e: MouseEvent): Unit = {
      val clickedProfilingNodes = allProfilingNodes.filter(_.contains(e.getX, e.getY))
      if (SwingUtilities.isRightMouseButton(e)) {
        scale = scale*(0.9)
      }
      if (SwingUtilities.isLeftMouseButton(e)) {
        if (e.getClickCount == 2) {
          if(clickedProfilingNodes.nonEmpty)
            clickedProfilingNodes.foreach(pn => if(pn.isCollapsed) pn.recursiveExpand() else pn.collapse())
          else
            scale = scale*(1.1)
        }
      }
      resize()
    }
    override def mouseEntered(e: MouseEvent): Unit = {}
    override def mousePressed(e: MouseEvent): Unit = {}
    override def mouseExited(e: MouseEvent): Unit = {}
    override def mouseReleased(e: MouseEvent): Unit = {}
  })

  this.addMouseMotionListener {
    new MouseMotionListener() {
      override def mouseMoved(e: MouseEvent): Unit = {
        val targetedProfilingNodes = allProfilingNodes.filter(_.contains(e.getX, e.getY))
        if(targetedProfilingNodes.nonEmpty) {
          displayToolTip(s"${targetedProfilingNodes.head.toString}")
        }
      }
      override def mouseDragged(e: MouseEvent): Unit = {}
    }
  }

  private def displayToolTip(text: String): Unit = this.showToolTip(text)

  def draw(): Unit ={
    resize()
    drawProfilerBoxes()
    allProfilingNodes.foreach(_.drawLinks(this))
    allProfilingNodes.foreach(_.moveStatRight(shapes.filter(_.isInstanceOf[VisualRectangle]).map(_.getBounds._2).max))
    allProfilingNodes.find(_.parent.isEmpty).get.recursiveExpand()
  }

  case class ProfilingNodeDisplay(rectangle: VisualRectangle, header: VisualText,
                                  statistics: VisualText, depth: Int){

    private val headerTxt = header.text
    private val statisticsTxt = statistics.text
    private var _links: List[VisualLine] = List.empty

    def setVisible(visible: Boolean): Unit ={
      rectangle.visible = visible
      _links.foreach(_.visible = visible)
      header.text = if(visible) headerTxt else ""
      statistics.text = if(visible) statisticsTxt else ""
    }

    def position(): (Int,Int) = (rectangle.x.toInt, rectangle.y.toInt)
    def moveDownBy(nbRow: Int, parentRow: Int): Unit ={
      val moveDownBy = (PROFILER_HEIGHT+HEIGHT_BETWEEN_PROFILERS)*nbRow
      val parentYPos = (PROFILER_HEIGHT+HEIGHT_BETWEEN_PROFILERS)*parentRow

      // Moving artifacts
      rectangle.move(rectangle.x, rectangle.y+moveDownBy)
      header.move(header.getBounds._1, header.getBounds._3+moveDownBy)
      statistics.move(statistics.getBounds._1, statistics.getBounds._3+moveDownBy)
      _links.foreach(l => l.move(0,moveDownBy))
      // Extending (or retracting) vertical VisualLine
      _links.head.orig = (_links.head.orig._1,parentYPos+PROFILER_HEIGHT)
    }

    def setLinks(links: List[VisualLine]): Unit = _links = links
  }


  case class ProfilingNode(value: Neighborhood, parent: Option[ProfilingNode]){

    private var _visible = false
    private var _row = 0
    private var _expanded = false

    private var nodeDisplay: ProfilingNodeDisplay = _
    private var children: List[ProfilingNode] = List.empty

    def hasChildren: Boolean = children.nonEmpty

    // LET THE LAZY, the tree is build from leaf to root. So the parent may not already have it's depth.
    lazy val depth: Int = if(parent.nonEmpty)parent.get.depth+1 else 0

    def contains(x: Int, y: Int): Boolean ={
      if(_visible){
        val rectBounds = nodeDisplay.rectangle.getBounds
        return x <= rectBounds._2 && x >= rectBounds._1 && y <= rectBounds._4 && y >= rectBounds._3
      }
      false
    }

    // WARNING use this method only when the tree is fully build
    def draw(isCombinator: Boolean): Unit ={
      val x = depth*WIDTH_BETWEEN_PROFILERS
      val y = 0
      val name: String = value.profiler.profiledNeighborhood
      val statistics: String = Properties.justifyLeftArray(value.profiler.collectThisProfileStatistics).mkString("\n")

      val header = new VisualText(drawing,x,y,name,false)
      header.move(header.getBounds._1+TEXT_PADDING, header.getBounds._3+header.font.getSize+TEXT_PADDING)
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
      text.move(text.getBounds._1+TEXT_PADDING, text.getBounds._3+text.font.getSize+TEXT_PADDING)
      text.setFont(new Font(Font.MONOSPACED, Font.BOLD, text.font.getSize))
      text.fontColor = if(isCombinator)combinatorTextColor else neighborhoodTextColor

      nodeDisplay = ProfilingNodeDisplay(rectangle,header,text,depth)
      nodeDisplay.setVisible(_visible)
    }

    def drawLinks(drawing: VisualDrawing): Unit ={
      for(childId <- children.indices){
        val childNodeDisplay = children(childId).nodeDisplay
        val to = childNodeDisplay.rectangle
        // Drawn as if the destination rectangle is set at Y pos 0
        val downwardStroke = VisualLine(drawing,
          depth*WIDTH_BETWEEN_PROFILERS+WIDTH_BETWEEN_PROFILERS/2, -(childId*PROFILER_HEIGHT + (childId+1)*HEIGHT_BETWEEN_PROFILERS),
          depth*WIDTH_BETWEEN_PROFILERS+WIDTH_BETWEEN_PROFILERS/2, to.height/4)
        downwardStroke.borderWidth = 3
        downwardStroke.outerCol = linksColor
        downwardStroke.visible = false
        val toDestStroke = VisualLine(drawing, downwardStroke.dest._1,downwardStroke.dest._2,to.x,downwardStroke.dest._2)
        toDestStroke.borderWidth = 3
        toDestStroke.outerCol = linksColor
        toDestStroke.visible = false
        childNodeDisplay.setLinks(List(downwardStroke,toDestStroke))
      }
    }

    def addChildren(children: List[ProfilingNode]): Unit ={
      this.children = children
    }

    def setVisible(visible: Boolean): Unit ={
      _visible = visible
      nodeDisplay.setVisible(visible)
    }

    def row: Int = _row

    def isVisible: Boolean = _visible

    def isExpanded: Boolean = _expanded

    def isCollapsed: Boolean = !_expanded

    def expand(): Unit ={
      allProfilingNodes.filter(pn => pn.isVisible && pn.row > _row).foreach(pn => pn.moveDownBy(children.length,if(pn.parent.isDefined)pn.parent.get._row else 0))
      children.indices.foreach(
        childId => {
          children(childId).setVisible(true)
          children(childId).moveAtRow(_row+childId+1, _row)
        }
      )
      _expanded = true
    }

    def recursiveExpand(): Unit ={
      expand()
      children.foreach(_.recursiveExpand())
    }

    def collapse(): Unit ={
      children.filter(_.isExpanded).foreach(_.collapse())
      children.foreach(_.setVisible(false))
      allProfilingNodes.filter(pn => pn.isVisible && pn.row > _row).foreach(pn => pn.moveDownBy(-children.length,if(pn.parent.isDefined)pn.parent.get._row else 0))
      _expanded = false
    }

    def moveDownBy(nbRow: Int, parentRow: Int): Unit ={
      _row += nbRow
      nodeDisplay.moveDownBy(nbRow, parentRow)
    }

    def moveAtRow(row: Int, parentRow: Int): Unit ={
      nodeDisplay.moveDownBy(row - _row, parentRow)
      _row = row
    }

    def moveStatRight(at: Long): Unit ={
      nodeDisplay.statistics.move(at+TEXT_PADDING,nodeDisplay.statistics.font.getSize+TEXT_PADDING)
    }

    override def toString: String = nodeDisplay.header.text
  }

}
