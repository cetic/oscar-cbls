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
  private val PROFILER_WIDTH = 200
  private val HEIGHT_BETWEEN_PROFILERS = 20
  private val WIDTH_BETWEEN_PROFILERS = PROFILER_WIDTH/4
  private val TEXT_PADDING = 2

  // COLORS
  private val linksColor = Color.BLACK
  private val combinatorColor = new Color(215,230,204)
  private val neighborhoodColor = new Color(204,204,204)

  var allProfilingRectangle: Array[VisualRectangle] = Array.empty
  var allProfilingName: Array[VisualText] = Array.empty
  var allProfilingText: Array[VisualText] = Array.empty
  var allLinks: Array[List[VisualShape]] = Array.empty

  def drawProfilerBoxes(currentNeighborhood: Neighborhood = search,
                        parentRect: VisualRectangle = null,
                        count: Int = 0,
                        depth: Int = 0): Int = {
    val isCombinator = currentNeighborhood match{
      case _: NeighborhoodCombinator => true
      case _ => false
    }
    val currentRectangleAndTexts =
      drawTask(if(isCombinator)combinatorColor else neighborhoodColor,count, depth,
        currentNeighborhood.profiler.profiledNeighborhood,
        Properties.justifyLeftArray(currentNeighborhood.profiler.collectThisProfileStatistics).mkString("\n"))
    allProfilingRectangle :+= currentRectangleAndTexts._1
    allProfilingName :+= currentRectangleAndTexts._2
    allProfilingText :+= currentRectangleAndTexts._3
    if(parentRect != null)
      allLinks :+= drawArrow(parentRect,currentRectangleAndTexts._1,linksColor)

    var nextCount = count
    if(isCombinator)
      currentNeighborhood.asInstanceOf[NeighborhoodCombinator].subNeighborhoods.foreach(child => {
          nextCount += 1
          nextCount = drawProfilerBoxes(child, currentRectangleAndTexts._1, nextCount, depth+1)
        })
    nextCount
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
      if (SwingUtilities.isRightMouseButton(e)) {
        scale = scale*(0.9)
      }
      if (SwingUtilities.isLeftMouseButton(e)) {
        if (e.getClickCount == 2) {
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

  private def drawTask(color: Color, row: Int, column: Int, name: String, profilingData: String): (VisualRectangle,VisualText,VisualText) ={
    val x = column*WIDTH_BETWEEN_PROFILERS
    val y = row*(PROFILER_HEIGHT+HEIGHT_BETWEEN_PROFILERS)

    val rectangle = new VisualRectangle(this, x, y, PROFILER_WIDTH, PROFILER_HEIGHT)
    rectangle.fill = true
    rectangle.innerCol = color

    val boxText = new VisualText(this,x,y,name,false)
    boxText.move(boxText.getBounds._1+TEXT_PADDING, boxText.getBounds._3+boxText.font.getSize+TEXT_PADDING)
    boxText.setFont(new Font(Font.MONOSPACED, Font.BOLD, boxText.font.getSize))

    val text = new VisualText(this,x+PROFILER_WIDTH,y,profilingData)
    text.move(text.getBounds._1+TEXT_PADDING, text.getBounds._3+text.font.getSize+TEXT_PADDING)
    text.setFont(new Font(Font.MONOSPACED, Font.BOLD, text.font.getSize))
    (rectangle,boxText,text)
  }

  private def drawArrow(from: VisualRectangle, to: VisualRectangle, color: Color): List[VisualShape] ={
    val downwardStroke = VisualLine(this,from.x+from.width/8, from.y+from.height,from.x+from.width/8, to.y+to.height/4)
    downwardStroke.borderWidth = 3
    downwardStroke.outerCol = color
    val toDestStroke = VisualLine(this,downwardStroke.orig._1,downwardStroke.orig._2,to.x,to.y+to.height/4)
    toDestStroke.borderWidth = 3
    toDestStroke.outerCol = color
    List(downwardStroke, toDestStroke)
  }

  def draw(): Unit ={
    resize()
    drawProfilerBoxes()
  }

}
