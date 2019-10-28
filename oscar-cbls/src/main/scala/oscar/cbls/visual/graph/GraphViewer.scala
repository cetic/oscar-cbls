package oscar.cbls.visual.graph

import java.awt.Color
import java.awt.geom.Line2D.Double
import java.awt.geom.Rectangle2D

import oscar.cbls
import oscar.cbls.algo.graph._
import oscar.visual.VisualDrawing
import oscar.visual.shapes._

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.swing.Color

/*
class InteractiveGraphViewer(graph:ConditionalGraphWithIntegerNodeCoordinates,
                             centroidColor:SortedMap[Int,Color],
                             colorForUnreachableNodes:Color  = Color.black,
                             colorForPermanentEdges:Color =Color.black,
                             colorForOpenEdges:Color = Color.green,
                             colorForClosedEdges:Color = Color.red,
                             colorForEmphasizedEdges:Color = Color.blue,
                             colorForEmphasizedEdges2:Color = Color.orange)
  extends JFrame(){

  val graphDrawing = new GraphViewer(graph,
    centroidColor,
    colorForUnreachableNodes,
    colorForPermanentEdges,
    colorForOpenEdges,
    colorForClosedEdges,
    colorForEmphasizedEdges,
    colorForEmphasizedEdges2)

  val toolbar = new VisualToolBar()
  toolbar.setFloatable(false)
  toolbar.addButton("toggleClosedEdges",toggleClosedEdges)
  toolbar.addButton("toggleRegularEdges",toggleRegularEdges)
  toolbar.addButton("toggleOpenEdges",toggleOpenEdges)
  this.add(toolbar, BorderLayout.NORTH)
  this.add(graphDrawing)

  graphDrawing.setPreferredSize(new java.awt.Dimension(1000,1000))
  this.pack()

  def toggleClosedEdges(): Unit ={
    hideClosedEdges = !hideClosedEdges
  }

  def toggleRegularEdges(): Unit ={
    hideRegularEdges = !hideRegularEdges
  }
  def toggleOpenEdges(): Unit ={
    hideOpenEdges = !hideOpenEdges
  }

  var hideClosedEdges:Boolean = false
  var hideRegularEdges:Boolean = false
  var hideOpenEdges:Boolean=false
  var openConditions:SortedSet[Long] = SortedSet.empty
  var centroids:SortedSet[Long] = SortedSet.empty
  var nodeToCentroid:SortedMap[Long,Long] = SortedMap.empty
  var emphasizeEdges:Iterable[Edge] = None
  var extraPath:Iterable[RevisableDistance] = None

  def updateAndRedraw(openConditions:SortedSet[Long],
                      centroids:SortedSet[Long],
                      nodeToCentroid:SortedMap[Long,Long],
                      emphasizeEdges:Iterable[Edge] = List.empty,
                      extraPath:Iterable[RevisableDistance]): Unit ={
    this.openConditions = openConditions
    this.centroids =centroids
    this.nodeToCentroid = nodeToCentroid
    this.emphasizeEdges = emphasizeEdges
    this.extraPath =extraPath
    redraw()
  }

  private def redraw(): Unit ={
    graphDrawing.redraw(
      openConditions = openConditions,
      centroids = centroids,
      nodeToCentroid = nodeToCentroid,
      hideClosedEdges = hideClosedEdges,
      hideRegularEdges,
      hideOpenEdges,
      emphasizeEdges,
      extraPath)
  }
}

*/
class GraphViewer(graph:ConditionalGraphWithIntegerNodeCoordinates,
                  centroidColor:SortedMap[Int,Color],
                  colorForUnreachableNodes:Color  = Color.black,
                  colorForPermanentEdges:Color =Color.black,
                  colorForOpenEdges:Color = Color.green,
                  colorForClosedEdges:Color = Color.red,
                  colorForEmphasizedEdges:Color = Color.blue,
                  colorForEmphasizedEdges2:Color = Color.orange)
  extends VisualDrawing(false,false){

  this.setDoubleBuffered(true) //does not work.

  val maxX = graph.coordinates.toList.map(_._1).max
  val maxY = graph.coordinates.toList.map(_._2).max

  var xMultiplier = this.getWidth.toDouble / maxX.toDouble
  var yMultiplier = this.getHeight.toDouble / maxY.toDouble

  override def addShape(shape: VisualShape, repaintAfter: Boolean = true){
    super.addShape(shape,false)
  }

  def redraw(openConditions:SortedSet[Long],
             centroids:SortedSet[Long],
             nodeToCentroid:SortedMap[Long,Long],
             hideClosedEdges:Boolean = false,
             hideRegularEdges:Boolean = false,
             hideOpenEdges:Boolean=false,
             emphasizeEdges:Iterable[Edge] = List.empty,
             extraPath:Iterable[RevisableDistance]) {

    super.clear(false)

    xMultiplier = this.getWidth.toDouble / maxX.toDouble
    yMultiplier = this.getHeight.toDouble / maxY.toDouble

    for(path <- extraPath){
      drawPath(path,emphNodes = true:Boolean,emphEdges=true)
    }

    drawEdges(openConditions:SortedSet[Long],hideClosedEdges,hideRegularEdges, hideOpenEdges,emphasizeEdges)

    drawNodes(centroids:SortedSet[Long],
      nodeToCentroid:SortedMap[Long,Long])

    //double buffering still does not work!
    super.repaint()
  }

  def drawNodes(centroids:SortedSet[Long],
                nodeToCentroid:SortedMap[Long,Long]): Unit ={

    for(nodeId <- graph.nodeRange){
      if(centroids contains nodeId){
        //this is a centroid
        drawNode(nodeId,centroidColor(nodeId),true,graph.nodes(nodeId).transitAllowed)
      }else{
        //this is not a centroid, check for a marked node
        nodeToCentroid.get(nodeId) match{
          case Some(centroidID) =>
            //a folowed node
            if(centroidID == -1){
              //not reacheable
              drawNode(nodeId,colorForUnreachableNodes,false,graph.nodes(nodeId).transitAllowed)
            }else{
              //reachable by centroidID
              drawNode(nodeId,centroidColor(cbls.longToInt(centroidID)),false,graph.nodes(nodeId).transitAllowed)
            }
          case None =>
          //not a followed node, set default black color, small dot
          //drawNode(node:NodeWithIntegerCoordinates,colorForUnreachableNodes,false)
        }
      }
    }
  }

  def drawNode(nodeId:Int,color:Color,isCentroid:Boolean,isTransitAllowed:Boolean) = {
    val nodeCoordinates = graph.coordinates(nodeId)
    if(isCentroid){
      //rectangle
      val side = 12
      val tempPoint = new VisualRectangle(this, new Rectangle2D.Double(
        nodeCoordinates._1 * xMultiplier - side/2,
        nodeCoordinates._2 * yMultiplier - side/2,
        side,
        side))
      tempPoint.innerCol_$eq(color)
      tempPoint.toolTip = "Centroid" + "(id:" + nodeId + " transit:"  +isTransitAllowed + ")"
    }else {
      if(isTransitAllowed) {
        //circle
        val radius = 3
        val tempPoint = new VisualCircle(this,
          nodeCoordinates._1 * xMultiplier,
          nodeCoordinates._2 * yMultiplier,
          radius)

        tempPoint.innerCol_$eq(color)
        tempPoint.toolTip = "Node" + "(id:" + nodeId + " transit:true)"

      }else {
        //cross
        val radius = 3
        val lineV = new VisualLine(this, new Double(
          nodeCoordinates._1 * xMultiplier,
          nodeCoordinates._2 * yMultiplier + radius,
          nodeCoordinates._1 * xMultiplier,
          nodeCoordinates._2 * yMultiplier - radius))
        val lineH = new VisualLine(this, new Double(
          nodeCoordinates._1 * xMultiplier + radius,
          nodeCoordinates._2 * yMultiplier,
          nodeCoordinates._1 * xMultiplier - radius,
          nodeCoordinates._2 * yMultiplier))

        lineV.dashed = false
        lineV.borderWidth = 3
        lineV.outerCol = color
        lineH.dashed = false
        lineH.borderWidth = 3
        lineH.outerCol = color

        lineV.toolTip = "Node" + "(id:" + nodeId + " transit:false)"
        lineH.toolTip = "Node" + "(id:" + nodeId + " transit:false)"

        val tempPoint = new VisualCircle(this,
          nodeCoordinates._1 * xMultiplier,
          nodeCoordinates._2 * yMultiplier,
          radius)
        tempPoint.border = false
        tempPoint.innerCol_$eq(color)
        tempPoint.toolTip = "Node" + "(id:" + nodeId + " transit:false)"

      }
    }
  }

  def emphNode(nodeCoordinates:(Int,Int),
               color:Color) = {
    //circle
    val radius = 6
    val tempPoint = new VisualCircle(this,
      nodeCoordinates._1 * xMultiplier,
      nodeCoordinates._2 * yMultiplier,
      radius)
    tempPoint.innerCol_$eq(color)
  }

  def drawEdges(openConditions:SortedSet[Long],
                hideClosedEdges:Boolean,
                hideRegularEdges:Boolean = false,
                hideOpenEdges:Boolean=false,
                emphasizeEdges:Iterable[Edge]): Unit = {
    for (edge <- graph.edges){
      edge.conditionID match{
        case None => //permanent edge
          if(!hideRegularEdges) drawEdge(edge:Edge,0)
        case Some(conditionID) =>
          if (openConditions contains conditionID){
            //open edge
            if(!hideOpenEdges) drawEdge(edge:Edge,1)
          }else{
            //closed edge
            if(!hideClosedEdges) drawEdge(edge:Edge,2)
          }
      }
    }
    for(edge <- emphasizeEdges){
      drawEdge(edge:Edge,3)
    }
  }

  def drawEdge(edge:Edge,style:Int): Unit ={
    val fromCoord = graph.coordinates(edge.nodeIDA)
    val toCoord = graph.coordinates(edge.nodeB.id)

    val line = new VisualLine(this, new Double(
      fromCoord._1 * xMultiplier,
      fromCoord._2 * yMultiplier,
      toCoord._1 * xMultiplier,
      toCoord._2 * yMultiplier))

    style match{
      case 0 => //permanent edge
        line.dashed = true
        line.outerCol = colorForPermanentEdges
      case 1 => //open edge
        line.dashed = false
        line.borderWidth = 5
        line.outerCol = colorForOpenEdges
      case 2 => //closed edge
        line.dashed = true
        line.borderWidth = 1
        line.outerCol = colorForClosedEdges
      case 3 => //emphasized
        line.dashed = false
        line.borderWidth = 1
        line.outerCol = colorForEmphasizedEdges
      case 4 => //emphasized
        line.dashed = false
        line.borderWidth = 5
        line.outerCol = colorForEmphasizedEdges2
      case 5 => //emphasized
        line.dashed = false
        line.borderWidth = 5
        line.outerCol = Color.pink
    }
  }


  def drawPath(d:RevisableDistance,emphNodes:Boolean,emphEdges:Boolean): Unit ={
    d match {
      case Distance(
      from: Node,
      to: Node,
      distance: Long,
      requiredConditions: SortedSet[Int],
      unlockingConditions: SortedSet[Int],
      path: Option[List[Edge]]) =>

        if(emphNodes){
          emphNode(graph.coordinates(from.id),
            Color.orange)
          emphNode(graph.coordinates(to.id),
            Color.orange)
        }

        if(emphEdges && path.isDefined) {
          for (edge <- path.get) {
            drawEdge(edge: Edge, 4)
          }

          for(closedEdge <-unlockingConditions.map(graph.conditionToConditionalEdges)){
            drawEdge(closedEdge: Edge, 5)
          }
        }
      case NeverConnected(
      from: Node,
      to: Node) =>
        if(emphNodes){
          emphNode(graph.coordinates(from.id),
            Color.orange)
          emphNode(graph.coordinates(to.id),
            Color.orange)
        }
      case NotConnected(
      from: Node,
      to: Node,
      unlockingConditions: SortedSet[Int]) =>
        if(emphNodes){
          emphNode(graph.coordinates(from.id),
            Color.orange)
          emphNode(graph.coordinates(to.id),
            Color.orange)
        }

    }
  }
}
