package oscar.cbls.visual.graph

import java.awt.Color
import java.awt.geom.Line2D.Double
import java.awt.geom.Rectangle2D

import oscar.cbls.algo.graph._
import oscar.visual.VisualDrawing
import oscar.visual.shapes._

import scala.collection.immutable.{SortedMap, SortedSet}

class GraphViewer(graph:ConditionalGraphWithIntegerNodeCoordinates,
                  centroidColor:SortedMap[Int,Color],
                  colorForUnreachableNodes:Color  = Color.black,
                  colorForPermanentEdges:Color =Color.black,
                  colorForOpenEdges:Color = Color.green,
                  colorForClosedEdges:Color = Color.red,
                  colorForEmphasizedEdges:Color = Color.blue,
                  colorForEmphasizedEdges2:Color = Color.orange,
                  nbNodesPerNode : Int = 1)
  extends VisualDrawing(false,false){

  this.setDoubleBuffered(true) //does not work.

  val maxX = graph.coordinates.toList.map(_._1).max
  val maxY = graph.coordinates.toList.map(_._2).max

  var xMultiplier = this.getWidth.toDouble / maxX.toDouble
  var yMultiplier = this.getHeight.toDouble / maxY.toDouble

  val rectangleSide = 12


  val edgesShape : Array[VisualLine] = graph.edges.map(e => {
    val coordA = graph.coordinates(e.nodeA.id)
    val coordB = graph.coordinates(e.nodeB.id)
    val line = new VisualLine(this,new Double(
      coordA._1 * xMultiplier,
      coordA._2 * yMultiplier,
      coordB._1 * xMultiplier,
      coordB._2 * yMultiplier
    ))

    line.dashed = true
    line.outerCol = colorForPermanentEdges
    line.visible_=(false)
    line
  })

  val rectangleShapes : Array[VisualShape] = graph.coordinates.map(coord => {
    val side = 12
    val rectangle = new VisualRectangle(this,new Rectangle2D.Double(
      coord._1 * xMultiplier - side/2,
      coord._2 * yMultiplier - side/2,
      side,
      side))
    rectangle.visible_=(false)
    rectangle
  })

  val circleShape : Array[Array[VisualShape]] = graph.coordinates.map(coord => {
    val baseRadius = 4
    (0 until nbNodesPerNode).map(id => {
      val radius = baseRadius + 4 * (nbNodesPerNode - id - 1)
      val circle : VisualShape = new VisualCircle(this,coord._1 * xMultiplier,coord._2 * yMultiplier,radius)
      circle.innerCol_=(Color.RED)
      circle.visible_=(false)
      circle
    }).toArray.reverse
  })



  def resize(): Unit = {
    xMultiplier = this.getWidth.toDouble / maxX.toDouble
    yMultiplier = this.getHeight.toDouble / maxY.toDouble

    println(this.getWidth.toDouble)
    println(this.getHeight.toDouble)
    val side = rectangleSide
    val nbNodes = rectangleShapes.length
    (0 until nbNodes).foreach(id => {
      val coord = graph.coordinates(id)
      rectangleShapes(id).move(coord._1 * xMultiplier - side / 2,coord._2 * yMultiplier - side / 2)
      circleShape(id).foreach(_.move(coord._1 * xMultiplier,coord._2 * yMultiplier))
    })
    graph.edges.indices.foreach(id => {
      val coordA = graph.coordinates(graph.edges(id).nodeA.id)
      val coordB = graph.coordinates(graph.edges(id).nodeB.id)
      edgesShape(id).orig_=(coordA._1 * xMultiplier,coordA._2 * yMultiplier)
      edgesShape(id).dest_=(coordB._1 * xMultiplier,coordB._2 * yMultiplier)
    })
  }


  override def addShape(shape: VisualShape,
                        repaintAfter: Boolean = true): Unit = {
    super.addShape(shape,false)
  }

  def redrawMultipleNodes(openConditions:SortedSet[Int],
                          centroids:SortedSet[Int],
                          nodeToCentroids:SortedMap[Int,Array[Int]],
                          k : Int,
                          hideClosedEdges:Boolean = false,
                          hideRegularEdges:Boolean = false,
                          hideOpenEdges:Boolean=false,
                          extraCentroids:Array[Int] = Array.empty,
                          emphasizeEdges:Iterable[Edge] = List.empty,
                          extraPath:Iterable[RevisableDistance]): Unit ={

    //super.clear(false)

    xMultiplier = this.getWidth.toDouble / maxX.toDouble
    yMultiplier = this.getHeight.toDouble / maxY.toDouble

    for(path <- extraPath){
      drawPath(path,emphNodes = true:Boolean,emphEdges=true)
    }

    drawEdges(openConditions:SortedSet[Int],hideClosedEdges,hideRegularEdges, hideOpenEdges,emphasizeEdges)


    drawNodes(centroids,nodeToCentroids,extraCentroids)
  }

  def redraw(openConditions:SortedSet[Int],
             centroids:SortedSet[Int],
             nodeToCentroid:SortedMap[Int,Int],
             hideClosedEdges:Boolean = false,
             hideRegularEdges:Boolean = false,
             hideOpenEdges:Boolean=false,
             extraCentroids:Array[Int]=Array.empty,
             emphasizeEdges:Iterable[Edge] = List.empty,
             extraPath:Iterable[RevisableDistance]): Unit ={

    //super.clear(false)

    xMultiplier = this.getWidth.toDouble / maxX.toDouble
    yMultiplier = this.getHeight.toDouble / maxY.toDouble

    for(path <- extraPath){
      drawPath(path,emphNodes = true:Boolean,emphEdges=true)
    }

    drawEdges(openConditions,hideClosedEdges,hideRegularEdges, hideOpenEdges,emphasizeEdges)

    drawNodes(centroids,
      nodeToCentroid.map {tup => tup._1 -> Array(tup._2)} ,extraCentroids)

    //double buffering still does not work!
    super.repaint()
  }

  def drawNodes(centroids:SortedSet[Int],
                nodeToCentroid:SortedMap[Int,Array[Int]],
                extraCentroids:Array[Int],
                radius : Int = 3): Unit ={

    for(nodeId <- graph.nodeRange){
      if(centroids contains nodeId){
        //this is a centroid
        drawNode(nodeId,centroidColor(nodeId),true,graph.nodes(nodeId).transitAllowed,radius)
      }else{
        //this is not a centroid, check for a marked node
        nodeToCentroid.get(nodeId) match{
          case Some(centroidIDArray) =>

            centroidIDArray.indices.foreach(index => {
              val centroidID = centroidIDArray(index)
              //a folowed node
              if (centroidID == -1) {
                //not reacheable
                drawNode(nodeId, colorForUnreachableNodes, false, graph.nodes(nodeId).transitAllowed, radius,index)
              } else {
                //reachable by centroidID
                drawNode(nodeId, centroidColor(centroidID), false, graph.nodes(nodeId).transitAllowed, radius,index)
              }
            })
          case None =>
            if (extraCentroids contains nodeId)
              drawNode(nodeId,Color.white,true,graph.nodes(nodeId).transitAllowed,radius)
          //not a followed node, set default black color, small dot
          //drawNode(node:NodeWithIntegerCoordinates,colorForUnreachableNodes,false)
        }
      }
    }
  }

  def drawNode(nodeId:Int,
               color:Color,
               isCentroid:Boolean,
               isTransitAllowed:Boolean,
               radius : Int,
               index : Int = 0): Unit = {
    val nodeCoordinates = graph.coordinates(nodeId)
    if(isCentroid){
      //rectangle
      val side = 12
      val tempPoint = rectangleShapes(nodeId)
      tempPoint.move(nodeCoordinates._1 * xMultiplier - side/2,
        nodeCoordinates._2 * yMultiplier - side/2)
      tempPoint.visible_=(true)
      tempPoint.innerCol_$eq(color)
      tempPoint.toolTip = s"Centroid(id:$nodeId transit:$isTransitAllowed)"
    }else {
      if(isTransitAllowed) {
        //circle

        val circle = circleShape(nodeId)(index)
        circle.move(nodeCoordinates._1 * xMultiplier,
          nodeCoordinates._2 * yMultiplier)
        circle.visible_=(true)
        circle.innerCol_$eq(color)
        circle.toolTip = s"Node(id:$nodeId transit:true)"


      }else {
        //cross
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

        lineV.toolTip = s"Node(id:$nodeId transit:false)"
        lineH.toolTip = s"Node(id:$nodeId transit:false)"
        val circle = circleShape(nodeId)(index)
        circle.move(nodeCoordinates._1 * xMultiplier,nodeCoordinates._2 * yMultiplier)
        circle.visible_=(true)
        circle.border = false
        circle.innerCol_=(color)
        circle.toolTip = s"Node(id:$nodeId transit:true)"
      }
    }
  }

  def emphNode(nodeCoordinates:(Int,Int),
               color:Color): Unit = {
    //circle
    val radius = 6
    val tempPoint = new VisualCircle(this,
      nodeCoordinates._1 * xMultiplier,
      nodeCoordinates._2 * yMultiplier,
      radius)
    tempPoint.innerCol_$eq(color)
  }

  def drawEdges(openConditions:SortedSet[Int],
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

    val line = edgesShape(edge.id)
    line.orig_=(fromCoord._1 * xMultiplier,fromCoord._2 * yMultiplier)
    line.dest_=(toCoord._1 * xMultiplier,toCoord._2 * yMultiplier)
    line.visible_=(true)

//    val line = new VisualLine(this, new Double(
//      fromCoord._1 * xMultiplier,
//      fromCoord._2 * yMultiplier,
//      toCoord._1 * xMultiplier,
//      toCoord._2 * yMultiplier))

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
    implicit val ev: Ordering[Edge] = EdgeOrdering

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
