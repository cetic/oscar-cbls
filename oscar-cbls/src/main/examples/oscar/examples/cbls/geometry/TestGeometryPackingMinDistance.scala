package oscar.examples.cbls.geometry

import java.awt.Color

import org.locationtech.jts.geom.Coordinate
import oscar.cbls.business.geometry
import oscar.cbls.business.geometry.invariants._
import oscar.cbls.business.geometry.model.CBLSGeometryConst
import oscar.cbls.core.computation.IntValue
import oscar.cbls.core.search.{ConstantMovesNeighborhood, EvaluableCodedMove}
import oscar.cbls.lib.invariant.numeric.{Sqrt, Square, Sum, Sum2}
import oscar.cbls.lib.search.combinators.{Atomic, BestSlopeFirst, Profile}
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.visual.geometry.GeometryDrawing
import oscar.cbls.visual.{ColorGenerator, SingleFrameWindow}
import oscar.cbls.{CBLSIntVar, Objective, Store}

/**
  * This demo tries to monimize the distance between centers of shapes plus the overlap distance
  * while keeping them all within a rectangle.
  * it proceeds through local search (although a more symbolic insertion-based technique would be more efficient
  */
object TestGeometryPackingMinDistance extends App{

  val store = Store()

  val nbShapes = 10

  val radiusArray = Array.tabulate(nbShapes:Int){
    i => 30*(i+1)
  }

  val maxX = 1100
  val maxY = 1380

  val outerFrame = geometry.factory.createLinearRing(Array(
    new Coordinate(0,0),
    new Coordinate(0,maxY),
    new Coordinate(maxX,maxY),
    new Coordinate(maxX,0),
    new Coordinate(0,0))).convexHull()

  //declaring the optimization model
  val coordArray = Array.tabulate(nbShapes){ i =>
    (new CBLSIntVar(store,radiusArray(i),radiusArray(i) to maxX - radiusArray(i),"shape_" + i + ".x"),
      new CBLSIntVar(store,radiusArray(i),radiusArray(i) to maxY - radiusArray(i),"shape_" + i + ".y"))
  }

  val constantShapesAt00 = Array.tabulate(nbShapes){ i =>
    new CBLSGeometryConst(store,
      if(i%2 ==0) geometry.createRectangle(radiusArray(i)*2,radiusArray(i)*2)
      else geometry.createCircle(radiusArray(i),nbEdges = 30),
      "shape_" + i)
  }

  //creating the shapes at the location taken fr omteh decision variables,
  // this is obtained by applying a translation on the constant shapes
  val placedShapes = Array.tabulate(nbShapes){ i =>
    new Apply(store,new Translation(store:Store,coordArray(i)._1,coordArray(i)._2),constantShapesAt00(i))
  }

  //this is about evaluating the overlap between shapes

  //we first create an overlap half matrix (thus it is not a global constraint)
  //also the overlap area is a slow-to-evaluate criterion for quantifying overlap
  val overlapAreasHalfMatrix:Array[Array[IntValue]] =
  Array.tabulate(nbShapes)(cirleID1 =>
    Array.tabulate(cirleID1)(cirleID2 =>
      Area(store,Intersection(store,placedShapes(cirleID1),placedShapes(cirleID2)))))

  val totalOverlapArea:IntValue = new Sum(overlapAreasHalfMatrix.flatMap(_.toList)).setName("totalOverlapArea")

  val overlapPerShape:Array[IntValue] = Array.tabulate(nbShapes)(shape1 =>
    Sum(Array.tabulate[IntValue](nbShapes)(shape2 =>
      if(shape1 == shape2) 0
      else overlapAreasHalfMatrix(shape1 max shape2)(shape1 min shape2)))
  )

  //bout the distances between centers of shapes

  //these are the distances we consider, from shape to shape
  val relevantDistances = List((0,1),(1,2),(1,3),(2,4),(3,5),(4,5),(5,6))

  val distancesArray = relevantDistances.map({case (fromId,toId) => {
    val (x1, y1) = coordArray(fromId)
    val (x2, y2) = coordArray(toId)
    Sqrt(Sum2(Square(x1 - x2),Square(y1 - y2))) //pytagore
  }})

  //val convexHullOfCircle1And3 = new ConvexHull(store, new Union(store, placedCirles(1),placedCirles(3)))

  val obj:Objective = Sum2(totalOverlapArea,new Sum(distancesArray))
  store.close()

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  val randomColors = ColorGenerator.generateRandomColors(nbShapes,175).toList

  val drawing = GeometryDrawing(relevantDistances) //this parameters specifies which centers are linked

  def updateDisplay() {
    val colorsIt = randomColors.toIterator
    drawing.drawShapes(shapes =
      (outerFrame,Some(Color.red),None,"")::
        Array.tabulate(nbShapes)(circleID => (
          placedShapes(circleID).value.geometry,
          None,
          Some(colorsIt.next),
          overlapPerShape(circleID).toString)).toList,
      centers = coordArray.toList.map(xy => (xy._1.value,xy._2.value)))
  }

  updateDisplay()

  SingleFrameWindow.show(drawing,"Packing and minimizing some shape to shape distance",1125,1400)

  //end of graphical stuff
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //Start of search

  val flattenedCoordArray:Array[CBLSIntVar] = coordArray.flatMap(xy => List(xy._1,xy._2))

  //declare a collection of neighborhoods

  def moveOneCoordNumeric = NumericAssignNeighborhood(flattenedCoordArray,"moveByOneCoord",
    domainExplorer = () => (dXForDetivativeEvalution: Int, maxIt: Long) => new NewtonRaphsonMinimize(20, 10)
    //these are alternative methods for the numerical optimization
    //new NarrowingExhaustive(dividingRatio = 10, maxIt = 10)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def moveXNumeric = NumericAssignNeighborhood(coordArray.map(_._1),"moveX",
    domainExplorer = () => (dividingRatio: Int, minStep: Long) => new NarrowingExhaustive(dividingRatio = 10, minStep = 1)
    //these are alternative methods for the numerical optimization
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def smallSlideY(shapeID:Int) = NumericAssignNeighborhood(Array(coordArray(shapeID)._2),"moveYSlave",
    domainExplorer = () => (step: Int, maxIt: Long) => new Slide(step=1,maxIt=20)
    //these are alternative methods for the numerical optimization
    // new NarrowingExhaustive(dividingRatio = 10, maxIt = 10)
    // new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def moveYNumeric = NumericAssignNeighborhood(coordArray.map(_._2),"moveY",
    //these are alternative methods for the numerical optimization
    domainExplorer = () => (dividingRatio: Int, minStep: Long) => new NarrowingExhaustive(dividingRatio = 10, minStep = 1)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def smallSlideX(circleID:Int) = NumericAssignNeighborhood(Array(coordArray(circleID)._1),"moveXSlave",
    domainExplorer = () => (step: Int, maxIt: Long) => new Slide(step=1,maxIt=20)
    //these are alternative methods for the numerical optimization
    // new NarrowingExhaustive(dividingRatio = 10, maxIt = 10)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def swapX = SwapsNeighborhood(
    coordArray.map(_._1),
    symmetryCanBeBrokenOnIndices = false,
    adjustIfNotInProperDomain = true,
    name = "swapXCoordinates")

  def swapYSlave(shape1:Int, shape2:Int) =
    SwapsNeighborhood(
      Array(coordArray(shape1)._2,coordArray(shape2)._2),
      symmetryCanBeBrokenOnIndices = false,
      adjustIfNotInProperDomain = true,
      name ="swapYCoordinates")

  //now we declare more complex neighborhoods that are composites of the above

  def swapAndSlide = (swapX dynAndThen(swapMove => {
    (swapYSlave(swapMove.idI,swapMove.idJ)
      andThen Atomic(BestSlopeFirst(List(smallSlideX(swapMove.idI),smallSlideY(swapMove.idI))),_>100))})
    ) name "SwapAndSlide"

  def moveOneShapeXAndThenY = moveXNumeric dynAndThen (assignMode => smallSlideY(assignMode.id)) name "moveXAndThenY"
  def moveOneShapeYAndThenX = moveYNumeric dynAndThen (assignMode => smallSlideX(assignMode.id)) name "moveYAndThenX"

  def moveOneCoordClassic = AssignNeighborhood(flattenedCoordArray,"moveByOneCoord")  //this one is awfully slow!!!


  def moveToHole = ConstantMovesNeighborhood[MoveShapeTo](
    () => {
      val allShapes = placedShapes.map(_.value)

      val holes:Iterable[(Int,Int)] = Overlap.centroidsOfFreeSpacesIn(allShapes.map(_.geometry),outerFrame)

      allShapes.indices.flatMap(circleID => {

        val oldX = coordArray(circleID)._1.valueInt
        val oldY = coordArray(circleID)._2.valueInt

        holes.map(hole => {
          (newObj:Long) => new MoveShapeTo(circleID,hole._1,hole._2,oldX,oldY,newObj)
        })
      })
    },
    neighborhoodName = "toHole"
  )

  class MoveShapeTo(val shapeID:Int, targetX:Int, targetY:Int, oldX:Int, oldY:Int, newObj:Long)
    extends EvaluableCodedMove(() => {
      coordArray(shapeID)._1 := (targetX max radiusArray(shapeID)) min (maxX - radiusArray(shapeID))
      coordArray(shapeID)._2 := (targetY max radiusArray(shapeID)) min (maxY - radiusArray(shapeID))

      () => {
        coordArray(shapeID)._1 := oldX
        coordArray(shapeID)._2 := oldY
      }
    },
      neighborhoodName = "toHole",
      objAfter = newObj){}


  def moveToHoleAndGradient =
    moveToHole dynAndThen(moveShapeToMove => new Atomic(
      gradientOnOneShape(moveShapeToMove.shapeID),
      _>10,
      stopAsSoonAsAcceptableMoves=true)) name "toHole&Gradient"

  def gradientOnOneShape(shapeID:Int) = new GradientDescent(
    vars = Array(coordArray(shapeID)._1,coordArray(shapeID)._2),
    name= "GradientMove(" + shapeID + ")",
    maxNbVars = 2,
    selectVars = List(0,1),
    variableIndiceToDeltaForGradientDefinition = _ => 20,
    hotRestart = false,
    linearSearch = new NarrowingStepSlide(dividingRatio = 10, minStep = 1)
    //new Slide(step=20,maxIt=100)
    // new NewtonRaphsonMinimize(5,40)
  )

  def swapAndGradient = swapX dynAndThen(swapMove => (
    swapYSlave(swapMove.idI,swapMove.idJ)
      andThen new Atomic(gradientOnOneShape(swapMove.idI),
      _>10,
      stopAsSoonAsAcceptableMoves=true))) name "swap&Gradient"


  val displayDelay:Long = 1000.toLong * 1000 * 1000 //1 seconds
  var lastDisplay = System.nanoTime()
  val search = (Profile(BestSlopeFirst(
    List(
      Profile(gradientOnOneShape(0)),  //TODO: try gradient on multiple shapes at the same time.
      Profile(gradientOnOneShape(1)),
      Profile(gradientOnOneShape(2)),
      Profile(gradientOnOneShape(3)),
      Profile(gradientOnOneShape(4)),
      Profile(gradientOnOneShape(5)),
      Profile(gradientOnOneShape(6)),
      Profile(gradientOnOneShape(7)),
      Profile(gradientOnOneShape(8)),
      Profile(gradientOnOneShape(9)),
      Profile(moveToHole),
      Profile(moveToHoleAndGradient),
      Profile(moveOneCoordNumeric),
      Profile(moveOneShapeXAndThenY),
      Profile(swapAndSlide),
      Profile(swapAndGradient),
      Profile(moveOneShapeYAndThenX)
    ),
    refresh=nbShapes*10))
    onExhaustRestartAfter(
    RandomizeNeighborhood(flattenedCoordArray, () => flattenedCoordArray.length/5, name = "smallRandomize"),
    maxRestartWithoutImprovement = 2,
    obj)
    onExhaustRestartAfter (
    RandomizeNeighborhood(flattenedCoordArray, () => flattenedCoordArray.length, name = "fullRandomize"),
    maxRestartWithoutImprovement = 2,
    obj)
    afterMove {
    if(System.nanoTime() > lastDisplay + displayDelay) {
      updateDisplay()
      lastDisplay = System.nanoTime()
    }
  } showObjectiveFunction obj)

  // Thread.sleep(20000)
  // println("start")
  // Thread.sleep(1000)

  search.verbose = 1
  search.doAllMoves(obj=obj, shouldStop = _ => obj.value == 0)

  updateDisplay() //after finish

  println(search.profilingStatistics)
  println
  println(overlapPerShape.mkString("\n"))
  println(totalOverlapArea)

}
