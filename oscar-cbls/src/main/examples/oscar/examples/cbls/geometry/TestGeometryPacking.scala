package oscar.examples.cbls.geometry

import java.awt.Color

import org.locationtech.jts.geom.Coordinate
import oscar.cbls.business.geometry
import oscar.cbls.business.geometry.invariants.{Apply, NoOverlapPenetration, Overlap, Translation}
import oscar.cbls.business.geometry.model.{CBLSGeometryConst, GeometryValue}
import oscar.cbls.core.computation.{AtomicValue, IntValue}
import oscar.cbls.core.search.{ConstantMovesNeighborhood, EvaluableCodedMove}
import oscar.cbls.lib.search.combinators.{Atomic, BestSlopeFirst, Profile}
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.visual.geometry.{GeometryDrawing, GeometryDrawingTypes}
import oscar.cbls.visual.{ColorGenerator, SingleFrameWindow}
import oscar.cbls.{CBLSIntVar, Objective, Store, atomic, longToInt}

/**
  * this demo tries to pack a set of shapes within a rectangle
  * in such a way that there is no overlap between these shapes.
  * it proceeds through local search (although a more symbolic insertion-based technique would be more efficient
  */
object TestGeometryPacking extends App{

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

  //declaring the decision variables; the XY of the center of the shapes
  val coordArray = Array.tabulate(nbShapes){ i =>
    (new CBLSIntVar(store,radiusArray(i),radiusArray(i) to maxX - radiusArray(i),"shape_" + i + ".x"),
      new CBLSIntVar(store,radiusArray(i),radiusArray(i) to maxY - radiusArray(i),"shape_" + i + ".y"))
  }

  //creating a set of constant shapes with center at 0,0
  val constantShapesAt00 = Array.tabulate(nbShapes){ i =>
    new CBLSGeometryConst(store,
      if(i%2 ==0) geometry.createRectangle(radiusArray(i)*2,radiusArray(i)*2)
      else geometry.createCircle(radiusArray(i),nbEdges = 30),
      "shape_" + i + (if (i%2 ==0) "rectangle" + radiusArray(i)*2 else "circle" + radiusArray(i)))
  }

  for(shape <- constantShapesAt00){
    println(shape)
  }

  //creating the shapes at the location taken from the decision variables,
  // this is obtained by applying a translation on the constant shapes
  val placedShapes:Array[AtomicValue[GeometryValue]] = Array.tabulate(nbShapes){ i =>
    new Apply(store,new Translation(store:Store,coordArray(i)._1,coordArray(i)._2),constantShapesAt00(i))
  }

  //this is about evaluating the overlap between shapes
/*
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
      else overlapAreasHalfMatrix(shape1 max shape2)(shape1 min shape2))).setName("overlapArea(shape_" + shape1 + ")")
  )
*/

  val overlapPenetrationConstraint = new NoOverlapPenetration(placedShapes)
  val overlapPenetrationPerShape:Array[IntValue] = Array.tabulate(nbShapes)(shape1 => overlapPenetrationConstraint.violation(shape1))

  //val convexHullOfCircle1And3 = new ConvexHull(store, new Union(store, placedCirles(1),placedCirles(3)))

  val obj:Objective = overlapPenetrationConstraint.output
  store.close()

 println("overlapPenetrationPerShape:" + overlapPenetrationPerShape.mkString(","))

  //End of the model
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //start of the graphical stuff

  val randomColors = ColorGenerator.generateRandomColors(nbShapes,175).toList

  val drawing = GeometryDrawing(List.empty, GeometryDrawingTypes.Simple)

  //this method is for updating the display, throughout the search, or after the search
  def updateDisplay() {
    val colorsIt = randomColors.toIterator
    drawing.drawShapes(shapes =
      (outerFrame,Some(Color.red),None,"")::
        Array.tabulate(nbShapes)(circleID => (
          placedShapes(circleID).value.geometry,
          None,
          Some(colorsIt.next),
          overlapPenetrationPerShape(circleID).toString)).toList.reverse,
      centers = coordArray.toList.map(xy => (xy._1.value,xy._2.value)))
  }

  updateDisplay()

  SingleFrameWindow.show(drawing,"searching for a packing without overlap",1125,1400)

  //end of graphical stuff
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //Start of search

  val flattenedCoordArray:Array[CBLSIntVar] = coordArray.flatMap(xy => List(xy._1,xy._2))

  //declare a collection of neighborhoods

  def moveOneCoordNumeric = NumericAssignNeighborhood(flattenedCoordArray,"moveByOneCoord",
    domainExplorer = () => (dXForDetivativeEvalution: Long, maxIt: Long) => new NewtonRaphsonMinimize(20, 10)
    //these are alternative methods for the numerical optimization
    //new NarrowingExhaustive(dividingRatio = 10, maxIt = 10)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def moveXNumeric = NumericAssignNeighborhood(coordArray.map(_._1),"moveX",
    domainExplorer = () => (dividingRatio: Long, minStep: Long) => new NarrowingExhaustive(dividingRatio = 10, minStep = 1)
    //these are alternative methods for the numerical optimization
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def smallSlideY(shapeID:Int) = NumericAssignNeighborhood(Array(coordArray(shapeID)._2),"moveYSlave",
    domainExplorer = () => (step: Long, maxIt: Long) => new Slide(step=1,maxIt=20)
    //these are alternative methods for the numerical optimization
    // new NarrowingExhaustive(dividingRatio = 10, maxIt = 10)
    // new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def moveYNumeric = NumericAssignNeighborhood(coordArray.map(_._2),"moveY",
    //these are alternative methods for the numerical optimization
    domainExplorer = () => (dividingRatio: Long, minStep: Long) => new NarrowingExhaustive(dividingRatio = 10, minStep = 1)
    //new Exhaustive(step = 50,skipInitial = true,maxIt = 1000/50)
  )

  def smallSlideX(circleID:Int) = NumericAssignNeighborhood(Array(coordArray(circleID)._1),"moveXSlave",
    domainExplorer = () => (step: Long, maxIt: Long) => new Slide(step=1,maxIt=20)
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

  def swapAndSlide = ((swapX dynAndThen(swapMove =>
    (swapYSlave(swapMove.idI,swapMove.idJ)
      andThen Atomic(BestSlopeFirst(List(smallSlideX(swapMove.idI),smallSlideY(swapMove.idI))),_>100))))
    ) name "SwapAndSlide"

  def moveOneShapeXAndThenY = moveXNumeric dynAndThen (assignMode => smallSlideY(assignMode.id)) name "moveXAndThenY"

  def moveOneShapeYAndThenX = moveYNumeric dynAndThen (assignMode => smallSlideX(assignMode.id)) name "moveYAndThenX"

  def moveOneCoordClassic = AssignNeighborhood(flattenedCoordArray,"moveByOneCoord")  //this one is awfully slow!!!


  def moveToHole = ConstantMovesNeighborhood[MoveShapeTo](
    () => {
      val allShapes = placedShapes.map(_.value)

      val holes:Iterable[(Int,Int)] = Overlap.centroidsOfFreeSpacesIn(allShapes.map(_.geometry),outerFrame)

      allShapes.indices.flatMap(shapeID => {

        val oldX = coordArray(shapeID)._1.value
        val oldY = coordArray(shapeID)._2.value

        holes.map(hole => {
          (newObj:Long) => new MoveShapeTo(shapeID,hole._1,hole._2,oldX,oldY,newObj)
        })
      })
    },
    neighborhoodName = "toHole"
  )

  class MoveShapeTo(val shapeID:Int, targetX:Long, targetY:Long, oldX:Long, oldY:Long, newObj:Long)
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
  ) guard (() => overlapPenetrationPerShape(shapeID).value !=0)

  def swapAndGradient = swapX dynAndThen(swapMove => (
    swapYSlave(swapMove.idI,swapMove.idJ)
      andThen new Atomic(gradientOnOneShape(swapMove.idI),
      _>1,
      stopAsSoonAsAcceptableMoves=true))) name "swap&Gradient"

  val displayDelay:Long = 1000.toLong * 1000 * 1000 * 2 //2 seconds
  var lastDisplay = System.nanoTime()

  val search = (Profile(BestSlopeFirst(
    List(
      atomic(gradientOnOneShape(0), _ > 10) name "gradient0",  //TODO: try gradient on multiple shapes at the same time.
      Atomic(gradientOnOneShape(1), _ > 10) name "gradient1",
      Atomic(gradientOnOneShape(2), _ > 10) name "gradient2",
      Atomic(gradientOnOneShape(3), _ > 10) name "gradient3",
      Atomic(gradientOnOneShape(4), _ > 10) name "gradient4",
      Atomic(gradientOnOneShape(5), _ > 10) name "gradient5",
      Atomic(gradientOnOneShape(6), _ > 10) name "gradient6",
      Atomic(gradientOnOneShape(7), _ > 10) name "gradient7",
      Atomic(gradientOnOneShape(8), _ > 10) name "gradient8",
      Atomic(gradientOnOneShape(9), _ > 10) name "gradient9",
      moveToHole,
      moveToHoleAndGradient,
      moveOneCoordNumeric,
      moveOneShapeXAndThenY,
      swapAndSlide,
      swapAndGradient,
      moveOneShapeYAndThenX).map(Profile(_)),
    refresh=nbShapes*10))
    onExhaustRestartAfter(
    RandomizeNeighborhood(flattenedCoordArray, () => flattenedCoordArray.length/5, name = "smallRandomize"),
    maxRestartWithoutImprovement = 1,
    restartFromBest = true,
    obj=obj)
    onExhaustRestartAfter (
    RandomizeNeighborhood(flattenedCoordArray, () => flattenedCoordArray.length, name = "fullRandomize"),
    maxRestartWithoutImprovement = 50,
    restartFromBest = true,
    obj=obj)
    afterMove {
    if(System.nanoTime() > lastDisplay + displayDelay) {
      updateDisplay()
      lastDisplay = System.nanoTime()
    }
  } showObjectiveFunction obj)

  search.verbose = 1
  search.doAllMoves(obj=obj, shouldStop = _ => obj.value <= 0)

  updateDisplay() //after finish

  println(search.profilingStatistics)
  println
  println(overlapPenetrationPerShape.mkString("\n"))
  println(overlapPenetrationConstraint.output)
}
