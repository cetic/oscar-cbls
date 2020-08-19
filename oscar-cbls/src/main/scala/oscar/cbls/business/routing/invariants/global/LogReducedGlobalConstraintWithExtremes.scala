package oscar.cbls.business.routing.invariants.global

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}

abstract class LogReducedGlobalConstraintWithExtremes[T:Manifest, @specialized(Int, Long, Boolean) U:Manifest](gc: GlobalConstraintCore, n: Int, v: Int)
  extends LogReducedGlobalConstraint[T, U](gc,n,v){
  var preComputeTime = 0L
  var preComputeCount = 0
  var computeValueTime = 0L
  var computeValueCount = 0

  class NodeAndExtremePreComputes(val node:Int,
                                  var fromStart:T = null.asInstanceOf[T],
                                  var toEnd:T = null.asInstanceOf[T]){
    override def toString: String = {
      s"NodeAndExtremePreComputes(node:$node fromStart:$fromStart toEnd:$toEnd)"
    }
  }

  private val vehicleToExtremePrecomputes:Array[Array[NodeAndExtremePreComputes]] = Array.fill(v)(null)

  private def identifyNodesAndAllocateExtremePrecomputes(e:Option[IntSequenceExplorer],
                                                         vehicle:Int,positionInVehicleRoute:Int,
                                                         preComputedVals:Array[VehicleAndPosition]): Unit ={
    e match {
      case None =>
        //end
        vehicleToExtremePrecomputes(vehicle) = Array.fill(positionInVehicleRoute+1)(null)
        vehicleToExtremePrecomputes(vehicle)(positionInVehicleRoute) = new NodeAndExtremePreComputes(vehicle)

      case  Some(x) if x.value < v && x.value != vehicle => ;
        //end
        vehicleToExtremePrecomputes(vehicle) = Array.fill(positionInVehicleRoute+1)(null)
        vehicleToExtremePrecomputes(vehicle)(positionInVehicleRoute) = new NodeAndExtremePreComputes(vehicle)

      case Some(ex) =>
        identifyNodesAndAllocateExtremePrecomputes(ex.next, vehicle, positionInVehicleRoute + 1, preComputedVals)

        vehicleToExtremePrecomputes(vehicle)(positionInVehicleRoute) = new NodeAndExtremePreComputes(ex.value)
    }
  }

  override def performPreCompute(vehicle: Int, routes: IntSequence): Unit = {
    val start = System.nanoTime()
    super.performPreCompute(vehicle, routes)
    //also compute the extremes forward and backwards

    identifyNodesAndAllocateExtremePrecomputes(
      routes.explorerAtAnyOccurrence(vehicle),
      vehicle,
      0,
      preComputedVals)

    val extremePrecomputesOfCurrentVehicle = vehicleToExtremePrecomputes(vehicle)
    val nv = extremePrecomputesOfCurrentVehicle.length-1 //return is ignored here, corrected later
    require(nv >= 1, nv)
    //forward precompute

    extremePrecomputesOfCurrentVehicle(0).fromStart = nodeValue(vehicle)

    for (i <- 1 until nv) {
      extremePrecomputesOfCurrentVehicle(i).fromStart =
        composeSteps(
          extremePrecomputesOfCurrentVehicle(i - 1).fromStart,
          nodeValue(extremePrecomputesOfCurrentVehicle(i).node))
    }

    extremePrecomputesOfCurrentVehicle(nv).fromStart =
      composeSteps(
        extremePrecomputesOfCurrentVehicle(nv - 1).fromStart,
        endNodeValue(vehicle))

    extremePrecomputesOfCurrentVehicle(nv).toEnd = endNodeValue(vehicle)

    //backward precompute
    for (i <- (0 until nv).reverse) {
      extremePrecomputesOfCurrentVehicle(i).toEnd =
        composeSteps(
          nodeValue(extremePrecomputesOfCurrentVehicle(i).node),
          extremePrecomputesOfCurrentVehicle(i+1).toEnd)
    }
    preComputeTime += System.nanoTime() - start
    preComputeCount += 1
  }

  override def computeVehicleValue(vehicle:Int,
                                   segments:QList[Segment],
                                   routes:IntSequence): U = {
    computeValueCount += 1
    val start = System.nanoTime()
    val res = computeVehicleValueComposed(
      vehicle,
      decorateSegmentsExtremes(
        vehicle,
        segments,
        isFirst = true))
    computeValueTime += System.nanoTime() - start
    res
  }

  def decorateSegmentsExtremes(vehicle:Int,
                               segments:QList[Segment],
                               isFirst:Boolean):QList[LogReducedSegment[T]] = {

    //TODO: it seems that the list construction produces a significant overhead. These might be replaced by QLists.
    segments match{
      case null =>
        //back to start; we add a single node (this will seldom be used, actually, since back to start is included in PreComputedSubSequence that was not flipped
        QList(LogReducedPreComputedSubSequenceGiven[T](
          vehicle: Int, vehicle: Int,
          QList(endNodeValue(vehicle))))
      case q =>
        q.head match{
          case PreComputedSubSequence
            (startNode: Int, endNode: Int, length: Int) =>
            val startNodeValue = preComputedVals(startNode)
            val endNodeValue = preComputedVals(endNode)

            if(isFirst){
              require(vehicle == startNodeValue.vehicle)
              //println("FROM START EXTREME!")

              QList(LogReducedPreComputedSubSequenceGiven[T](
                startNode: Int, endNode: Int,
                QList(vehicleToExtremePrecomputes(vehicle)(endNodeValue.positionInVehicleRoute).fromStart)
              ), decorateSegmentsExtremes(vehicle:Int, segments = q.tail,isFirst = false))

            } else if(
              startNodeValue.vehicle == vehicle
                && q.tail == null //tested second because more time consuming than the first condition
                && endNodeValue.positionInVehicleRoute == vehicleToExtremePrecomputes(vehicle).length-2L
            ){
              //last one, on the same vehicle as when pre-computation was performed, and nothing was removed until the end of this route

              //println("TO END EXTREME!")

              QList(LogReducedPreComputedSubSequenceGiven[T](
                startNode: Int, endNode: Int,
                QList(vehicleToExtremePrecomputes(vehicle)(startNodeValue.positionInVehicleRoute).toEnd)
              ))
            }else{
              QList(LogReducedPreComputedSubSequenceLazy[T](
                startNode: Int, endNode: Int,
                stepGenerator = () => extractSequenceOfT(
                  startNodeValue.vehicle, startNodeValue.positionInVehicleRoute,
                  endNodeValue.positionInVehicleRoute, flipped = false)), decorateSegmentsExtremes(vehicle:Int, segments = q.tail,isFirst = false))
            }
          case FlippedPreComputedSubSequence(
          startNode: Int, endNode: Int, length: Int) =>
            val startNodeValue = preComputedVals(startNode)
            val endNodeValue = preComputedVals(endNode)

            QList(LogReducedFlippedPreComputedSubSequence[T](
              startNode: Int, endNode: Int,
              stepGenerator = () => extractSequenceOfT(
                startNodeValue.vehicle, startNodeValue.positionInVehicleRoute,
                endNodeValue.positionInVehicleRoute, flipped = true)), decorateSegmentsExtremes(vehicle:Int, segments = q.tail,isFirst = false))

          case NewNode(node: Int) =>
            QList(LogReducedNewNode[T](node: Int, nodeValue(node)), decorateSegmentsExtremes(vehicle:Int, segments = q.tail,isFirst = false))
        }
    }
  }
}
