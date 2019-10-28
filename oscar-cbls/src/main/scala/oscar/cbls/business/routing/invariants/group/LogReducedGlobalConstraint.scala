package oscar.cbls.business.routing.invariants.group

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/


import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.ChangingSeqValue
import oscar.cbls._


sealed abstract class LogReducedSegment[T]()


/**
  * This represents a subsequence starting at startNode and ending at endNode.
  * This subsequence was present in the global sequence when the pre-computation was performed
  * @param startNode the first node of the subsequence
  * @param endNode the last node of the subsequence
  * @param steps: a list of values of type T such that
  *             if they were concatenated using the compose function,
  *             it would yield the value t between fromNode and toNode.
  *             there are O(log(n)) of thee values in the list
  * @tparam T the type of precomputation
  */
abstract sealed class LogReducedPreComputedSubSequence[T](val startNode:Long,
                                                          val endNode:Long) extends LogReducedSegment[T]{

  def steps:QList[T]

  override def toString: String = {
    "LogReducedPreComputedSubSequence(startNode:" + startNode +
      " endNode:" + endNode + " steps:{" + steps.mkString(",") + "})"
  }
}

object LogReducedPreComputedSubSequence{
  def unapply[T](l:LogReducedPreComputedSubSequence[T]):Option[(Long,Long,QList[T])] = {
    Some((l.startNode,l.endNode,l.steps))
  }
}

class LogReducedPreComputedSubSequenceLazy[T](startNode:Long,
                                              endNode:Long,
                                              stepGenerator: () => QList[T])
  extends  LogReducedPreComputedSubSequence[T](startNode:Long, endNode:Long){

  private var generatedSteps:Option[QList[T]] = None
  override def steps:QList[T] = {
    generatedSteps match{
      case None =>
        val a = stepGenerator()
        generatedSteps = Some(a)
        a
      case Some(a) => a
    }
  }

  override def toString: String = {
    "LogReducedPreComputedSubSequence(startNode:" + startNode +
      " endNode:" + endNode + " steps:{" + steps.mkString(",") + "})"
  }
}

object LogReducedPreComputedSubSequenceLazy{
  def apply[T](startNode:Long,
               endNode:Long,
               stepGenerator: () => QList[T]):LogReducedPreComputedSubSequenceLazy[T] =
    new LogReducedPreComputedSubSequenceLazy[T](startNode,
      endNode,
      stepGenerator)
}

class LogReducedPreComputedSubSequenceGiven[T](startNode:Long,
                                               endNode:Long,
                                               val steps:QList[T])
  extends LogReducedPreComputedSubSequence[T](startNode:Long,endNode:Long){

  override def toString: String = {
    "LogReducedPreComputedSubSequence(startNode:" + startNode +
      " endNode:" + endNode + " steps:{" + steps.mkString(",") + "})"
  }
}


object LogReducedPreComputedSubSequenceGiven {

  def apply[T](startNode: Long,
               endNode: Long,
               steps: QList[T]): LogReducedPreComputedSubSequenceGiven[T] =
    new LogReducedPreComputedSubSequenceGiven[T](startNode: Long,
      endNode: Long,
      steps: QList[T])
}

/**
  * This represents a subsequence starting at startNode and ending at endNode.
  * This subsequence was not present in the global sequence when the pre-computation was performed, but
  * the flippedd subsequence obtained by flippig it was present in the global sequence when the pre-computation was performed, but
  * @param startNode the first node of the subsequence
  * @param endNode the last node of the subsequence
  * @param steps: a list of values of type T such that
  *             if they were concatenated using the compose function,
  *             it would yield the value t between toNode and fromNode, as in the original sequence.
  *             there are O(log(n)) of thee values in the list
  * @tparam T the type of precomputation
  */
class LogReducedFlippedPreComputedSubSequence[T](val startNode:Long,
                                                 val endNode:Long,
                                                 stepGenerator: () => QList[T]) extends LogReducedSegment[T]{

  lazy val steps:QList[T] = stepGenerator()

  override def toString: String = {
    "LogReducedFlippedPreComputedSubSequence(startNode:" + startNode +
      " endNode:" + endNode + " steps:{" + steps.mkString(",") + "})"
  }
}

object LogReducedFlippedPreComputedSubSequence {

  def apply[T](startNode:Long, endNode:Long, stepGenerator: () => QList[T]) =
    new LogReducedFlippedPreComputedSubSequence[T](startNode,endNode, stepGenerator)

  def unapply[T](l: LogReducedFlippedPreComputedSubSequence[T]): Option[(Long, Long, QList[T])] = {
    Some((l.startNode, l.endNode, l.steps))
  }
}
  /**
    * This represent that a node that was not present in the initial sequence
    * when pre-computation was performed.
    * @param node
    */
  case class LogReducedNewNode[T](node:Long, value:T) extends LogReducedSegment[T]{
    override def toString: String = {
      "LogReducedNewNode(node:" + node + ")"
    }
  }




  /**
    * This API provides an easy to use framework for defining a custom global constraint for vehicle routing.
    * it is to be used when the pre-computation needs to be performed on every possible sub-sequence of route,
    * thus pre-computation is O(n²)-time to achieve O(1L) query time per segment.
    *
    * This particular API provides a reduction of the pre-computation time to O(n)
    * at the cost of performing the segment query in O(log(n))
    *
    * The difference in implementation is that it does not decorates evey possible segment with a value of type T.
    * Instead it decorates a subset of these segments, and each queried segment has a sequence of values of type T
    * associated with it. These values are queried from the pre-computation.
    * The assembly includes O(log(n)) of these pre-computations.
    *
    * @param gc the GlobalConstraint linked to this constraint
    * @param n the number of nodes
    * @param v the number of vehicles
    * @tparam T the type of pre-computation, which is on subsequences (not on nodes)
    */
  abstract class LogReducedGlobalConstraint[T:Manifest](gc: GlobalConstraintCore, n: Long, v :Long)
    extends GlobalConstraintDefinition(gc,v){

    /**
      * this method delivers the value of the node
      * @return the type T associated with the node "node"
      */
    def nodeValue(node: Long): T

    /**
      * this one is similar to the nodeValue except that it only is applied on vehicle,
      * to represent the return to the vehicle start at teh end of its route
      * @param vehicle
      * @return
      */
    def endNodeValue(vehicle:Long):T

    /**
      * this method is for composing steps into bigger steps.
      * @param firstStep the type T associated with stepping over a sequence of nodes (which can be minial two)
      * @param secondStep the type T associated with stepping over a sequence of nodes (which can be minial two)
      * @return the type T associated wit hthe first step followed by the second step
      */
    def composeSteps(firstStep: T, secondStep: T): T

    //TODO: this method might also sent the vehicle value when pre-computation was performed?
    /**
      * this method is called by the framework when the value of a vehicle must be computed.
      *
      * @param vehicle the vehicle that we are focusing on
      * @param segments the segments that constitute the route.
      *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
      * @return the value associated with the vehicle. This value should only be computed based on the provided segments
      */
    def computeVehicleValueComposed(vehicle: Long,
                                    segments: QList[LogReducedSegment[T]]): Unit

    class NodeAndPreComputes(val node:Long,
                             var precomputes:Array[T] = null){
      override def toString: String = {
        "NodeAndPreComputes(node:" + node + " precomputes:" + (if(precomputes == null) null else precomputes.mkString(",")) + ")"
      }
    }

    private val vehicleToPrecomputes:Array[Array[NodeAndPreComputes]] = Array.fill(v)(null)

    val preComputedVals:Array[VehicleAndPosition] = Array.fill(n)(null)

    def printVehicleToPrecomputes(vehicle:Long): Unit ={
      val precomputes = vehicleToPrecomputes(vehicle)
      println(precomputes.map(_.toString).mkString("\n"))
    }
    override def performPreCompute(vehicle:Long,
                                   routes:IntSequence): Unit ={

      //println("performPreCompute(vehicle:" + vehicle + " v:" + v + " routes:" + routes)

      //identify all nodes
      identifyNodesAndAllocate(routes.explorerAtAnyOccurrence(vehicle),vehicle,0L,preComputedVals)

      if(vehicleToPrecomputes(vehicle).length > 1L) {
        var sequenceOfLevels = decomposeToBitNumbersMSBFirst(vehicleToPrecomputes(vehicle).length)
        //println("length of vehicle :" + vehicleToPrecomputes(vehicle).length)
        //println("sequence of levels: " + sequenceOfLevels)

        var positionInRoute = 0L
        while (sequenceOfLevels.nonEmpty) {
          val currentLevel = sequenceOfLevels.head
          sequenceOfLevels = sequenceOfLevels.tail

          decorateAndAllocate(vehicle, positionInRoute, currentLevel, allocateFirst = true)
          positionInRoute += 1L << currentLevel
        }

        //      require(positionInRoute == (vehicleToPrecomputes(vehicle).length + 1L), positionInRoute + " " + (vehicleToPrecomputes(vehicle).length + 1L))

      }
      //printVehicleToPrecomputes(vehicle:Long)
    }

    private def identifyNodesAndAllocate(e:Option[IntSequenceExplorer],
                                         vehicle:Long,positionInVehicleRoute:Long,
                                         preComputedVals:Array[VehicleAndPosition]): Unit ={
      e match {
        case None =>
          //end
          vehicleToPrecomputes(vehicle) = Array.fill(positionInVehicleRoute+1L)(null)
          vehicleToPrecomputes(vehicle)(positionInVehicleRoute) = new NodeAndPreComputes(vehicle)

        case  Some(x) if x.value < v && x.value != vehicle => ;
          //end
          vehicleToPrecomputes(vehicle) = Array.fill(positionInVehicleRoute+1L)(null)
          vehicleToPrecomputes(vehicle)(positionInVehicleRoute) = new NodeAndPreComputes(vehicle)

        case Some(ex) =>
          preComputedVals(ex.value) = new VehicleAndPosition(vehicle, positionInVehicleRoute, node = ex.value)

          identifyNodesAndAllocate(ex.next, vehicle, positionInVehicleRoute + 1L, preComputedVals)

          vehicleToPrecomputes(vehicle)(positionInVehicleRoute) = new NodeAndPreComputes(ex.value)
      }
    }

    private def decomposeToBitNumbersMSBFirst(x:Long):List[Long] = {
      require(x >= 0L)

      var remaining = x
      var offset = 0L
      var toReturn = List.empty[Long]

      while(remaining != 0L){
        if((remaining & 1L) != 0L) {
          toReturn = offset :: toReturn
          remaining = remaining ^ 1L
        }
        remaining = remaining >> 1L
        offset = offset + 1L
      }
      toReturn
    }

    private def decorateAndAllocate(vehicle:Long,positionInRoute:Long,level:Long,allocateFirst:Boolean){
      //println("decorateAndAllocate(vehicle:" + vehicle + " level:" + level + " positionInRoute:" + positionInRoute)

      if(allocateFirst){
        vehicleToPrecomputes(vehicle)(positionInRoute).precomputes = Array.fill(level+1L)(null.asInstanceOf[T])
      }

      if(level == 0L){
        val precompute = vehicleToPrecomputes(vehicle)(positionInRoute)
        val node = precompute.node

        if(node == vehicle && positionInRoute != 0L){
          precompute.precomputes(0L) = endNodeValue(node)
        }else{
          precompute.precomputes(0L) = nodeValue(node)
        }

      }else{

        val stepSize = 1L << (level-1L)

        decorateAndAllocate(vehicle,positionInRoute,level-1L,allocateFirst = false)
        decorateAndAllocate(vehicle, positionInRoute+stepSize,level-1L,allocateFirst=true)

        vehicleToPrecomputes(vehicle)(positionInRoute).precomputes(level) =
          composeSteps(
            vehicleToPrecomputes(vehicle)(positionInRoute).precomputes(level-1L),
            vehicleToPrecomputes(vehicle)(positionInRoute + stepSize).precomputes(level-1L))
      }
    }

    override def computeVehicleValue(vehicle:Long,
                                     segments:QList[Segment],
                                     routes:IntSequence):Unit = {
      // println("routes:" + routes)
      computeVehicleValueComposed(vehicle, decorateSegments(vehicle, segments))
    }

    def decorateSegments(vehicle:Long,segments:Iterable[Segment]):QList[LogReducedSegment[T]] = {

      segments match{
        case Nil =>
          //back to start; we add a single node (this will seldom be used, actually, since back to start is included in PreComputedSubSequence that was not flipped
          QList(LogReducedPreComputedSubSequenceGiven[T](
            vehicle: Long, vehicle: Long,
            QList(endNodeValue(vehicle))))

        case head :: tail =>
          head match {
            case PreComputedSubSequence
              (startNode: Long, endNode: Long, length) =>
              val startNodeValue = preComputedVals(startNode)
              val endNodeValue = preComputedVals(endNode)
              if(tail.isEmpty
                && startNodeValue.vehicle == vehicle
                && endNodeValue.positionInVehicleRoute == vehicleToPrecomputes(vehicle).length-2L){

                //last one, on the same vehicle as when pre-computation was performed, and nothing was removed until the end of this route
                QList(LogReducedPreComputedSubSequenceLazy[T](
                  startNode: Long, vehicle:Long, //we set vehicle as the real end
                  stepGenerator = () => extractSequenceOfT(
                    startNodeValue.vehicle, startNodeValue.positionInVehicleRoute,
                    vehicleToPrecomputes(vehicle).length-1L, flipped = false)))

              }else {
                QList(LogReducedPreComputedSubSequenceLazy[T](
                  startNode: Long, endNode: Long,
                  stepGenerator = () => extractSequenceOfT(
                    startNodeValue.vehicle, startNodeValue.positionInVehicleRoute,
                    endNodeValue.positionInVehicleRoute, flipped = false)), decorateSegments(vehicle, tail))
              }
            case FlippedPreComputedSubSequence(startNode: Long, endNode: Long, length) =>
              val startNodeValue = preComputedVals(startNode)
              val endNodeValue = preComputedVals(endNode)
              QList(LogReducedFlippedPreComputedSubSequence[T](
                startNode: Long, endNode: Long,
                stepGenerator = () => extractSequenceOfT(
                  startNodeValue.vehicle, startNodeValue.positionInVehicleRoute,
                  endNodeValue.positionInVehicleRoute, flipped = true)), decorateSegments(vehicle, tail))

            case NewNode(node: Long) =>
              QList(LogReducedNewNode[T](node: Long, value = nodeValue(vehicle)), decorateSegments(vehicle, tail))
          }
      }
    }

    def extractSequenceOfT(vehicle:Long,
                           startPositionInRoute:Long,
                           endPositionInRoute:Long,
                           flipped:Boolean):QList[T] = {

      if(flipped){
        extractSequenceOfTUnflippedGoingUp(vehicleToPrecomputes(vehicle),
          startPositionInRoute = endPositionInRoute,
          endPositionInRoute = startPositionInRoute)
      }else{
        extractSequenceOfTUnflippedGoingUp(vehicleToPrecomputes(vehicle),
          startPositionInRoute = startPositionInRoute,
          endPositionInRoute = endPositionInRoute)
      }
    }

    private def extractSequenceOfTUnflippedGoingUp(vehiclePreComputes:Array[NodeAndPreComputes],
                                                   startPositionInRoute:Long,
                                                   endPositionInRoute:Long):QList[T] = {

      if(startPositionInRoute == endPositionInRoute+1L) return null

      val maxLevel = vehiclePreComputes(startPositionInRoute).precomputes.length - 1L
      val levelStep = 1L << maxLevel

      if(startPositionInRoute + levelStep > endPositionInRoute+1L){
        //we need to go down
        extractSequenceOfTUnflippedGoingDown(vehiclePreComputes:Array[NodeAndPreComputes],
          startPositionInRoute:Long,
          endPositionInRoute:Long,
          maxLevel-1L)
      }else{
        //take the step and go up
        QList(
          vehiclePreComputes(startPositionInRoute).precomputes(maxLevel),
          extractSequenceOfTUnflippedGoingUp(vehiclePreComputes:Array[NodeAndPreComputes],
            startPositionInRoute + levelStep,
            endPositionInRoute:Long))
      }
    }

    private def extractSequenceOfTUnflippedGoingDown(vehiclePreComputes:Array[NodeAndPreComputes],
                                                     startPositionInRoute:Long,
                                                     endPositionInRoute:Long,
                                                     maxLevel:Long):QList[T] = {

      if(startPositionInRoute == endPositionInRoute+1L) return null

      val levelStep = 1L << maxLevel

      if(startPositionInRoute + levelStep > endPositionInRoute+1L) {
        //too far, go down further
        extractSequenceOfTUnflippedGoingDown(vehiclePreComputes:Array[NodeAndPreComputes],
          startPositionInRoute:Long,
          endPositionInRoute:Long,
          maxLevel-1L)
      }else{
        //take the step and go down
        QList(
          vehiclePreComputes(startPositionInRoute).precomputes(maxLevel),
          extractSequenceOfTUnflippedGoingDown(vehiclePreComputes:Array[NodeAndPreComputes],
            startPositionInRoute + levelStep,
            endPositionInRoute:Long,
            maxLevel-1L))
      }
    }
  }


  case class VehicleAndPosition(val vehicle:Long,
                                val positionInVehicleRoute:Long,
                                val node:Long)


