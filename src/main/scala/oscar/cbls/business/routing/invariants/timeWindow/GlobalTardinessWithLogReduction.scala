package oscar.cbls.business.routing.invariants.timeWindow

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.invariants.global.{LogReducedFlippedPreComputedSubSequence, LogReducedGlobalConstraint, LogReducedNewNode, LogReducedPreComputedSubSequence, LogReducedSegment}
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue}

import scala.annotation.tailrec

object GlobalTardinessWithLogReduction{
  def apply(routes: ChangingSeqValue, n: Int, v: Int,
            globalEarlyLine: Long,
            singleNodeTardinessTransferFunctions: Array[TardinessTransferFunction],
            delayMatrix: Array[Array[Long]],
            tardinessPerVehicle: Array[CBLSIntVar],
            endTimePerVehicle: Array[CBLSIntVar]): GlobalTardinessWithLogReduction = {
    new GlobalTardinessWithLogReduction(routes, n, v,
      globalEarlyLine, singleNodeTardinessTransferFunctions,
      delayMatrix, tardinessPerVehicle, endTimePerVehicle)
  }
}

class GlobalTardinessWithLogReduction(routes: ChangingSeqValue,
                                      n: Int, v: Int,
                                      globalEarlyLine: Long,
                                      singleNodeTardinessTransferFunctions: Array[TardinessTransferFunction],
                                      delayMatrix: Array[Array[Long]],
                                      tardinessPerVehicle: Array[CBLSIntVar],
                                      endTimePerVehicle: Array[CBLSIntVar]) extends LogReducedGlobalConstraint[TwoWaysTardinessTransferFunction, (Long, Long)](routes, n, v) {
  private val twoWaysSingleNodeTardinessTransferFunctions: Array[TwoWaysTardinessTransferFunction] =
    singleNodeTardinessTransferFunctions.map(ttf => TwoWaysTardinessTransferFunction(ttf, ttf.flipped()))

  override def nodeValue(node: Int): TwoWaysTardinessTransferFunction = twoWaysSingleNodeTardinessTransferFunctions(node)

  override def endNodeValue(vehicle: Int): TwoWaysTardinessTransferFunction = twoWaysSingleNodeTardinessTransferFunctions(vehicle)

  override def composeSteps(firstStep: TwoWaysTardinessTransferFunction, secondStep: TwoWaysTardinessTransferFunction): TwoWaysTardinessTransferFunction = {
    val delayNonFlipped = delayMatrix(firstStep.to(false))(secondStep.from(false))
    val nonFlippedTardinessTransferFunction = composeTardinessTransferFunction(firstStep.nonFlippedTF,secondStep.nonFlippedTF,delayNonFlipped)
    val delayFlipped = delayMatrix(secondStep.from(false))(firstStep.to(false))
    val flippedTardinessTransferFunction = composeTardinessTransferFunction(secondStep.flippedTF,firstStep.flippedTF,delayFlipped)
    TwoWaysTardinessTransferFunction(nonFlippedTardinessTransferFunction, flippedTardinessTransferFunction)
  }

  override def computeVehicleValueComposed(vehicle: Int, segments: QList[LogReducedSegment[TwoWaysTardinessTransferFunction]]): (Long, Long) = {
    @tailrec
    def composeTransferFunctions(tardinessTransferFunctions: QList[TwoWaysTardinessTransferFunction],
                                 lastTardinessTransferFunction: TardinessTransferFunction,
                                 flipped: Boolean): TardinessTransferFunction = {
      if(lastTardinessTransferFunction == null)
        composeTransferFunctions(tardinessTransferFunctions.tail, tardinessTransferFunctions.head(flipped), flipped)
      else if(tardinessTransferFunctions == null)
        lastTardinessTransferFunction
      else {
        val currentTardinessTransferFunction = tardinessTransferFunctions.head(flipped)
        val newTardinessTransferFunction = computeTransferFunction(lastTardinessTransferFunction, currentTardinessTransferFunction)
        val tail = tardinessTransferFunctions.tail
        composeTransferFunctions(tail, newTardinessTransferFunction, flipped)
      }
    }

    @tailrec
    def composeLogReduceSegments(logReducedSegments: QList[LogReducedSegment[TwoWaysTardinessTransferFunction]],
                                 previousTardinessTransferFunction: TardinessTransferFunction = null): TardinessTransferFunction ={
      if(logReducedSegments == null) previousTardinessTransferFunction
      else {
        val computedTardinessTransferFunction: TardinessTransferFunction = logReducedSegments.head match {
          case s@LogReducedPreComputedSubSequence(_, endNode, steps) =>
            composeTransferFunctions(steps, null, false)

          case s@LogReducedFlippedPreComputedSubSequence(_, endNode, steps) =>
            composeTransferFunctions(steps.reverse, null, true)

          case s@LogReducedNewNode(node, transferFunctionOfNode) =>
            composeTransferFunctions(QList(transferFunctionOfNode), null, false)

          case x =>
            throw new Error(s"Unhandled match with $x")
        }
        val newTardinessTransferFunction = {
          if(previousTardinessTransferFunction == null) computedTardinessTransferFunction
          else computeTransferFunction(previousTardinessTransferFunction, computedTardinessTransferFunction)
        }
        composeLogReduceSegments(logReducedSegments.tail, newTardinessTransferFunction)
      }
    }
    val globalTardinessTransferFunction = composeLogReduceSegments(segments)
    (globalTardinessTransferFunction.tardiness(0L),
      globalTardinessTransferFunction.end(0L))
  }

  override protected def assignVehicleValue(vehicle: Int, value: (Long, Long)): Unit = {
    tardinessPerVehicle(vehicle) := value._1
    endTimePerVehicle(vehicle) := value._2
  }

  override protected def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): (Long, Long) = {
    var totalTardiness = 0L
    var endTime = 0L
    var currentExplorer = routes.explorerAtAnyOccurrence(vehicle).get.next
    var continue = currentExplorer.isDefined && currentExplorer.get.value >= v
    var previousNode = vehicle
    while(continue){
      val delay = delayMatrix(previousNode)(currentExplorer.get.value)
      val arrival = endTime + delay
      val tardiness = singleNodeTardinessTransferFunctions(currentExplorer.get.value).tardiness(arrival)
      val leaveTime = singleNodeTardinessTransferFunctions(currentExplorer.get.value).end(arrival)
      totalTardiness += tardiness
      endTime = leaveTime

      previousNode = currentExplorer.get.value
      // If this is the last node of the vehicle we stop here
      if(currentExplorer.get.next.nonEmpty && currentExplorer.get.next.get.value >= v) {
        currentExplorer = currentExplorer.get.next
      } else {
        continue = false
      }
    }
    val delayToDepot = delayMatrix(previousNode)(vehicle)
    val arrivalAtDepot = endTime+delayToDepot
    val tardinessAtDepot = singleNodeTardinessTransferFunctions(vehicle).tardiness(arrivalAtDepot)
    val endTimeAtDepot = singleNodeTardinessTransferFunctions(vehicle).end(arrivalAtDepot)
    totalTardiness += tardinessAtDepot
    endTime = endTimeAtDepot

    (totalTardiness,endTime)
  }

  private def computeTransferFunction(first: TardinessTransferFunction, second: TardinessTransferFunction): TardinessTransferFunction ={
    val delay = delayMatrix(first.to)(second.from)
    composeTardinessTransferFunction(first, second, delay)
  }

  private def composeTardinessTransferFunction(first: TardinessTransferFunction, second: TardinessTransferFunction, delay: Long): TardinessTransferFunction ={
    (first,second) match {
      case (f1: TardinessTransferFunctionWithoutEarlyLine, f2: TardinessTransferFunctionWithoutEarlyLine) =>
        val Ar = f1.A + f2.A
        val Br = f1.B + f2.B + f2.A*(f1.C + delay)
        val Cr = f1.C + f2.C + delay
        TardinessTransferFunctionWithoutEarlyLine(first.from, second.to, Ar, Br, Cr)
      case (f1: TardinessTransferFunctionWithoutEarlyLine, g2: TardinessTransferFunctionWithEarlyLine) =>
        val Ar = f1.A + g2.A
        val Br = f1.B + g2.A*(f1.C + delay) + g2.B
        val Cr = g2.C
        val Dr = f1.C + g2.D + delay
        val Fr = g2.F
        TardinessTransferFunctionWithEarlyLine(first.from, second.to, globalEarlyLine, Ar, Br, Cr, Dr, Fr)
      case (g1: TardinessTransferFunctionWithEarlyLine, f2: TardinessTransferFunctionWithoutEarlyLine) =>
        val Ar = g1.A
        val Br = f2.A*(g1.F + delay) + g1.B + f2.B
        val Cr = g1.C + f2.A
        val Dr = g1.D
        val Fr = g1.F + delay + f2.C
        TardinessTransferFunctionWithEarlyLine(first.from, second.to, globalEarlyLine, Ar, Br, Cr, Dr, Fr)
      case (g1: TardinessTransferFunctionWithEarlyLine, g2: TardinessTransferFunctionWithEarlyLine) =>
        val Ar = g1.A
        val Br = g1.B + g2.B + (g2.A + g2.C)*(g1.F + delay) + (g2.C * g2.D)
        val Cr = g2.A + g1.C + g2.C
        val Dr = g1.D
        val Fr = g1.F + g2.F + delay + g2.D
        TardinessTransferFunctionWithEarlyLine(first.from, second.to, globalEarlyLine, Ar, Br, Cr, Dr, Fr)
    }
  }
}
