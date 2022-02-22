package oscar.cbls.business.routing.invariants.timeWindow

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.business.routing.invariants.global.GlobalConstraintCore
import oscar.cbls.business.routing.invariants.segments.Segment
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue}

import scala.annotation.tailrec

object GlobalTardiness {
  def apply(routes: ChangingSeqValue, n: Int, v: Int,
            globalEarlyLine: Long,
            singleNodeTardinessTransferFunctions: Array[TardinessTransferFunction],
            delayMatrix: Array[Array[Long]],
            tardinessPerVehicle: Array[CBLSIntVar],
            endTimePerVehicle: Array[CBLSIntVar]): GlobalTardiness = {
    new GlobalTardiness(routes, n, v,
      globalEarlyLine, singleNodeTardinessTransferFunctions,
      delayMatrix, tardinessPerVehicle, endTimePerVehicle)
  }
}

/**
 * This constraint purpose is to compute and maintain the cumulated tardiness on a vehicle.
 *
 * @param routes The routes of the problem
 * @param n The number of nodes (and vehicles)
 * @param v The number of vehicles
 * @param globalEarlyLine The early line that's used for all TardinessTransferFunctionWithEarlyLine
 * @param singleNodeTardinessTransferFunctions A list of TardinessTransferFunction, some with early line some without
 * @param delayMatrix The matrices that contains the delay between each pair of points,
 *                    meaning the estimated time between two arrivals.
 *                    Ex : A -> B == task duration at A + travel duration between A and B
 * @param tardinessPerVehicle The cumulated tardiness per vehicle (sum of all tardiness)
 * @param endTimePerVehicle The end time of each vehicle back at depot
 */
class GlobalTardiness(routes: ChangingSeqValue,
                      n: Int, v: Int,
                      globalEarlyLine: Long,
                      singleNodeTardinessTransferFunctions: Array[TardinessTransferFunction],
                      delayMatrix: Array[Array[Long]],
                      tardinessPerVehicle: Array[CBLSIntVar],
                      endTimePerVehicle: Array[CBLSIntVar]) extends GlobalConstraintCore[(Long,Long)](routes,v) {

  tardinessPerVehicle.foreach(_.setDefiningInvariant(this))
  endTimePerVehicle.foreach(_.setDefiningInvariant(this))

  val preComputedValues: Array[Array[TardinessTransferFunction]] =
    Array.tabulate(n)(from => Array.tabulate(n)(to =>
      if(from == to) singleNodeTardinessTransferFunctions(from) else null))

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

  override protected def assignVehicleValue(vehicle: Int, value: (Long,Long)): Unit = {
    tardinessPerVehicle(vehicle) := value._1
    endTimePerVehicle(vehicle) := value._2
  }

  override protected def performPreCompute(vehicle: Int, routes: IntSequence): Unit = {
    @tailrec
    def performPreComputeForNode(node: Int, prevNode: Int, route: QList[Int], lastTF: TardinessTransferFunction): Unit ={
      if(route != null) {
        val curNode = route.head
        val newTF = composeTardinessTransferFunction(lastTF, singleNodeTardinessTransferFunctions(curNode), delayMatrix(prevNode)(curNode))
        preComputedValues(node)(curNode) = newTF
        performPreComputeForNode(node, curNode, route.tail, newTF)
      }
    }

    @tailrec
    def performPreComputeOnRoute(route: QList[Int]): Unit ={
      val node = route.head
      preComputedValues(node)(node) = singleNodeTardinessTransferFunctions(node)
      val lastTF = preComputedValues(node)(node)
      val prevNode = node
      performPreComputeForNode(node, prevNode, route.tail, lastTF)
      if(route.tail != null)
        performPreComputeOnRoute(route.tail)
    }

    var continue = true
    var vExplorer = routes.explorerAtAnyOccurrence(vehicle).get.next
    var route: QList[Int] = QList(vehicle)
    while(continue){
      vExplorer match {
        case None => continue = false
        case Some(elem) =>
          if (elem.value < v && elem.value != vehicle){
            continue = false
          } else {
            route = QList(elem.value, route)
          }
          vExplorer = elem.next
      }
    }
    performPreComputeOnRoute(route)
    performPreComputeOnRoute(route.reverse)
  }

  override protected def computeVehicleValue(vehicle: Int, segments: QList[Segment], routes: IntSequence): (Long,Long) = {
    val firstSegment = segments.head
    val firstTardinessTransferFunction = preComputedValues(firstSegment.startNode())(firstSegment.endNode())
    var lastNode = firstSegment.endNode()
    var composedVehicleTardinessTransferFunction = {
      QList.qFold[Segment, TardinessTransferFunction](segments.tail,
        (acc, item) => {
          val nextTardinessTransferFunction = preComputedValues(item.startNode())(item.endNode())
          val delay = delayMatrix(lastNode)(item.startNode())
          lastNode = item.endNode()
          composeTardinessTransferFunction(acc, nextTardinessTransferFunction, delay)
        },
        firstTardinessTransferFunction
      )
    }
    val delayToDepot = delayMatrix(lastNode)(vehicle)
    composedVehicleTardinessTransferFunction = composeTardinessTransferFunction(composedVehicleTardinessTransferFunction, singleNodeTardinessTransferFunctions(vehicle),delayToDepot)
    (composedVehicleTardinessTransferFunction.tardiness(0L),
      composedVehicleTardinessTransferFunction.end(0L))
  }

  override protected def computeVehicleValueFromScratch(vehicle: Int, routes: IntSequence): (Long,Long) = {
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

  def arrivalAndStartTimesPerVehicle(finalResult: Boolean): Array[(List[Long], List[Long])] = {
    val routesNow = routes.value
    if(finalResult)
      for(vehicle <- 0 until v) performPreCompute(vehicle, routesNow)

    Array.tabulate(v)(vehicle => {
      var arrivals: List[Long] = List.empty
      var starts: List[Long] = List.empty
      var prevNode: Int = vehicle
      var prevExplorer: IntSequenceExplorer = routesNow.explorerAtAnyOccurrence(prevNode).get
      var continue = prevExplorer.next.isDefined && prevExplorer.next.get.value >= v
      while(continue){
        val currentExplorer = prevExplorer.next.get
        val currentNode = currentExplorer.value
        val delaySincePrevNode = delayMatrix(prevNode)(currentNode)
        val prevTtf = preComputedValues(vehicle)(prevNode)
        arrivals :+= prevTtf.end(0L) + delaySincePrevNode
        singleNodeTardinessTransferFunctions(currentNode) match {
          case _: TardinessTransferFunctionWithEarlyLine =>
            starts :+= Math.max(arrivals.last, globalEarlyLine)
          case _: TardinessTransferFunctionWithoutEarlyLine =>
            starts :+= arrivals.last
        }
        prevNode = currentNode
        prevExplorer = currentExplorer
        continue = prevExplorer.next.isDefined && prevExplorer.next.get.value >= v
      }
      (arrivals, starts)
    })
  }
}

abstract class TardinessTransferFunction(val from: Int, val to: Int){
  def tardiness(startTime: Long): Long
  def end(startTime: Long): Long
  def flipped(): TardinessTransferFunction
}

object TardinessTransferFunction{

  def createTardinessTransferFunctionWithoutEarlyline(from: Int, to: Int, tardinessFactor: Int): TardinessTransferFunctionWithoutEarlyLine =
    TardinessTransferFunctionWithoutEarlyLine(from, to, tardinessFactor)

  def createTardinessTransferFunctionWithEarlyline(from: Int, to: Int, earlyLine: Int): TardinessTransferFunctionWithEarlyLine =
    TardinessTransferFunctionWithEarlyLine(from, to, earlyLine)

}

case class TardinessTransferFunctionWithoutEarlyLine(override val from: Int, override val to: Int, A: Long, B: Long = 0, C: Long = 0) extends TardinessTransferFunction(from,to) {
  override def tardiness(startTime: Long): Long = A*startTime + B
  override def end(startTime: Long): Long = startTime + C
  override def flipped(): TardinessTransferFunction = TardinessTransferFunctionWithoutEarlyLine(to,from,A,B,C)
}

case class TardinessTransferFunctionWithEarlyLine(override val from: Int, override val to: Int, e: Long, A: Long = 0, B: Long = 0, C: Long = 0, D: Long = 0, F: Long = 0) extends TardinessTransferFunction(from,to){
  override def tardiness(startTime: Long): Long = A*startTime + B + C*Math.max(startTime + D, e)
  override def end(startTime: Long): Long = Math.max(startTime + D, e) + F
  override def flipped(): TardinessTransferFunction = TardinessTransferFunctionWithEarlyLine(to,from,e,A,B,C,D,F)
}

case class TwoWaysTardinessTransferFunction(nonFlippedTF: TardinessTransferFunction, flippedTF: TardinessTransferFunction){

  def apply(flipped: Boolean): TardinessTransferFunction ={
    if(flipped)flippedTF
    else nonFlippedTF
  }

  def from(flipped: Boolean): Int ={
    if(flipped)flippedTF.from
    else nonFlippedTF.from
  }

  def to(flipped: Boolean): Int ={
    if(flipped)flippedTF.to
    else nonFlippedTF.to
  }

  def tardiness(startTime: Long, flipped: Boolean): Long ={
    if(flipped) flippedTF.tardiness(startTime)
    else nonFlippedTF.tardiness(startTime)
  }

  def end(startTime: Long, flipped: Boolean): Long ={
    if(flipped) flippedTF.end(startTime)
    else nonFlippedTF.end(startTime)
  }

  override def toString: String =
    s"""Two ways transfert function :
       |Non-flipped : $nonFlippedTF
       |Flipped : $flippedTF""".stripMargin
}