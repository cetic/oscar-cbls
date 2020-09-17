/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer and Florent Ghilain.
 *     Factorization of code by Yoann Guyot.
 * ****************************************************************************
 */
package oscar.cbls.business.routing.neighborhood

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior}

/**
 * Moves a point of a route to another place in the same or in an other route.
 * The search complexity is O(nk).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class OnePointMove(nodesToMove: () => Iterable[Int],
                        relevantNewPredecessors: () => Int => Iterable[Int],
                        vrp:VRP,
                        neighborhoodName: String = "OnePointMove",
                        selectPointToMoveBehavior:LoopBehavior = First(),
                        selectDestinationBehavior:LoopBehavior = First(),
                        hotRestart: Boolean = true,
                        allPointsToMoveAreRouted:Boolean = true,
                        allRelevantNeighborsAreRouted:Boolean = true,
                        positionIndependentMoves:Boolean = false)
  extends EasyNeighborhoodMultiLevel[OnePointMoveMove](neighborhoodName) {

  val seq = vrp.routes
  val v = vrp.v
  val n = vrp.n

  //the indice to start with for the exploration
  var startIndice: Int = 0

  override def exploreNeighborhood(initialObj: Long): Unit ={
    val iterationSchemeOnZone =
      if (hotRestart) HotRestart(nodesToMove(), startIndice)
      else nodesToMove()

    //TODO: we might also want to go for checkpoint less neighborhood to avoid invariants computing checkpoint values for small neighborhoods.
    val startValue = seq.defineCurrentValueAsCheckpoint(true)

    def evalObjAndRollBack() : Long = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(startValue)
      a
    }

    val relevantNeighborsNow = relevantNewPredecessors()

    val (movedPointsIt,notifyFound1) = selectPointToMoveBehavior.toIterator(iterationSchemeOnZone)
    while (movedPointsIt.hasNext) {
      movedPointForInstantiation = movedPointsIt.next()

      if(!vrp.isADepot(movedPointForInstantiation)) {
        //depots cannot be moved at all.
        startValue.positionOfAnyOccurrence(movedPointForInstantiation) match {
          case None => ;//was not routed, actually
          case Some(positionOfMovedPoint) =>
            this.positionOfMovedPointForInstantiation = positionOfMovedPoint

            val (insertionPointIt,notifyFound2) = selectDestinationBehavior.toIterator(relevantNeighborsNow(movedPointForInstantiation))
            while (insertionPointIt.hasNext) {
              newPredecessorForInstantiation = insertionPointIt.next()
              if (movedPointForInstantiation != newPredecessorForInstantiation) {

                startValue.positionOfAnyOccurrence(newPredecessorForInstantiation) match {
                  case None => ;
                  case Some(positionOfNewPredecessor) =>
                    if(positionOfNewPredecessor+1L != positionOfMovedPoint) {
                      this.positionOfNewPredecessorForInstantiation = positionOfNewPredecessor

                      doMove(positionOfMovedPoint, positionOfNewPredecessor)

                      if(evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
                        notifyFound1()
                        notifyFound2()
                      }
                    }
                }
              }
            }
        }
      }
    }
    seq.releaseTopCheckpoint()
    startIndice = movedPointForInstantiation + 1
  }
  var movedPointForInstantiation:Int = -1
  var newPredecessorForInstantiation:Int = -1
  var positionOfMovedPointForInstantiation:Int = -1
  var positionOfNewPredecessorForInstantiation:Int = -1

  override def instantiateCurrentMove(newObj: Long): OnePointMoveMove =
    OnePointMoveMove(movedPointForInstantiation, positionOfMovedPointForInstantiation,
      newPredecessorForInstantiation, positionOfNewPredecessorForInstantiation,
      positionIndependentMoves,
      newObj, this, neighborhoodName)

  override def reset(): Unit = {
    startIndice = 0
  }

  def doMove(positionOfMovedPoint:Int, positionOfNewPredecessor:Int): Unit = {
    seq.move(positionOfMovedPoint,positionOfMovedPoint,positionOfNewPredecessor,false)
  }

  def doPositionIndependentMove(movedPoint:Int, newPredecessor:Int): Unit = {
    val s = seq.newValue
    val movedPos = s.positionOfAnyOccurrence(movedPoint).get
    val positionOfPredecessor = s.positionOfAnyOccurrence(newPredecessor).get
    seq.move(movedPos,movedPos,positionOfPredecessor,false)
  }
}

/**
 * Models a one-point-move operator of a given VRP problem.
 * @param newPredecessor the place where insert the moving point.
 * @param objAfter the objective value if we performed this one-point-move operator.
 * @param neighborhood the originating neighborhood
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class OnePointMoveMove(movedPoint: Int,movedPointPosition:Int,
                            newPredecessor: Int,newPredecessorPosition:Int,
                            positionIndependentMoves:Boolean,
                            override val objAfter: Long,
                            override val neighborhood: OnePointMove,
                            override val neighborhoodName: String = "OnePointMoveMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName, neighborhood.vrp){

  override def impactedPoints: Iterable[Int] = List(movedPoint,newPredecessor)

  override def commit(): Unit = {
    if(positionIndependentMoves){
      neighborhood.doPositionIndependentMove(movedPoint, newPredecessor)
    }else{
      neighborhood.doMove(movedPointPosition, newPredecessorPosition)
    }
  }

  override def toString: String = (
    s"${neighborhoodNameToString}OnePointMove($movedPoint afterPoint $newPredecessor${if (positionIndependentMoves) " positionIndependent" else ""}$objToString)")

  override def shortString:String =
    s"OnePointMove($movedPoint after $newPredecessor${if (positionIndependentMoves) " pi" else ""})"
}
