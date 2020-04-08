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
  *     This code has been initially developed by Ghilain Florent.
  *     Refactored (in respect with the new architecture) by Yoann Guyot.
  * ****************************************************************************
  */

package oscar.cbls.business.routing.neighborhood

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

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior, EasyNeighborhood}
import oscar.cbls._

/**
  * Removes a point of route.
  * The search complexity is O(n).
  * @param relevantPointsToRemove: the points that we will try to remove; tolerates non routed points
  * @param vrp the routing problem
  * @param neighborhoodName the name of the neighborhood, for verbosities
  * @param selectNodeBehavior how to select node to remove
  * @param hotRestart true if hotRestart is needed, false otherwise
  * @author renaud.delandtsheer@cetic.be
  * @author yoann.guyot@cetic.be
  * @author Florent Ghilain (UMONS)
  */
case class RemovePoint(relevantPointsToRemove:()=>Iterable[Int],
                       vrp: VRP,
                       neighborhoodName:String = "RemovePoint",
                       selectNodeBehavior:LoopBehavior = First(),
                       hotRestart:Boolean = true,
                       positionIndependentMoves:Boolean = false)
  extends EasyNeighborhoodMultiLevel[RemovePointMove](neighborhoodName){

  //the indice to start with for the exploration
  var startIndice: Int = 0

  var pointToRemove:Int = -1
  var positionOfPointToRemove:Int = -1

  val v = vrp.v
  val seq = vrp.routes

  override def exploreNeighborhood(initialObj: Long): Unit = {

    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    def evalObjAndRollBack() : Long = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    val iterationSchemeOnZone =
      if (hotRestart) HotRestart(relevantPointsToRemove(), startIndice)
      else relevantPointsToRemove()

    val (it,notifyFound) = selectNodeBehavior.toIterator(iterationSchemeOnZone)
    while (it.hasNext) {
      pointToRemove = it.next()

      if(pointToRemove >= v){ //otherwise, it is a vehicle start, and we do not try to remove it.
        seq.value.positionOfAnyOccurrence(pointToRemove) match {
          case None => ;
          case Some(p) =>
            positionOfPointToRemove = p
            doMove(positionOfPointToRemove)
            if (evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
              notifyFound()
            }
        }
      }
    }
    seq.releaseTopCheckpoint()
    startIndice = pointToRemove + 1
  }

  override def instantiateCurrentMove(newObj: Long) =
    RemovePointMove(
      positionOfPointToRemove,
      pointToRemove,
      positionIndependentMoves,
      vrp,
      newObj,
      this,
      neighborhoodNameToString)

  def doMove(positionOfPointToRemove: Int) {
    seq.remove(positionOfPointToRemove)
  }

  def doMovePositionIndependent(valueToRemove: Int) {
    val s = seq.newValue
    seq.remove(s.positionOfAnyOccurrence(valueToRemove).get)
  }

  //this resets the internal state of the Neighborhood
  override def reset(){startIndice = 0}
}

/**
  * Models a remove-point operator of a given VRP problem.
  * @param positionOfPointToRemove the predecessor of the point that will be removed.
  * @param objAfter the objective value if we performed this remove-point operator.
  * @param neighborhood the originating neighborhood
  * @author renaud.delandtsheer@cetic.be
  * @author yoann.guyot@cetic.be
  * @author Florent Ghilain (UMONS)
  */
case class RemovePointMove(positionOfPointToRemove: Int,
                           pointToRemove:Int,
                           positionIndependentMoves:Boolean,
                           vrp:VRP,
                           override val objAfter:Long,
                           override val neighborhood:RemovePoint,
                           override val neighborhoodName:String = null)
  extends VRPSMove(objAfter, neighborhood, neighborhoodName, vrp){

  override def impactedPoints: List[Int] = List(pointToRemove)

  override def commit() {
    if(positionIndependentMoves){
      neighborhood.doMovePositionIndependent(pointToRemove)
    }else{
      neighborhood.doMove(positionOfPointToRemove)
    }
  }

  override def toString: String = "RemovePoint(point:" + pointToRemove + objToString + ")"

  override def shortString: String = "RemovePoint(" + pointToRemove + (if (positionIndependentMoves) " pi" else "") + ")"
}
