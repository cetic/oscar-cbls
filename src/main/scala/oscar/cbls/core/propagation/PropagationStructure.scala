// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.core.propagation

import scala.annotation.tailrec
import oscar.cbls.algo.heap.AggregatedBinaryHeap
import oscar.cbls.algo.rb.RedBlackTreeMap

/** Manages propagation among propagation elements.
  *
  * A propagation structure handles the propagation among a set of propagation elements. The main
  * goal of the propagation is to ensure that when a value changes in the propagation graph, the
  * propagation elements are updated through a unique wave that reaches all the variables at most
  * once.
  *
  * To achive this goal, the propagation works as follows:
  *
  *   - When all the elements are registered, they are ordered according to the distance from the
  *     input (the propagation elements that depend on nothing). This ordering is achieve in the
  *     [[setupPropagationStructure]] method
  *   - When the propagation is triggered, the elements are updated following the order computed
  *     previously
  *
  * OscaR.cbls supports partial propagation. When a propagation element is registered for partial
  * propagation, the propagation structure will compute the elements on which this elements depends.
  * When a propagation is triggered, only this elements will be updated.
  *
  * @param debugLevel
  *   the level of debug
  */
class PropagationStructure(debugLevel: Int) {

  private var currentId: Int = -1

  private[propagation] def generateId(): Int = {
    currentId +=1 
    currentId
  }

  private var closed = false

  private var executionQueue: AggregatedBinaryHeap[PropagationElement] = null

  private var propagationElements: List[PropagationElement] = List()

  private var partialPropagationTargets: List[PropagationElement] = List()

  private var partialPropagationTracks: RedBlackTreeMap[Array[Boolean]] = RedBlackTreeMap.empty

  /** Prepares the propagation structure for the use of propagation.
    *
    *   - Compute the layer of each propagation element to compute the order of element update
    *   - Compute the tracks for the partial propagation
    *
    * @param dropStaticGraph
    */
  protected def setupPropagationStructure(dropStaticGraph: Boolean): Unit = {
    // Computing the layer of the propagation elements
    val nbLayer = computePropagationElementsLayers()
    executionQueue = AggregatedBinaryHeap[PropagationElement](p => p.layer, nbLayer)

    // Computing the tracks for partial propagation
    computePartialPropagationTrack()

    closed = true
  }

  private def computePartialPropagationTrack(): Unit = {
    for (pe <- partialPropagationTargets) {
      val track = buildTrackForTarget(pe)
      partialPropagationTracks = partialPropagationTracks.insert(pe.id, track)
    }
  }

  private def buildTrackForTarget(pe: PropagationElement): Array[Boolean] = {
    @tailrec
    def buildTrackForTargetRec(
      toTreat: List[PropagationElement],
      track: Array[Boolean]
    ): Array[Boolean] = {
      toTreat match {
        case Nil => track
        case h :: t =>
          track(h.id) = true
          val newToTreat = toTreat ::: h.staticallyListenedElements.filter(pe => !track(pe.id))
          buildTrackForTargetRec(newToTreat, track)
      }
    }
    buildTrackForTargetRec(List(pe), Array.fill(currentId)(false))
  }

  /** Compute the propagation layer for the propagation elements of this propagation structure
    *
    * The layer of a propagation elements corresponds to the distance of this element to the inputs
    * of the propagation graph (i.e. the elements that have no predessors)
    * @return
    *   the maximum layer of the graph
    */
  private def computePropagationElementsLayers(): Int = {
    val nbPropagationElements                      = currentId + 1
    val nbListeningElementPerPC                    = Array.fill(nbPropagationElements)(0)
    var fstLayerElements: List[PropagationElement] = List()
    for (p <- propagationElements) {
      val nbListenedElements = p.staticallyListenedElements.size
      nbListeningElementPerPC(p.id) = nbListenedElements
      if (nbListenedElements == 0)
        fstLayerElements = p :: fstLayerElements
    }

    @tailrec
    def computeLayerOfElement(
      onGoingLayer: List[PropagationElement],
      nextLayer: List[PropagationElement],
      currentLayerId: Int,
      nbElementsLeft: Int
    ): Int = {
      require(
        nbElementsLeft > 0,
        "Problem with Layer counting algorithm (the propagation graph seems to have a cycle which is forbidden)"
      )
      onGoingLayer match {
        case Nil =>
          if (nextLayer.nonEmpty) {
            computeLayerOfElement(nextLayer, List(), currentLayerId + 1, nbElementsLeft)
          } else {
            require(nbElementsLeft == 0, "All the elements have not been treated")
            currentLayerId
          }
        case currentElement :: otherElements =>
          currentElement.layer = currentLayerId
          var newNextLayer: List[PropagationElement] = nextLayer
          for (p <- currentElement.staticallyListeningElements) {
            nbListeningElementPerPC(p.id) = nbListeningElementPerPC(p.id) - 1
            if (nbListeningElementPerPC(p.id) == 0)
              newNextLayer = p :: newNextLayer
          }
          computeLayerOfElement(otherElements, newNextLayer, currentLayerId, nbElementsLeft - 1)
      }
    }

    computeLayerOfElement(fstLayerElements, List(), 0, nbPropagationElements)

  }

  /** Register a propagation element for partial propagation
    *
    * When an element is scheduled for partial propagation, the propagation structure computes the
    * other elements on which this elements depends. Later, when a propapation up to this element is
    * required, only the elements on which this element depends will be propagated
    *
    * @param p
    *   The element that is registered for partial propagation
    */
  protected def registerForPartialPropagation(p: PropagationElement): Unit = {
    partialPropagationTargets = p :: partialPropagationTargets
    if (closed) {
      println(
"""Warning: You should not register a variable for partial propagation after model is closed.
         this might cause the model to crash if static graph was dropped on model close.
         To avoid this, create all your objective functions before model close.
         Note: there might be some implicit conversions related to the use of search strategies.""")
      computePartialPropagationTrack()
    }

  }

  /** Triggers the propagation in the graph.
    *
    * The propagation has a target and stops when the target of the propagation has bee reached
    *
    * @param upTo
    *   The target element of the propagation
    */
    protected final def propagate(upTo: PropagationElement): Unit = {
    }

  /** Schedules a propagation elements for the propagation
    *
    * @param p
    *   the element to schedule
    */
  private[propagation] def scheduleForPropagation(p: PropagationElement): Unit = ???

}
