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
  * The propagation structure has 4 levels of debugs.
  *   - 0: No debug at all
  *   - 1: The method checkInternals of PropagationElement is called after the element has been
  *     propagated if the propagation is total
  *   - 2: The method checkInternals of PropagationElement is called after the element has been
  *     propagated in a total or partial propagation
  *   - 3: Partial propagation is disabled (every propagation is a total propagation) and the method
  *     checkInternals of PropagationElement is called after the element has been propagated
  *
  * @param debugLevel
  *   the level of debug
  */
class PropagationStructure(debugLevel: Int) {

  require(debugLevel <= 3,"Debug level cannot be higher that 3")
  require(debugLevel >= 0,"Debug level cannot be lower than 0")

  private var currentId: Int = -1

  private[propagation] def registerAndGenerateId(p: PropagationElement): Int = {
    propagationElements = p :: propagationElements
    currentId += 1
    currentId
  }

  protected var closed: Boolean = false

  private var propagating: Boolean = false

  private var scheduledElements: List[PropagationElement] = List()

  private var postponedElements: List[PropagationElement] = List()

  private var executionQueue: AggregatedBinaryHeap[PropagationElement] = null

  private var propagationElements: List[PropagationElement] = List()

  private var partialPropagationTargets: List[PropagationElement] = List()

  private var partialPropagationTracks: RedBlackTreeMap[Array[Boolean]] = RedBlackTreeMap.empty

  private var currentTargetIdForPartialPropagation: Option[Int] = None

  protected def getPropagationElements: List[PropagationElement] = propagationElements

  private def nbPropagationElements = currentId + 1

  /** Prepares the propagation structure for the use of propagation.
    *
    *   - Compute the layer of each propagation element to compute the order of element update
    *   - Compute the tracks for the partial propagation
    *
    * @param dropStaticGraph
    */
  protected def setupPropagationStructure(): Unit = {
    // Computing the layer of the propagation elements
    val nbLayer = computePropagationElementsLayers()
    executionQueue = AggregatedBinaryHeap[PropagationElement](p => p.layer, nbLayer + 1)

    // Computing the tracks for partial propagation
    if (debugLevel < 3)
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
          val newToTreat = t ::: h.staticallyListenedElements.filter(pe => !track(pe.id))
          buildTrackForTargetRec(newToTreat, track)
      }
    }
    buildTrackForTargetRec(List(pe), Array.fill(nbPropagationElements)(false))
  }

  /** Compute the propagation layer for the propagation elements of this propagation structure
    *
    * The layer of a propagation elements corresponds to the distance of this element to the inputs
    * of the propagation graph (i.e. the elements that have no predessors)
    * @return
    *   the maximum layer of the graph
    */
  private def computePropagationElementsLayers(): Int = {
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
        nbElementsLeft >= 0,
        "Problem with Layer counting algorithm (the propagation graph seems to have a cycle which is forbidden)"
      )
      onGoingLayer match {
        case Nil =>
          if (nextLayer.nonEmpty) {
            computeLayerOfElement(nextLayer, List(), currentLayerId + 1, nbElementsLeft)
          } else {
            require(
              nbElementsLeft == 0,
              "All the elements have not been treated (there shall be a cycle on the propagation graph)"
            )
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
  def registerForPartialPropagation(p: PropagationElement): Unit = {
    if (debugLevel < 3) {
      partialPropagationTargets = p :: partialPropagationTargets
      if (closed) {
        println(
          """Warning: You should not register a variable for partial propagation after model is closed.
         this might cause the model to crash if static graph was dropped on model close.
         To avoid this, create all your objective functions before model close.
         Note: there might be some implicit conversions related to the use of search strategies."""
        )
        computePartialPropagationTrack()
      }
    }
  }

  /** Triggers the propagation in the graph.
    *
    * The propagation has a target and stops when the target of the propagation has bee reached
    *
    * @param target
    *   The target element of the propagation
    */
  protected final def propagate(target: PropagationElement = null): Unit = {
    var currentLayer = 0

    val check: Boolean = (debugLevel >= 1 && (target == null || debugLevel >= 2))

    val theTrack = if (target == null) null else partialPropagationTracks.getOrElse(target.id, null)

    @inline
    def track(id: Int) = if (theTrack == null) true else theTrack(id)

    @tailrec @inline
    def filterScheduledWithTrack: Unit = {
      scheduledElements match {
        case Nil =>
        case h :: t =>
          scheduledElements = t
          if (track(h.id))
            executionQueue.insert(h)
          else
            postponedElements = h :: postponedElements
          filterScheduledWithTrack
      }
    }

    @tailrec @inline
    def filterAndEnqueuePostponedElements(
      postponed: List[PropagationElement],
      newPostponed: List[PropagationElement] = Nil
    ): List[PropagationElement] = {
      postponed match {
        case Nil => newPostponed
        case h :: t =>
          val newList =
            if (track(h.id)) {
              executionQueue.insert(h)
              newPostponed
            } else {
              h :: newPostponed
            }
          filterAndEnqueuePostponedElements(t, newList)
      }
    }

    @tailrec @inline
    def doPropagation(): Unit = {
      if (executionQueue.nonEmpty) {
        val currentElement = executionQueue.popFirst().get
        currentElement.propagateElement
        if (check)
          currentElement.checkInternals()
        filterScheduledWithTrack
        doPropagation()
      }
    }

    if (!propagating) {
      propagating = true
      val sameTarget: Boolean = currentTargetIdForPartialPropagation match {
        case None     => false
        case Some(id) => id == target.id
      }
      if (sameTarget) {
        filterScheduledWithTrack
      } else {
        postponedElements = filterAndEnqueuePostponedElements(postponedElements)
        filterScheduledWithTrack
      }
      doPropagation()
      propagating = false

    }
  }

  /** Schedules a propagation elements for the propagation
    *
    * @param p
    *   the element to schedule
    */
  private[propagation] def scheduleElementForPropagation(p: PropagationElement): Unit = {
    scheduledElements = p :: scheduledElements
  }

  /** Computes the list of path in the dependency graph of the propagation structure
    *
    * A path is a list of integer that means that there is a path along this list of element. The
    * list of path is a list of this so called path such that giving all this path suffices to
    * describe the structure. This method is mainly used to make the dot file
    *
    * @return
    *   The list of path
    */
  protected def pathList: List[List[Int]] = {
    def developNode(
      currentNode: PropagationElement,
      alreadyVisitedNodes: List[Int]
    ): (List[List[Int]], List[Int]) = {
      if (alreadyVisitedNodes.contains(currentNode.id)) {
        (List(List(currentNode.id)), alreadyVisitedNodes)
      } else {
        currentNode.staticallyListeningElements match {
          case Nil => (List(List(currentNode.id)), currentNode.id :: alreadyVisitedNodes)
          case _ :: _ =>
            val resOfSons =
              developListOfNode(currentNode.staticallyListeningElements, alreadyVisitedNodes)
            (
              resOfSons._1
                .map(l => {
                  l match {
                    case Nil    => List()
                    case h :: t => (currentNode.id :: h) :: t
                  }
                })
                .flatten,
              currentNode.id :: resOfSons._2
            )
        }
      }
    }

    def developListOfNode(
      nodes: List[PropagationElement],
      alreadyVisitedNodes: List[Int]
    ): (List[List[List[Int]]], List[Int]) = {
      nodes match {
        case Nil => (List(), alreadyVisitedNodes)
        case h :: t =>
          val resOfRest = developListOfNode(t, alreadyVisitedNodes)
          val resOfNode = developNode(h, resOfRest._2)
          (resOfNode._1 :: resOfRest._1, resOfNode._2)
      }
    }

    developListOfNode(
      propagationElements.filter(_.staticallyListenedElements.isEmpty),
      List()
    )._1.flatten

  }

  /** Produces a string that represents the propagation structure in the dot format
    *
    * @param names
    *   a map that allow to change the names of some vertices
    * @param shapes
    *   a map that allow to change the shape of some vertices
    * @return
    */
  def toDot(
    names: Map[Int, String] =
      propagationElements.map(_.id).zip(propagationElements.map(_.id.toString)).toMap,
    shapes: Map[Int, String] = Map.empty[Int, String]
  ): String = {

    val labelDefinition: List[String] =
      propagationElements
        .map(e => {
          val label = names.getOrElse(e.id, "")
          val shape = shapes.getOrElse(e.id, "")
          if (label == "") {
            if (shape == "")
              None
            else
              Some(s"  ${e.id} [shape = \"$shape\"]")
          } else {
            if (shape == "")
              Some(s"  ${e.id} [label = \"$label\"]")
            else
              Some(s"  ${e.id} [label = \"$label\",shape = \"$shape\"]")
          }
        })
        .flatten
    val developedNodes: Array[Boolean] = Array.fill(propagationElements.length)(false)

    val lines = pathList
    s"""
digraph PropagationStructure {
${labelDefinition.mkString(";\n")};
${lines.map(l => s"  ${l.mkString(" -> ")};").mkString("\n")}
}
"""

  }

}
