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

  protected var closed: Boolean = false

  def isClosed: Boolean = closed

  //priority queue is ordered, first on propagation planning list, second on DAG.

  /**
   * This method is to be overridden and is expected to return the propagation elements
   * on which the propagation structure will reason.
   * The method is expected to return consistent result once the setupPropagationStructure method is called
   */
  def getPropagationElements: List[PropagationElement]

  private var currentId: Int = -1

  def generateId(): Int = {
    currentId += 1
    currentId
  }

  /** Prepares the propagation structure for the use of propagation.
    *
    *   - Compute the layer of each propagation element to compute the order of element update
    *   - Compute the tracks for the partial propagation
    */
  protected def setupPropagationStructure(): Unit = {

    val clusteredPropagationComponents: List[PropagationElement] = getPropagationElements.toList

    addFastPropagationTracks()

    //this performs the sort on Propagation Elements that do not belong to a strongly connected component,
    // plus the strongly connected components, considered as a single node. */
    val layerCount = computePositionsThroughDistanceToInput(clusteredPropagationComponents) + 1
    executionQueue = new AggregatedBinaryHeap[PropagationElement](p => p.layer, layerCount)

    propagating = false
    previousPropagationTrack = null

    if (dropStaticGraphAfter) dropStaticGraph()

    //variables are already able to propagate immediately before model close and if not monitored yet.

    scheduledElements = null

    val it = getPropagationElements.iterator
    while (it.hasNext) {
      it.next().rescheduleIfNeeded()
    }

  }

  /**
   * This computes the position of the clustered PE based on distance to input,
   * that is: the SCC and the PE not belonging to an SCC
   * @return the max Position, knowing that the first is zero
   */
  private def computePositionsThroughDistanceToInput(propagationComponents: List[PropagationElement]): Int = {
    val nbPropagationElements = propagationComponents.size
    val nbListeningElementPerPC = Array.fill(nbPropagationElements)(0)
    var fstLayerElements : List[PropagationElement] = List()
    for (p <- propagationComponents) {
      val nbListenedElements = p.staticallyListenedElements.size
      nbListeningElementPerPC(p.id) = nbListenedElements
      if (nbListenedElements == 0)
        fstLayerElements = p :: fstLayerElements
    }

    @tailrec
    def computeLayerOfElement(treatedLayer : List[PropagationElement],nextLayer : List[PropagationElement],currentLayerId : Int,nbElementsLeft : Int) : Int = {
      require(nbElementsLeft > 0,"Problem with Layer counting algorithm (the propagation graph seems to have a cycle which is forbidden)")
      treatedLayer match {
        case Nil =>
          if (nextLayer.nonEmpty) {
            computeLayerOfElement(nextLayer,List(),currentLayerId + 1,nbElementsLeft)
          } else {
            require(nbElementsLeft == 0,"All the elements have not been treated")
            currentLayerId
          }
        case currentElement :: otherElements =>
          currentElement.layer = currentLayerId
          var newNextLayer : List[PropagationElement] = List()
          for (p <- currentElement.staticallyListeningElements) {
            nbListeningElementPerPC(p.id) = nbListeningElementPerPC(p.id) - 1
            if (nbListeningElementPerPC(p.id) == 0)
              newNextLayer = p :: newNextLayer
          }
          computeLayerOfElement(otherElements,newNextLayer,currentLayerId,nbElementsLeft - 1)
      }
    }

    computeLayerOfElement(fstLayerElements,List(),0,nbPropagationElements)

  }

  def dropStaticGraph(): Unit = {
    for (p <- getPropagationElements) p.dropStaticGraph()
  }

  private[this] var scheduledElements: QList[PropagationElement] = null
  private[this] var executionQueue: AbstractHeap[PropagationElement] = null

  //I'v been thinking about using a BitArray here, but although this would slightly decrease memory
  // (think, relative to all the rest of the stored data), it would increase runtime
  private[this] var fastPropagationTracks: RedBlackTreeMap[Array[Boolean]] =  RedBlackTreeMap.empty

  private var partialPropagationTargets: List[PropagationElement] = List.empty

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
    if(closed) {
      println("Warning: You should not register a variable for partial propagation after model is closed.")
      println("         this might cause the model to crash if static graph was dropped on model close.")
      println("         To avoid this, create all your objective functions before model close.")
      println("         Note: there might be some implicit conversions related to the use of search strategies. ")
      addFastPropagationTracks()
    }
  }

  private[this] var previousPropagationTrack: Array[Boolean] = null

  def isPropagating: Boolean = propagating

  /** Triggers the propagation in the graph.
    *
    * The propagation has a target and stops when the target of the propagation has bee reached
    *
    * @param upTo
    *   The target element of the propagation
    */
  protected final def propagate(upTo: PropagationElement): Unit = ???
    if (!propagating) {
      if (UpTo != null && !debugMode) {
        //partial propagation, only if requested and not in debug mode (so in debug mode, it will always be total, and with debug)
        val Track = fastPropagationTracks.getOrElse(UpTo.uniqueID, null)
        val SameAsBefore = Track != null && previousPropagationTrack == Track
        propagating = true
        if (verbose) {
          println("PropagationStructure: " + (if (Track == null) "total" else "partial") + " propagation triggered by " + UpTo + " SameAsBefore:" + SameAsBpefore)
        }
        propagateOnTrack(Track, SameAsBefore)
        previousPropagationTrack = Track
      } else {
        //total propagation
        propagating = true
        if (verbose) {
          println("PropagationStructure: total propagation triggered manually")
        }
        propagateOnTrack(null, false)
        previousPropagationTrack = null
      }
    }
  }

  /**Builds and stores the partial propagation tracks*/
  private def addFastPropagationTracks(): Unit = {
    for (propagationGroup <- partialPropagationTargets) {
      val propagationGroupWithoutTrack = QList.buildFromIterable(propagationGroup.filter(p => !fastPropagationTracks.contains(p.uniqueID)))
      if (propagationGroupWithoutTrack != null) {
        val track = BuildFastPropagationTrack(propagationGroupWithoutTrack)
        for (singleTarget <- propagationGroupWithoutTrack) {
          fastPropagationTracks = fastPropagationTracks.insert(singleTarget.uniqueID, track)
        }
      }
    }
  }

  /**
   * Builds the partial propagation track for the specified target
   * @param target the propagation element for which we build the partial propagation track
   * @return an array of boolean: UniqueID => should the element with UniqueID be propagated for this target?
   */
  private def BuildFastPropagationTrack(target: QList[PropagationElement]): Array[Boolean] = {
    val Track: Array[Boolean] = Array.fill(getMaxID + 1)(false)

    var ToExplore: QList[PropagationElement] = target

    var currentPos = target
    while (currentPos != null) {
      if (currentPos.head.uniqueID != -1L)
        Track(currentPos.head.uniqueID) = true
      currentPos = currentPos.tail
    }

    while (ToExplore != null) {
      val n = ToExplore.head
      ToExplore = ToExplore.tail
      for (nn <- n.getStaticallyListenedElements)
        if (nn.uniqueID != -1L && !Track(nn.uniqueID)) {
          ToExplore = QList(nn, ToExplore)
          Track(nn.uniqueID) = true
        }
    }

    for (scc <- stronglyConnectedComponentsList) {
      Track(scc.uniqueID) = Track(scc.propagationElements.head.uniqueID)
    }
    Track
  }

  def checkUniqueID(): Unit = {
    for (p <- getPropagationElements) {
      require(p.uniqueID != -1L)
    }
  }

  private var postponedElements: QList[PropagationElement] = null

  /**
   * performs a propagation on a propagation track
   * if propagation track is omitte, total propagation is performed
   * @param track the propagation track, an array indices_of_propagation_element -> should it be propagated now
   * @param sameAsBefore the previous propagation was on the same track, so that the postponed element are still postponed
   */
  @inline
  private def propagateOnTrack(track: Array[Boolean], sameAsBefore: Boolean): Unit = {

    if (sameAsBefore) {
      //initialize the heap with the scheduled elements that are on the track
      var currentPos = scheduledElements
      while (currentPos != null) {
        val e = currentPos.head
        currentPos = currentPos.tail
        if (track(e.uniqueID)) {
          executionQueue.insert(e)
        } else {
          postponedElements = QList(e, postponedElements)
        }
      }
      scheduledElements = null
    } else if (track == null) {
      //all elements are to be put on the heap, included postponed ones
      var currentPos = postponedElements
      while (currentPos != null) {
        val e = currentPos.head
        currentPos = currentPos.tail
        executionQueue.insert(e)
      }
      postponedElements = null

      currentPos = scheduledElements
      while (currentPos != null) {
        val e = currentPos.head
        currentPos = currentPos.tail
        executionQueue.insert(e)
      }
      scheduledElements = null

    } else {
      //there is a track, and we need to check postponed elements because they might be on this track
      var newPostponed: QList[PropagationElement] = null
      var currentPos = postponedElements
      while (currentPos != null) {
        val e = currentPos.head
        currentPos = currentPos.tail
        if (track(e.uniqueID)) {
          executionQueue.insert(e)
        } else {
          newPostponed = QList(e, newPostponed)
        }
      }
      postponedElements = newPostponed

      currentPos = scheduledElements
      while (currentPos != null) {
        val e = currentPos.head
        currentPos = currentPos.tail
        if (track(e.uniqueID)) {
          executionQueue.insert(e)
        } else {
          postponedElements = QList(e, postponedElements)
        }
      }
      scheduledElements = null
    }

    var previousLayer = 0L

    val anythingDone = executionQueue.nonEmpty

    while (executionQueue.nonEmpty) {
      val first = executionQueue.popFirst()
      first.propagate()
      assert(first.position >= previousLayer, "single wave not enforced")
      assert({
        previousLayer = first.position; true
      })
      while (scheduledElements != null) {
        val e = scheduledElements.head
        scheduledElements = scheduledElements.tail
        if (track == null || track(e.uniqueID)) {
          executionQueue.insert(e)
        } else {
          postponedElements = QList(e, postponedElements)
        }
      }
    }

    if (track == null && anythingDone) {
      checker match {
        case Some(c) =>
          for (p <- getPropagationElements) {
            p.checkInternals(c)
          }
        case None =>
      }
    }
    propagating = false
  }

  /**
    * this method performs a check on the whole model.
    * it first ensures that there is nothing to propagate
    * @param c
    */
  def performCheck(c:Checker): Unit ={
    if(postponedElements != null || scheduledElements != null) {
      propagateOnTrack(null, false)
    }
    for (p <- getPropagationElements) {
      p.checkInternals(c)
    }
  }

  /** Schedules a propagation elements for the propagation
    *
    * @param p
    *   the element to schedule
    */
  private[propagation] def scheduleForPropagation(p: PropagationElement): Unit = {
    scheduledElements = QList(p, scheduledElements)
  }

  /**
   * this variable controls propagation.
   * initially true to avoid spurious propagation during the construction of the data structure;
   * set to false by setupPropagationStructure
   */
  var propagating: Boolean = true

  /**
   * this variable is set by the propagation element to notify that they are propagating.
   * it is used to ensure that no propagation element perform illegal operation
   * such as writing a variable they do not control, etc)
   */
  private[core] var PropagatingElement: PropagationElement = null

  /**
   * returns the propagation element that is currently propagating.
   * it allows one to ensure that the propagating element behaves as declared in its dependencies
   */
  def getPropagatingElement: PropagationElement = PropagatingElement

  /**
   * Builds a dictionary to store data related to the PE.
   * the dictionary is O(1L), based on an array.
   * It only works on PE that are registered to this structure.
   * The storage is not initialized, call the initialize to set it to some conventional value.
   * @tparam T the type stored in the data structure
   * @return a dictionary over the PE that are registered in the propagation structure.
   */
  def getNodeStorage[T](implicit X: Manifest[T]): NodeDictionary[T] = new NodeDictionary[T](this.MaxID)

  /**
   * returns some info on the PropagationStructure
   * call this after closing
   * @return
   */
  def stats: String = {
    s"""PropagationStructure(
       |  declaredAcyclic: $noCycle
       |  topologicalSort:$topologicalSort${if (!topologicalSort) s" (layerCount:${executionQueue.asInstanceOf[AggregatedBinomialHeapQList[PropagationElement]].maxPosition})" else ""}
       |  sortScc:$sortScc
       |  actuallyAcyclic:$acyclic
       |  TotalPropagationElementCount:${getPropagationElements.size}
       |  StronglyConnectedComponentsCount:${stronglyConnectedComponentsList.size}
       |${stronglyConnectedComponentsList.map(_.stats).mkString("\n")}
       |  PropagationElementsNotInSCC:{
       |${getPropagationElements.filter(_.schedulingHandler == this).map(_.getClass.getSimpleName).groupBy((name: String) => name).map(a => a._1 + ":" + a._2.size).mkString("\n    ")}
       |  }
       |)""".stripMargin
  }
}
