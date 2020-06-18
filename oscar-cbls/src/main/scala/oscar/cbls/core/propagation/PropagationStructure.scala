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
 * ****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 * ****************************************************************************
 */
package oscar.cbls.core.propagation

import oscar.cbls.algo.dag._
import oscar.cbls.algo.dll._
import oscar.cbls.algo.heap.{AbstractHeap, AggregatedBinomialHeapQList, BinomialHeap}
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.tarjan._

/**
 * a schedulingHandler handles the scheduling for a set of PE.
 *
 */
trait SchedulingHandler {
  /**
   * when a PE needs propagation, it schedules itself to his SH through this method
   * notice that a PE cannot schedule itself for propagation if it has already been, and has not been propagated since
   * PE should ensure this by themselves, EG through an internal boolean variable
   * @param e the PE that is to be scheduled
   */
  def scheduleForPropagation(e: PropagationElement): Unit

  /**
   * @return the propagation structure that contains this, itself if it is a PS
   */
  def propagationStructure: PropagationStructure
}

/**
 * This class manages propagation among propagation elements.
 *
 * This class is intended to be extended, and the overriding class must implement
 * the method getPropagationElements that returns the propagation elements to be considered
 * Each propagation element has a UniqueID. Those should be assigned continuously starting from 0.
 *
 * It is to be used as follows: once the set of propagation elements is stabilized,
 * one must call setupPropagationStructure, which will built the necessary internal data structure
 * propagation are triggered by calling the propagate method.
 * additionally, before calling setupPropagationStructure, the method registerForPartialPropagation can be
 * called to specify propagation elements that might require lazy propagation.
 *
 *  Two debug mechanisms are provided: trace printing and debug mode.
 *
 *  A trace printing is provided; the propagation structure prints a trace of what it is propagating.
 *  This is activated by the Verbose parameter. All prints are preceded by ''PropagationStruture''
 *  This an be useful when checking the behavior of partial propagation.
 *
 *  A self-check method is called by the propagation structure after propagation is performed.
 *  This is activated by the Checker parameter.
 *  You should ensure that Asteroid is compiled with assert activated if you are using the debug mode.
 *  It will considerably slow down Asteroid, as other checks are implemented in the base modules.
 *
 *  Also, although this propagation structure is intended to support acyclic graph
 *  for the static dependency graph, you can deactivate the associated mechanism
 *  by setting the IsAcyclic graph to true.
 *  If unsure, set to false (or do not set; it is false by default),
 *  the engine will discover it by itself. See also method isAcyclic to query a propagation structure.
 *
 * @param verbose requires that the propagation structure prints a trace of what it is doing.
 * @param checker set a Some[Checker] top check all internal properties of invariants after propagation, set to None for regular execution
 * @param noCycle is to be set to true only if the static dependency graph is acyclic.
 * @param topologicalSort if true, use topological sort, false, use distance to input, and associated faster heap data structure
 * @param sortScc true if SCC should be sorted, false otherwise. Set to true, unless you know what your are doing. Setting to false might provide a speedup, but propagation will not be single pass on SCC anymore
 * @author renaud.delandtsheer@cetic.be
 */
abstract class PropagationStructure(val verbose: Boolean, val checker: Option[Checker] = None, val noCycle: Boolean, val topologicalSort: Boolean, val sortScc: Boolean = true)
  extends SchedulingHandler {

  protected var closed: Boolean = false

  def isClosed: Boolean = closed

  //priority queue is ordered, first on propagation planning list, second on DAG.

  /**
   * This method is to be overridden and is expected to return the propagation elements
   * on which the propagation structure will reason.
   * The method is expected to return consistent result once the setupPropagationStructure method is called
   */
  def getPropagationElements: QList[PropagationElement]

  /**
   * @return the propagation structure that contains this, itself if it is a PS
   */
  override def propagationStructure: PropagationStructure = this

  /**
   * This method is to be overridden and is expected to return the maximal value of the UniqueID
   * of the propagation elements
   * The method is expected to return consistent result once the setupPropagationStructure method is called
   */
  private var MaxID: Int = -1

  def getMaxID: Int = MaxID

  def GetNextID(): Int = {
    MaxID += 1
    MaxID
  }

  private var acyclic: Boolean = false

  /**
   * @return true if the propagation structure consider that his graph is acyclic, false otherwise.
   * call this after the call to setupPropagationStructure
   * If the propagation structure has been created with NoCycle set to true, this will return true
   */
  def isAcyclic: Boolean = acyclic

  private var stronglyConnectedComponentsList: List[StronglyConnectedComponent] = List.empty

  /**
   * To call when one has defined all the propagation elements on which propagation will ever be triggered.
   * It must be called before any propagation is triggered,
   * as it allows the propagation structure to build the necessary internal structures
   * @param DropStaticGraph if true, the propagation structure drops the static graph after setup.
   */
  protected def setupPropagationStructure(DropStaticGraph: Boolean): Unit = {

    val ClusteredPropagationComponents: List[PropagationElement] = if (noCycle) {
      getPropagationElements.toList
    } else {
      //identification des composantes connexes
      val storageForTarjan = this.getNodeStorage[TarjanNodeData]
      storageForTarjan.initialize(() => new TarjanNodeData)
      val stronglyConnectedComponents: List[QList[PropagationElement]] = TarjanWithExternalStorage.getStronglyConnexComponents[PropagationElement](
        getPropagationElements,
        p => p.getStaticallyListeningElements,
        storageForTarjan.get)
      acyclic = true
      stronglyConnectedComponentsList = List.empty
      stronglyConnectedComponents.map((a: QList[PropagationElement]) =>
        if (a.tail == null) {
          a.head
        } else {
          acyclic = false

          val c: StronglyConnectedComponent = if (sortScc) new StronglyConnectedComponentTopologicalSort(a, this, GetNextID())
          else new StronglyConnectedComponentNoSort(a, this, GetNextID())
          stronglyConnectedComponentsList = c :: stronglyConnectedComponentsList
          c
        })
    }

    addFastPropagationTracks()

    //this performs the sort on Propagation Elements that do not belong to a strongly connected component,
    // plus the strongly connected components, considered as a single node. */
    var LayerCount:Int = 0
    if (topologicalSort) {
      computePositionsThroughTopologicalSort(ClusteredPropagationComponents)
      executionQueue = new BinomialHeap[PropagationElement](p => p.position, ClusteredPropagationComponents.size)
    } else {
      LayerCount = computePositionsThroughDistanceToInput(ClusteredPropagationComponents) + 1
      executionQueue = new AggregatedBinomialHeapQList[PropagationElement](p => p.position, LayerCount)
    }

    propagating = false
    previousPropagationTrack = null

    if (DropStaticGraph) dropStaticGraph()

    //variables are already able to propagate immediately before model close and if not monitored yet.

    scheduledElements = null

    val it = getPropagationElements.iterator
    while (it.hasNext) {
      it.next().rescheduleIfNeeded()
    }

    for (scc <- stronglyConnectedComponentsList) {
      scc.rescheduleIfNeeded()
    }
    //propagate() we do not propagate anymore here since the first query might require a partial propagation only
  }

  /**This computes the position of the clustered PE, that is: the SCC and the PE not belonging to an SCC*/
  private def computePositionsThroughTopologicalSort(ClusteredPropagationComponents: List[PropagationElement]): Unit = {
    var front: List[PropagationElement] = ClusteredPropagationComponents.filter(n => { n.setCounterToPrecedingCount(); n.position == 0})
    var position = 0 //la position du prochain noeud place.
    while (front.nonEmpty) {
      val n = front.head
      front = front.tail
      n.position = position
      position += 1
      front = n.decrementSucceedingAndAccumulateFront(front)
    }
    if (position != ClusteredPropagationComponents.size) {
      if (noCycle) {
        throw new Exception("cycle detected in propagation graph, please set NoCycle flag to false when declaring your model")
      } else {
        throw new Exception("internal bug")
      }
    }
  }

  /**
   * This computes the position of the clustered PE based on distance to input,
   * that is: the SCC and the PE not belonging to an SCC
   * @return the max Position, knowing that the first is zero
   */
  private def computePositionsThroughDistanceToInput(ClusteredPropagationComponents: List[PropagationElement]): Int = {
    val front: DoublyLinkedList[PropagationElement] = new DoublyLinkedList[PropagationElement]()
    for (pe <- ClusteredPropagationComponents) {
      pe.setCounterToPrecedingCount()
      if (pe.position == 0) front.enqueue(pe)
    }
    front.enqueue(null) //null marker denotes when Position counter must be incremented
    var position = 0 //la position du prochain noeud place.
    var count = 0 //the number of PE
    var countInLayer = 0

    while (true) {
      //we know it is not empty here
      val n = front.dequeue()
      if (n == null) {
        if (front.isEmpty) {
          if (count != ClusteredPropagationComponents.size) {
            if (noCycle) {
              throw new Exception("cycle detected in propagation graph although NoCycle was set to true")
            } else {
              throw new Exception("internal bug")
            }
          }
          return position + 1
        } else {
          countInLayer = 0
          position += 1
          front.enqueue(null) //null marker denotes when Position counter must be incremented
        }
      } else {
        n.position = position
        count += 1
        countInLayer += 0
        for (pe <- n.decrementSucceedingAndAccumulateFront(List.empty)) front.enqueue(pe)
      }
    }
    0 //never reached
  }

  def dropStaticGraph(): Unit = {
    for (p <- getPropagationElements) p.dropStaticGraph()
  }

  private[this] var scheduledElements: QList[PropagationElement] = null
  private[this] var executionQueue: AbstractHeap[PropagationElement] = null

  //I'v been thinking about using a BitArray here, but although this would slightly decrease memory
  // (think, relative to all the rest of the stored data), it would increase runtime
  private[this] var fastPropagationTracks: RedBlackTreeMap[Array[Boolean]] =  RedBlackTreeMap.empty

  private var partialPropagationTargets: List[QList[PropagationElement]] = List.empty

  /**
   * to call before setupPropagationStructure to specify PropagationElements
   * on which one need partial propagation
   * if several elements are submitted at the same time,they constitute a target group, which is propagated altogether.
   */
  def registerForPartialPropagation(p: PropagationElement*): Unit = {
    partialPropagationTargets = QList.buildFromIterable(p) :: partialPropagationTargets
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

  private [this] val debugMode = checker match{case Some(_) => true; case None => false}

  /**
   * triggers the propagation in the graph.
   * this method will do nothing if called before setupPropagationStructure
   * if UpTo set to a PropagationElement,
   * and provided it has been registered through the registerForPartialPropagation method,
   * the propagation will be partial, targeting this element.
   * @param UpTo: the optional target of partial propagation
   */
  final def propagate(UpTo: PropagationElement = null): Unit = {
    if (!propagating) {
      if (UpTo != null && !debugMode) {
        //partial propagation, only if requested and not in debug mode (so in debug mode, it will always be total, and with debug)
        val Track = fastPropagationTracks.getOrElse(UpTo.uniqueID, null)
        val SameAsBefore = Track != null && previousPropagationTrack == Track
        propagating = true
        if (verbose) {
          println("PropagationStructure: " + (if (Track == null) "total" else "partial") + " propagation triggered by " + UpTo)
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
   * @param Track the propagation track, an array indices_of_propagation_element -> should it be propagated now
   * @param SameAsBefore the previous propagation was on the same track, so that the postponed element are still postponed
   */
  @inline
  private def propagateOnTrack(Track: Array[Boolean], SameAsBefore: Boolean): Unit = {

    if (SameAsBefore) {
      //initialize the heap with the scheduled elements that are on the track
      var currentPos = scheduledElements
      while (currentPos != null) {
        val e = currentPos.head
        currentPos = currentPos.tail
        if (Track(e.uniqueID)) {
          executionQueue.insert(e)
        } else {
          postponedElements = QList(e, postponedElements)
        }
      }
      scheduledElements = null
    } else if (Track == null) {
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
        if (Track(e.uniqueID)) {
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
        if (Track(e.uniqueID)) {
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
        if (Track == null || Track(e.uniqueID)) {
          executionQueue.insert(e)
        } else {
          postponedElements = QList(e, postponedElements)
        }
      }
    }

    if (Track == null && anythingDone) {
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

  /**this method is used by propagationComponents to schedule themselves for propagation. */
  def scheduleForPropagation(p: PropagationElement): Unit = {
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

  /*This dumps the propagation graphs in a dot format, for documentation purposes
    * Static graph should only be set if the static graph has not been dropped
    * @param StaticGraph adds the static graph as red arrows
    * @param DynamicGraph adds the dynamic graph as blue arrows
    * @return a string that contains the dot format
    **/
  /*
  def dumpToDot(StaticGraph: Boolean, DynamicGraph: Boolean, Target:PropagationElement = null): String = {
    var ToReturn = "digraph PropagationStructure {\n"
    ToReturn += "   rankdir=LR;\n"
    def nodeName(p: PropagationElement) = "node" + p.uniqueID

    if(!StaticGraph && !DynamicGraph)
      throw new Exception("you want to dump to dot, but none of the static and dynamic graphs")

    for (e <- getPropagationElements if e.schedulingHandler == this) {
      if (! (!StaticGraph && e.isInstanceOf[BulkPropagator]))
        ToReturn += "   " + nodeName(e) + e.getDotNode + "\n"
    }

    for (scc <- StronglyConnexComponentsList){
      ToReturn += "   subgraph " + "cluster_"+nodeName(scc) + "{" + "\n"
      for (f <- scc.Elements) {
        ToReturn += "      " + nodeName(f) + f.getDotNode + "\n"
      }
      ToReturn += "   }" + "\n"
    }

    if (StaticGraph && DynamicGraph){
      for (e <- getPropagationElements) {
        for (f <- e.getStaticallyListenedElements if f.uniqueID != -1L) {
          if (e.getDeterminingElement == f){
            //determining element, blue arrow
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = blue]" + "\n"
          }else if (e.getDynamicallyListenedElements.exists(p => p==f)){
            //in static and dynamic graph
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = red]" + "\n"
          }else{
            //only in static graph
            if(this.isAcyclic){
              ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = black style=dotted]" + "\n"
            }else{
              ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = black style=dotted constraint=false]" + "\n"
            }
          }
        }
        for (f <- e.getDynamicallyListenedElements if f.uniqueID != -1L) {
          if (!e.getStaticallyListenedElements.exists(p => p==f)){
            //in dynamic graph and not in static one because of bulking
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = red]" + "\n"
          }
        }
      }
    }else if (StaticGraph) {
      for (e <- getPropagationElements) {
        for (f <- e.getStaticallyListenedElements if f.uniqueID != -1L) {
          ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = black style=dotted]" + "\n"
        }
      }
    }else if (DynamicGraph) {
      for (e <- getPropagationElements) {
        for (f <- e.getDynamicallyListenedElements if f.uniqueID != -1L) {
          if (e.getDeterminingElement == f){
            //determining element, blue arrow
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = blue]" + "\n"
          }else{
            //in dynamic graph
            ToReturn += "   " + nodeName(f) + " -> " + nodeName(e) + "[color = red]" + "\n"
          }
        }
      }
    }
    ToReturn + "}\n"
  }
*/

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

/**
 * This is a O(1L) dictionary for propagation elements.
 * It is based on an array, and the keys it support is only the PE that have been reistered
 * to the propagation structure by the time this is instantiated.
 * WARNING: this is not efficient if you do not actually use many of the keys
 * because the instantiated array will be very large compared to your benefits.
 * This might kill cache and RAM for nothing
 *
 * @param MaxNodeID the maximal ID of a node to be stored in the dictionary (since it is O(1L) it is an array, and we allocate the full necessary size
 * @tparam T the type stored in this structure
 * @author renaud.delandtsheer@cetic.be
 */
class NodeDictionary[T](val MaxNodeID: Int)(implicit val X: Manifest[T]) {
  private val storage: Array[T] = new Array[T](MaxNodeID + 1)

  def update(elem: PropagationElement, value: T): Unit = {
    storage(elem.uniqueID) = value
  }

  def get(elem: PropagationElement): T = storage(elem.uniqueID)

  def initialize(value: () => T): Unit = {
    for (i <- storage.indices) storage(i) = value()
  }
}

abstract class StronglyConnectedComponent(val propagationElements: Iterable[PropagationElement],
                                          val core: PropagationStructure, val _UniqueID: Int) extends PropagationElement with SchedulingHandler {
  schedulingHandler = core
  uniqueID = _UniqueID

  for (e <- propagationElements) e.schedulingHandler = this

  def size: Int = propagationElements.size

  override def propagationStructure: PropagationStructure = core

  /**
   * This returns the dot node to display on the DOT output for the node. Only the argument of the nodes
   * example: "[label= \"toto\" shape=diamond color=red]"
   */
  def getDotNode: String = {
    throw new Exception("StrognlyConnectedComponent are handled as subgraph in dot files")
    ""
  }

  var scheduledElements: QList[PropagationElement] = null

  def scheduleForPropagation(element: PropagationElement): Unit = {
    scheduledElements = QList(element, scheduledElements)
    super.scheduleForPropagation()
  }

  override def decrementSucceedingAndAccumulateFront(acc: List[PropagationElement]): List[PropagationElement] = {
    var toreturn = acc
    for (element <- propagationElements) {
      toreturn = element.decrementSucceedingAndAccumulateFront(toreturn)
    }
    toreturn
  }

  override def setCounterToPrecedingCount(): Boolean = {
    position = propagationElements.count(p => p.setCounterToPrecedingCount())
    position != 0
  }

  override private[core] def rescheduleIfNeeded(): Unit = {}
  //we do nothing, since it is the propagation elements that trigger the registration if needed of SCC

  override def checkInternals(c: Checker): Unit = {
    for (e <- propagationElements) { e.checkInternals(c) }
  }

  def stats:String = {
    s"""{
       |  ${propagationElements.map(_.getClass.getSimpleName).groupBy((name: String) => name).map(a => a._1 + ":" + a._2.size).mkString("\n    ")}
       |}""".stripMargin
  }
}

class StronglyConnectedComponentNoSort(Elements: Iterable[PropagationElement],
                                       core: PropagationStructure, _UniqueID: Int) extends StronglyConnectedComponent(Elements, core, _UniqueID) {

  override def performPropagation(): Unit = {
    while (scheduledElements != null) {
      val x = scheduledElements.head
      scheduledElements = scheduledElements.tail
      x.propagate()
    }
  }

  override def stats: String = s"  StronglyConnectedComponentNoSort${super.stats}"
}

class StronglyConnectedComponentTopologicalSort(
  override val propagationElements: Iterable[PropagationElement],
  override val core: PropagationStructure,
  _UniqueID: Int)
  extends StronglyConnectedComponent(propagationElements, core, _UniqueID) with DAG {

  for (e <- propagationElements) {
    e.setInSortingSCC()
  }
  for (e <- propagationElements) {
    e.initiateDynamicGraphFromSameComponent(this)
  }

  //for the DAG
  override def nodes = propagationElements.asInstanceOf[Iterable[DAGNode]]

  var newDependenciesToInject: List[WaitingDependency] = List.empty

  case class WaitingDependency(from: PropagationElement,
                               to: PropagationElement,
                               var inject1: (() => Unit) = null,
                               var inject2: (() => Unit) = null,
                               var isStillValid: (() => Boolean) = null) {
    /**
     * injects the waiting dependency
     * @return true if the dependency was injected, false otherwise
     */
    def injectIfStillValid(): Boolean = {
      if (isStillValid()) {
        inject1()
        inject2()
        true
      } else false
    }

    def inject(): Unit = {
      inject1()
      inject2()
    }
  }

  def injectWaitingNewDependencies(autoSort: Boolean): Unit = {
    for (d: WaitingDependency <- newDependenciesToInject) {
      if (d.injectIfStillValid() && autoSort) notifyAddEdge(d.from, d.to)
    }
    newDependenciesToInject = List.empty
  }

  def registerListenedWaitingDependency(injector: () => Unit,
                                        isStillValid: () => Boolean): Unit = {
    if (autoSort) {
      val waiting = newDependenciesToInject.head
      waiting.inject1 = injector
      waiting.isStillValid = isStillValid
    } else {
      injector()
    }
  }

  def registerListeningWaitingDependency(injector: () => Unit): Unit = {
    if (autoSort) {
      val waiting = newDependenciesToInject.head
      waiting.inject2 = injector
    } else {
      injector()
    }
  }

  def addDependency(from: PropagationElement, to: PropagationElement): Unit = {
    if (autoSort) {
      newDependenciesToInject = WaitingDependency(from, to) :: newDependenciesToInject
    }
  }

  /**
   * this is called when the dependency has been added and all its field are filled.
   * We take the opportunity to check if the dependency is by any chance already implemented
   * in the sort.
   * if yes, we inject it right away, since it does not trigger any computation, actually.
   */
  def dependencyAdded(): Unit = {
    if (autoSort) {
      val waiting = newDependenciesToInject.head
      if (waiting.from.position < waiting.to.position) {
        waiting.inject()
        notifyAddEdge(waiting.from, waiting.to)
        newDependenciesToInject = newDependenciesToInject.tail
      }
    }
  }

  val h: BinomialHeap[PropagationElement] = new BinomialHeap[PropagationElement](p => p.position, size)

  override def performPropagation(): Unit = {
    //setting autosort to true will not perform any operation unless it was set to false. This happens in two cases:
    //at the initial propagation, and when a stall just occurred. In these case, a non-incremental sort takes place

    injectWaitingNewDependencies(autoSort)
    autoSort = true

    var currentPos = scheduledElements
    while (currentPos != null) {
      h.insert(currentPos.head)
      currentPos = currentPos.tail
    }
    scheduledElements = null

    var maxposition: Long = -1L

    while (!h.isEmpty) {
      val x = h.popFirst()
      x.propagate()
      assert(x.position >= maxposition, "non monotonic propagation detected in SCC")
      assert({ maxposition = x.position; true })

      var currentPos = scheduledElements
      while (currentPos != null) {
        h.insert(currentPos.head)
        currentPos = currentPos.tail
      }
      scheduledElements = null
    }
  }

  override def stats: String = s"  StronglyConnectedComponentTopologicalSort${super.stats}"
}

object PropagationElement {
  implicit val Ord: Ordering[PropagationElement] = (o1: PropagationElement, o2: PropagationElement) => o1.compareTo(o2)
}

/**
 * This class is used in as a handle to register and unregister dynamically to variables
 * @author renaud.delandtsheer@cetic.be
 */
class KeyForElementRemoval(val keyForListenedElement: DPFDLLStorageElement[(PropagationElement, Int)], val keyForListeningElement: DPFDLLStorageElement[PropagationElement]) {
  def performRemove(): Unit = {
    keyForListeningElement.delete()
    keyForListenedElement.delete()
  }
}

case object DummyKeyForElementRemoval extends KeyForElementRemoval(null, null) {
  override def performRemove(): Unit = {}
}

/**
 * this is a basic PE that actually does not integrate into the propagation network
 * it is used by constants
 */
trait BasicPropagationElement {

  protected[propagation] def registerStaticallyListeningElement(listening: PropagationElement): Unit = {
    //TODO: here, we should add this to the listening in order to use our symmetry detction framework
  }

  /**
   * only if the listening is not varying its dependencies
   *
   * there is not scc because if someone call this, he is not dynamic PE, hence is not a boundary
   * it also has no dynamicallyListened stuff to update (only static stuff)
   * can only be called before model closing
   * @param listening the dynamically listening element
   * @param i: the payload that will be given for the notification, according to what the PE is supposed to do
   */
  protected[propagation] def registerDynamicallyListeningElementNoKey(listening: PropagationElement, i: Int): Unit = {}

  /**
   * @param listening the listening element
   * @param sccOfListening the SCC in case listening is on he boundary, null otherwise
   * @param dynamicallyListenedElementDLLOfListening the PFDLL
   * @return a key for dependency removal
   */
  protected[propagation] def registerDynamicallyListeningElement(listening: PropagationElement,
                                                                 i: Int,
                                                                 sccOfListening: StronglyConnectedComponentTopologicalSort,
                                                                 dynamicallyListenedElementDLLOfListening:DelayedPermaFilteredDoublyLinkedList[PropagationElement]): KeyForElementRemoval = DummyKeyForElementRemoval

  def schedulingHandler: SchedulingHandler = null
}

/**
 * it does not changes it listened elements
 * however, its listening elements might change, and a proper list must therefore be kept.
 */
class PropagationElement extends BasicPropagationElement with DAGNode {

  def dropStaticGraph(): Unit = {
    staticallyListenedElements = null
  }

  var dynamicallyListenedElementsFromSameComponent: Iterable[PropagationElement] = null
  var dynamicallyListeningElementsFromSameComponent: Iterable[PropagationElement] = null

  //dynamicallyListenedElementsFromSameComponent
  final def getDAGPrecedingNodes: Iterable[DAGNode] = dynamicallyListenedElementsFromSameComponent

  //dynamicallyListeningElementsFromSameComponent
  final def getDAGSucceedingNodes: Iterable[DAGNode] = dynamicallyListeningElementsFromSameComponent

  def initiateDynamicGraphFromSameComponent(stronglyConnectedComponentTopologicalSort: StronglyConnectedComponentTopologicalSort): Unit = {
    initiateDynamicGraphFromSameComponentListening(stronglyConnectedComponentTopologicalSort)
    initiateDynamicGraphFromSameComponentListened(stronglyConnectedComponentTopologicalSort)
  }

  protected def initiateDynamicGraphFromSameComponentListening(stronglyConnectedComponentTopologicalSort: StronglyConnectedComponentTopologicalSort): Unit = {
    def filterForListening(listeningAndPayload: (PropagationElement, Any), injector: () => Unit, isStillValid: () => Boolean): Unit = {
      if (stronglyConnectedComponentTopologicalSort == listeningAndPayload._1.schedulingHandler)
        stronglyConnectedComponentTopologicalSort.registerListeningWaitingDependency(injector)
    }

    dynamicallyListeningElementsFromSameComponent = dynamicallyListeningElements.delayedPermaFilter(filterForListening, (e) => e._1)
  }

  protected def initiateDynamicGraphFromSameComponentListened(stronglyConnectedComponentTopologicalSort: StronglyConnectedComponentTopologicalSort): Unit = {
    assert(stronglyConnectedComponentTopologicalSort == mySchedulingHandler)
    //filters the list of staticallyListenedElements

    dynamicallyListenedElementsFromSameComponent = staticallyListenedElements.filter(_.schedulingHandler == stronglyConnectedComponentTopologicalSort)
  }

  /**
   * the thing to which we schedult ourselves for propagation
   * can be a SCC or a PS
   */
  override def schedulingHandler: SchedulingHandler = mySchedulingHandler

  def schedulingHandler_=(s: SchedulingHandler): Unit = {
    mySchedulingHandler = s
  }

  private[this] var mySchedulingHandler: SchedulingHandler = null

  def propagationStructure: PropagationStructure = if (mySchedulingHandler == null) null else mySchedulingHandler.propagationStructure
  def hasPropagationStructure = mySchedulingHandler != null

  /**
   * set to true if the PropagationElement is scheduled for propagation, false otherwise.
   * this is managed by the PropagationElement
   */
  private[this] var internalIsScheduled: Boolean = false
  def isScheduled: Boolean = internalIsScheduled

  private[propagation] var staticallyListenedElements: List[PropagationElement] = List.empty
  private[propagation] var staticallyListeningElements: List[PropagationElement] = List.empty

  private final val dynamicallyListeningElements: DelayedPermaFilteredDoublyLinkedList[(PropagationElement, Int)] = new DelayedPermaFilteredDoublyLinkedList[(PropagationElement, Int)]

  /**
   * through this method, the PropagationElement must declare which PropagationElement it is listening to
   * in the static dependency graph. The result must be stable after the call to setupPropagationStructure.
   * to override
   */
  protected[core] final def getStaticallyListenedElements: Iterable[PropagationElement] = staticallyListenedElements

  /**
   * through this method, the PropagationElement must declare which PropagationElement listen to it
   * in the static dependency graph. The result must be stable after the call to setupPropagationStructure.
   * to override
   */
  protected[core] final def getStaticallyListeningElements: Iterable[PropagationElement] = staticallyListeningElements

  private[core] final def getDynamicallyListeningElements:DelayedPermaFilteredDoublyLinkedList[(PropagationElement, Int)]
  = dynamicallyListeningElements

  protected[core] def getDynamicallyListenedElements: Iterable[PropagationElement] = staticallyListenedElements

  protected def registerStaticallyListenedElement(b: BasicPropagationElement): Unit = {
    assert(b != this)
    b.registerStaticallyListeningElement(this)
  }

  override protected[propagation] def registerStaticallyListeningElement(listening: PropagationElement): Unit = {
    listening.staticallyListenedElements = this :: listening.staticallyListenedElements
    staticallyListeningElements = listening :: staticallyListeningElements
  }

  /**this will not return a key because we do not have varying dependencies*/
  protected def registerDynamicallyListenedElement(b: BasicPropagationElement, i: Int): KeyForElementRemoval = {
    b.registerDynamicallyListeningElementNoKey(this, i)
    null
  }

  /**
   * only if the listening is not varying its dependencies
   *
   * there is not scc because if someone call this, he is not dynamic PE, hence is not a boundary
   * it also has no dynamicallyListened stuff to update (only static stuff)
   * can only be called before model closing
   * @param listening the dynamically listening element
   */
  override protected[propagation] def registerDynamicallyListeningElementNoKey(listening: PropagationElement, i: Int): Unit = {
    dynamicallyListeningElements.addElem(listening, i)
  }

  /**
   * @param listening the listening element
   * @param sccOfListening the SCC in case listening is on he boundary, null otherwise
   * @param dynamicallyListenedElementDLLOfListening the PFDLL
   * @return a key for dependency removal
   */
  override protected[propagation] def registerDynamicallyListeningElement(listening: PropagationElement, i: Int,
                                                                          sccOfListening: StronglyConnectedComponentTopologicalSort,
                                                                          dynamicallyListenedElementDLLOfListening: DelayedPermaFilteredDoublyLinkedList[PropagationElement]): KeyForElementRemoval = {
    if (sccOfListening != null && sccOfListening == this.mySchedulingHandler) {
      //this is only called once the component is established, so no worries.
      //we must call this before performing the injection to create the waitingDependency in the SCC
      sccOfListening.addDependency(this, listening)
      val keyForListenedElement = dynamicallyListeningElements.addElem((listening, i))
      val keyForListeningElement = dynamicallyListenedElementDLLOfListening.addElem(this)
      sccOfListening.dependencyAdded()
      new KeyForElementRemoval(keyForListenedElement, keyForListeningElement)
    } else {
      val keyForListenedElement = dynamicallyListeningElements.addElem((listening, i))
      val keyForListeningElement = dynamicallyListenedElementDLLOfListening.addElem(this)
      new KeyForElementRemoval(keyForListenedElement, keyForListeningElement)
    }
  }

  def setInSortingSCC(): Unit = {}

  def compare(that: DAGNode): Int = {
    assert(this.uniqueID != -1, s"cannot compare non-registered PropagationElements this: [$this] that: [$that]")
    assert(that.uniqueID != -1, s"cannot compare non-registered PropagationElements this: [$this] that: [$that]")
    this.uniqueID - that.uniqueID
  }

  def decrementSucceedingAndAccumulateFront(acc: List[PropagationElement]): List[PropagationElement] = {
    var toreturn = acc
    for (succeeding <- getStaticallyListeningElements) {
      if (succeeding.schedulingHandler == mySchedulingHandler.propagationStructure || succeeding.schedulingHandler != mySchedulingHandler) {
        //not in the same SCC as us
        toreturn = succeeding.decrementAndAccumulateFront(toreturn)
      }
    }
    toreturn
  }

  final def decrementAndAccumulateFront(acc: List[PropagationElement]): List[PropagationElement] = {
    position -= 1
    if (position == 0) {
      //faut pusher qqchose
      mySchedulingHandler match {
        case scc: StronglyConnectedComponent =>
          scc.decrementAndAccumulateFront(acc)
        case s: PropagationStructure => this :: acc
      }
    } else {
      acc
    }
  }

  /**
   * Sets the Position oto the number of element that need to be decremented, not belonging to same connex component
   * for connex component, set it to the number of element that are referenced from othercomponents
   * @return true if there is a dependency, false otherwise
   */
  def setCounterToPrecedingCount(): Boolean = {
    //le compteur est mis au nombre de noeud precedent qui ne sont pas dans la meme composante connexe
    mySchedulingHandler match {
      case scc: StronglyConnectedComponent =>
        position = this.getStaticallyListenedElements.count(p => p.schedulingHandler != scc && p.schedulingHandler != null)
      case ps: PropagationStructure =>
        position = this.getStaticallyListenedElements.count(p => p.schedulingHandler != null)
    }
    position != 0L
  }

  /**to invoque to force inclusion of the propagation element in the current or next propagation wave. */
  final def scheduleForPropagation(): Unit = {
    assert(schedulingHandler != null, "cannot schedule or propagate element out of propagation structure")
    if (!internalIsScheduled) {
      internalIsScheduled = true
      mySchedulingHandler.scheduleForPropagation(this)
    }
  }

  private[core] def rescheduleIfNeeded(): Unit = {
    if (internalIsScheduled) {
      mySchedulingHandler.scheduleForPropagation(this)
    }
  }

  /**
   * Performs the propagation, and some bookkeeping around it.
   */
  final def propagate(): Unit = {
    assert(internalIsScheduled) //could not be scheduled actually, if was propagated, but not purged from postponed (in case select propagation for input is implemented)
    assert(propagationStructure != null, "cannot schedule or propagate element out of propagation structure")
    assert({ propagationStructure.PropagatingElement = this; true })
    performPropagation()
    internalIsScheduled = false //to avoid registering SCC to the propagation structure every time...
    assert({ propagationStructure.PropagatingElement = null; true })
  }

  /**
   * this is the propagation method that should be overridden by propagation elements.
   * notice that it is only called in a propagation wave if:
   * 1L: it has been registered for propagation since the last time it was propagated
   * 2L: it is included in the propagation wave: partial propagation wave do not propagate all propagation elements;
   *    it only propagates the ones that come in the predecessors of the targeted propagation element
   *  overriding this method is optional, so an empty body is provided by default
   */
  def performPropagation(): Unit = {}

  /**
   * This is the debug procedure through which propagation element can redundantly check
   * that the incremental computation they perform through the performPropagation method is correct
   * overriding this method is optional, so an empty body is provided by default
   */
  def checkInternals(c: Checker): Unit = {}

  /**
   * This returns the dot node to display on the DOT output for the node. Only the argument of the nodes
   * example: "[label= \"toto\" shape=diamond color=red]"
   */
  //  def getDotNode: String
}

trait VaryingDependenciesPE extends PropagationElement {
  //for cycle managing
  /**
   * set to true if the PropagationElement is one that can break
   * or make dependency cycles in the dynamic dependency graph
   * managed by the PropagationComponent
   * basically, set to true if the determiningElement is not in the same component
   * and if this PropagationElement belongs to a cycle in the static dependency graph
   */
  private var inSortingSCC: Boolean = false

  /**
   * this sets the value of IsBoundary according to the definition of this variable
   * @return the value of IsBoundary
   */
  override def setInSortingSCC(): Unit = {
    assert(schedulingHandler.isInstanceOf[StronglyConnectedComponentTopologicalSort])
    require(determiningElement != null)
    require(determiningElement.schedulingHandler == null || determiningElement.schedulingHandler != this.schedulingHandler)
    inSortingSCC = true
  }

  private var determiningElement: BasicPropagationElement = null
  def getDeterminingElement = determiningElement

  /**
   * must belong to the statically listened elements.
   * cannot be added to the dynamically listened ones
   * (it is added through this method, and you cannot remove it, so you do not get the key for removing it)
   * @param p the element that determines the dynamic dependencies of the propagation element
   * @param i an additional value that is stored in this element together with the reference to this,
   * can be use for notification purposes
   */
  protected final def registerDeterminingElement(p: BasicPropagationElement, i: Int): Unit = {
    p match {
      case pe: PropagationElement =>
        assert(this.getStaticallyListenedElements.exists(e => e == pe),
          s"dependency to determining element $p must be registered in static propagation graph")
        assert(determiningElement == null, "only one determining element is authorized")
        registerDynamicallyListenedElement(pe, i)
        determiningElement = pe
      case _ => ;
    }
  }

  private[propagation] final val dynamicallyListenedElements: DelayedPermaFilteredDoublyLinkedList[PropagationElement] = new DelayedPermaFilteredDoublyLinkedList[PropagationElement]

  override protected[core] def getDynamicallyListenedElements: Iterable[PropagationElement] = dynamicallyListenedElements

  override protected def registerDynamicallyListenedElement(b: BasicPropagationElement, i: Int): KeyForElementRemoval =
    b.registerDynamicallyListeningElement(
      this,
      i,
      if (inSortingSCC) schedulingHandler.asInstanceOf[StronglyConnectedComponentTopologicalSort] else null,
      dynamicallyListenedElements)

  override protected def initiateDynamicGraphFromSameComponentListened(stronglyConnectedComponentTopologicalSort: StronglyConnectedComponentTopologicalSort): Unit = {
    assert(stronglyConnectedComponentTopologicalSort == schedulingHandler)
    def filterForListened(listened: PropagationElement, injector: () => Unit, isStillValid: () => Boolean): Unit = {
      if (stronglyConnectedComponentTopologicalSort == listened.schedulingHandler)
        stronglyConnectedComponentTopologicalSort.registerListenedWaitingDependency(injector, isStillValid)
    }
    dynamicallyListenedElementsFromSameComponent = dynamicallyListenedElements.delayedPermaFilter(filterForListened)
  }

  override def dropStaticGraph(): Unit = {
    staticallyListenedElements = null
    staticallyListeningElements = null
  }
}

/**
 * This is the node type to be used for bulking
 * @author renaud.delandtsheer@cetic.be
 * *
 */
trait BulkPropagator extends PropagationElement {
  override protected def initiateDynamicGraphFromSameComponentListened(stronglyConnectedComponentTopologicalSort: StronglyConnectedComponentTopologicalSort): Unit = {
    assert(stronglyConnectedComponentTopologicalSort == schedulingHandler)
    //filters the list of staticallyListenedElements

    dynamicallyListenedElementsFromSameComponent = List.empty
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
abstract class Checker {
  def check(verity: Boolean, traceOption: => Option[String] = None): Unit
}

/**
 * a checker that trows an error as soon as there is an error
 * @author renaud.delandtsheer@cetic.be
 */
case class ErrorChecker() extends Checker {
  def check(verity: Boolean, traceOption: => Option[String]): Unit = {
    if (!verity)
      throw new Error(s"Error in checker, debug: $traceOption")
  }
}
