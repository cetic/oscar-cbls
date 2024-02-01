package oscar.cbls.core.propagation

/** An interface that provides the method to handle propagation for the element of the propopagation
  * graph
  *
  * @param propagationStructure
  *   The propagation structure to which the element is attached
  */
abstract class PropagationElement(propagationStructure: PropagationStructure) {

  private var layer_ : Int = 0L

  def layer = layer_

  def layer_=(newLayer : Int) = layer_ = newLayer

  private val id_ = propagationStructure.generateId()

  def id = id_



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

  /** Register an element as listened by this propagation element
    *
    * When a element listen another element, it means that this element dependes on the one it
    * listens. If a listened element is modified, this element has to be modified by the propagation
    * wave
    *
    * @param elem
    *   The element to insert
    */
  protected def registerStaticallyListenedElement(elem: PropagationElement): Unit = {
    assert(b != this)
    b.registerStaticallyListeningElement(this)
  }

  override protected[propagation] def registerStaticallyListeningElement(listening: PropagationElement): Unit = {
    listening.staticallyListenedElements = this :: listening.staticallyListenedElements
    staticallyListeningElements = listening :: staticallyListeningElements
  }

  /** Add an element in the set of dynamically listened element
    *
    * Some element may not always listened for the same elements (e.g. the "element" invariant).
    * This methods allow to add an element in the set of element that are listened by an element. It
    * returns a key that allows to remove the element from the set of listened element in constant
    * time.
    *
    * @param elem
    *   The element to insert
    * @return
    *   The key to perform the remove
    */
  protected def registerDynamicallyListenedElement(elem: PropagationElement): KeyForRemoval = ???

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

  /** Schedules the propagation element in the next propagation waves
    */
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

  /** this is the propagation method that should be overridden by propagation elements. notice that
    * it is only called in a propagation wave if: 1L: it has been registered for propagation since
    * the last time it was propagated 2L: it is included in the propagation wave: partial
    * propagation wave do not propagate all propagation elements; it only propagates the ones that
    * come in the predecessors of the targeted propagation element overriding this method is
    * optional, so an empty body is provided by default
    */
  def performPropagation(): Unit

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
>>>>>>> 61386b82a (Adding Propagation element en structure)
}
