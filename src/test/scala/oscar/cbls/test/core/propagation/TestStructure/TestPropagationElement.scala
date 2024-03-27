package oscar.cbls.core.propagation


/** A propagation element to test the propagation structure.
  *
  * @param structure
  *   the propagation structure to which this element is attached
  */

abstract class TestPropagationElement(structure: TestPropagationStructure)
    extends PropagationElement(structure) {

  private val debugLevel = structure.debugLevel

  var updateRequired: Boolean                = false
  var updateRequiredThisPropagation: Boolean = false
  var nbUpdate: Int                          = 0

  var nbCheckInternals = 0

  val name: String

  protected[this] var dynamicallyListeningElement: List[TestPropagationElement] = List()

  def staticGraphIsNull: Boolean = staticallyListenedElements == null && staticallyListeningElements == null

  def isOutput: Boolean = staticallyListeningElements.isEmpty

  def isInput: Boolean = staticallyListenedElements.isEmpty

  def validateLayer(): Unit = {
    assert(
      theoreticalLayer == layer,
      s"On $name: The layer computed by the layer computing algorithm is not coherent with the layer computed by construction (theoreticalLayer : $theoreticalLayer, algorithm layer : $layer)"
    )
  }

  private var transitivePredecessorsElements: Set[TestPropagationElement] = Set()
  private var transitiveSuccessorsElements: Set[TestPropagationElement]   = Set()

  def transitivePredecessors: Set[TestPropagationElement] = transitivePredecessorsElements

  def transitiveSuccessors: Set[TestPropagationElement] = transitiveSuccessorsElements

  private def addDynamicallyListeningElement(e: TestPropagationElement): Unit = {
    dynamicallyListeningElement = e :: dynamicallyListeningElement
  }

  override def registerStaticallyListenedElement(e: PropagationElement): Unit = {
    super.registerStaticallyListenedElement(e)
    e.asInstanceOf[TestPropagationElement].addDynamicallyListeningElement(this)
    addTransitiveDependency(e.asInstanceOf[TestPropagationElement])
  }

  private def addTransitivePredecessor(e: TestPropagationElement): Unit = {
    transitivePredecessorsElements = transitivePredecessorsElements + e
  }

  private def addTransitiveSuccessor(e: TestPropagationElement): Unit = {
    transitiveSuccessorsElements = transitiveSuccessorsElements + e
  }

  def addTransitiveDependency(e: TestPropagationElement): Unit = {
    addTransitivePredecessor(e)
    e.addTransitiveSuccessor(this)
    for (p <- e.transitivePredecessorsElements) {
      addTransitivePredecessor(p)
      p.addTransitiveSuccessor(this)
      for (s <- transitiveSuccessorsElements) {
        p.addTransitiveSuccessor(s)
      }
    }
    for (s <- transitiveSuccessorsElements) {
      e.addTransitiveSuccessor(s)
      s.addTransitivePredecessor(e)
      for (p <- e.transitivePredecessorsElements) {
        s.addTransitivePredecessor(p)
      }
    }
  }

  var theoreticalLayer: Int = -1

  override def checkInternals(): Unit = {
    nbCheckInternals += 1
  }

  def resetFlags(): Unit = {
    nbCheckInternals = 0
    if (updateRequiredThisPropagation) {
      updateRequiredThisPropagation = false
      updateRequired = false
      nbUpdate = 0
    }
  }

  def checkUpdate: Unit = {
    if (updateRequiredThisPropagation) {
      if (debugLevel > 1 || (debugLevel == 1 && structure.lastPropagationWasTotal))
        assert(
          nbCheckInternals == 1,
          s"Element $name should have been checked during this propagation but has not been"
        )
      else
        assert(
          nbCheckInternals == 0,
          s"Element $name shouldn't have been checked during this propagation but has been"
        )
      assert(
        updateRequired,
        "Problem in the TestPropagationStructure : an update was required in this propagation even if no update where required at all"
      )
      assert(
        nbUpdate == 1,
        s"Variable $name has not been updated the proper number of time (updated $nbUpdate times instead of 1)"
      )
    } else {
      assert(
        nbUpdate == 0,
        s"Variable $name has not been updated the proper number of time (updated $nbUpdate times instead of 0)"
      )
    }
  }

}
