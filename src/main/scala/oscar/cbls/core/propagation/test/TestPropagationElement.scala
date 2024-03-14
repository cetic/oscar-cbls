package oscar.cbls.core.propagation

import scala.annotation.tailrec

abstract class TestPropagationElement(structure: TestPropagationStructure)
    extends PropagationElement(structure) {

  var updateRequired : Boolean = false
  var updateRequiredThisPropagation : Boolean = false
  var nbUpdate : Int = 0

  val name: String

  def isOutput : Boolean = {staticallyListeningElements.isEmpty}

  def isInput : Boolean = staticallyListenedElements.isEmpty

  def validateLayer: Unit = {
    assert(theoreticalLayer == layer, s"On $name: The layer computed by the layer computing algorithm is not coherent with the layer computed by construction (theoreticalLayer : $theoreticalLayer, algorithm layer : $layer)")
  }

  private var transitivePredecessorsElements : List[TestPropagationElement] = List()

  private var transitiveSuccessorsElements : List[TestPropagationElement] = List()

  def transitivePredecessors : List[TestPropagationElement] = transitivePredecessorsElements

  def transitiveSuccessors : List[TestPropagationElement] = transitiveSuccessorsElements

  override def registerStaticallyListenedElement(e : PropagationElement) = {
    super.registerStaticallyListenedElement(e)
    addTransitiveDependency(e.asInstanceOf[TestPropagationElement])
  }

  private def addTransitivePredecessor(e : TestPropagationElement) = {
    transitivePredecessorsElements = addElementInList(e,transitivePredecessorsElements)
  }

  private def addTransitiveSuccessor(e : TestPropagationElement) = {
    transitiveSuccessorsElements = addElementInList(e,transitiveSuccessorsElements)
  }

  @tailrec
  private[this] def addElementInList(e : TestPropagationElement,
    currentList : List[TestPropagationElement] = transitivePredecessorsElements,
    newList:List[TestPropagationElement] = List()) : List[TestPropagationElement] = {
    currentList match {
      case Nil => e :: newList
      case h :: t => if (h == e) addElementInList(e,t,newList) else addElementInList(e,t,h::newList)
    }
  }

  def addTransitiveDependency(e : TestPropagationElement) = {
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

  var theoreticalLayer : Int = -1

  override def checkInternals(): Unit = {}

  def resetFlags : Unit = {
    if (updateRequiredThisPropagation) {
      updateRequiredThisPropagation = false
      updateRequired = false
      nbUpdate = 0
    }
  }

  def checkUpdate : Unit = {
    if (updateRequiredThisPropagation) {
      assert(updateRequired,"Problem in the TestPropagationStructure : an update was required in this propagation even if no update where required at all")
      assert(nbUpdate == 1,s"Variable $name has not been updated the proper number of time (updated $nbUpdate times instead of 1)")
    } else {
      assert(nbUpdate == 0,s"Variable $name has not been updated the proper number of time (updated $nbUpdate times instead of 0)")
    }
  }

}
