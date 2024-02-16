package oscar.cbls.core.propagation

import scala.annotation.tailrec

abstract class TestPropagationElement(structure: TestPropagationStructure)
    extends PropagationElement(structure) {

  val name: String

  def isOutput : Boolean = {staticallyListeningElements.isEmpty}

  def validateLayer: Unit = {
    require(theoreticalLayer_ == layer, "Invalid layer")
  }

  private var theoreticalPredecessors_ : List[TestPropagationElement] = List()

  def theoreticalPredecessors = theoreticalPredecessors_

  protected def addElementInTrack(e: TestPropagationElement) = {
    @tailrec
    def computeNewTrack(
      e: TestPropagationElement = e,
      currentTrack: List[TestPropagationElement] = theoreticalPredecessors_,
      newTrack: List[TestPropagationElement] = List()
    ): List[TestPropagationElement] = {
      currentTrack match {
        case Nil => newTrack
        case h :: t =>
          if (h == e) computeNewTrack(e, t, newTrack) else computeNewTrack(e, t, h :: newTrack)
      }
    }
    theoreticalPredecessors_ = computeNewTrack()
  }

  private var theoreticalLayer_ : Int = -1

  def theoreticalLayer_=(value: Int) = theoreticalLayer_ = value

  def theoreticalLayer: Int = theoreticalLayer_

  override def checkInternals(): Unit = {}

}
