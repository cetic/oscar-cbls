package oscar.cbls.core.propagation

import scala.annotation.tailrec

abstract class TestPropagationElement(structure: TestPropagationStructure)
    extends PropagationElement(structure) {

  val name: String

  def isOutput : Boolean = {staticallyListeningElements.isEmpty}

  def validateLayer: Unit = {
    assert(theoreticalLayer_ == layer, s"On $name: The layer computed by the layer computing algorithm is not coherent with the layer computed by construction (theoreticalLayer : $theoreticalLayer_, algorithm layer : $layer)")
  }

  private var theoreticalPredecessors_ : List[TestPropagationElement] = List()

  def theoreticalPredecessors = theoreticalPredecessors_

  protected def addElementInTrack(e: TestPropagationElement) = {
    @tailrec
    def computeNewTrack(
      e: TestPropagationElement = e,
      currentTrack: List[TestPropagationElement] = theoreticalPredecessors_,
      onewTrack: List[TestPropagationElement] = List()
    ): List[TestPropagationElement] = {
      currentTrack match {
        case Nil => e :: newTrack
        case h :: t =>
          if (h == e) computeNewTrack(e, t, newTrack) else computeNewTrack(e, t, h :: newTrack)
      }
    }
    theoreticalPredecessors_ = computeNewTrack(e,e.theoreticalPredecessors.foldLeft(theoreticalPredecessors_)((pred,e) => computeNewTrack(e,pred)))
    //println(s"$name -> ${theoreticalPredecessors_.map(_.name)}")
  }

  private var theoreticalLayer_ : Int = -1

  def theoreticalLayer_=(value: Int) = theoreticalLayer_ = value

  def theoreticalLayer: Int = theoreticalLayer_

  override def checkInternals(): Unit = {}

}
