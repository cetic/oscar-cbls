package oscar.cbls.core.propagation

class TestInvariantElement(structure: TestPropagationStructure)
    extends TestPropagationElement(structure) {

  override val name: String = s"Invariant ${this.id}"

  override def checkInternals(): Unit = {}

  override def performPropagation(): Unit = {
    require(false, "This should not be called")
  }

  def notifyVarChange() = {
    for (v <- staticallyListeningElements)
      v.asInstanceOf[TestVariableElement].update()
  }

  def registerStaticAndDynamicDependency(elem: TestVariableElement) = {
    addElementInTrack(elem)
    registerStaticallyListenedElement(elem)
  }

}


