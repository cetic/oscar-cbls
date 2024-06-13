package oscar.cbls.core.propagation

class TestInvariantElement(structure: TestPropagationStructure)
    extends TestPropagationElement(structure) {

  override val name: String = s"Invariant ${this.id}"

  override def checkInternals(): Unit = {}

  def notifyVarChange(): Unit = {
    for (v <- dynamicallyListeningElement)
      v.asInstanceOf[TestVariableElement].update()
  }

  // def registerStaticAndDynamicDependency(elem: TestVariableElement): Unit = {
  //   registerStaticallyListenedElement(elem)
  // }

}
