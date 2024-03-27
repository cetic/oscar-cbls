package oscar.cbls.core.propagation

class TestVariableElement(structure: TestPropagationStructure)
    extends TestPropagationElement(structure) {

  override val name: String = s"Variable $id"

  override def performPropagation(): Unit = {
    nbUpdate += 1
    notifyChange()
  }

  def update(): Unit = {
    updateRequired = true
    for (s <- transitiveSuccessors) {
      s match {
        case v: TestVariableElement => v.updateRequired = true
        case _                      =>
      }
    }
    scheduleForPropagation()
  }

  def notifyChange(): Unit = {
    for (v <- dynamicallyListeningElement)
      v.asInstanceOf[TestInvariantElement].notifyVarChange()
  }

  def setDefiningInvariant(inv: TestInvariantElement): Unit = {
    registerStaticallyListenedElement(inv)
  }

}
