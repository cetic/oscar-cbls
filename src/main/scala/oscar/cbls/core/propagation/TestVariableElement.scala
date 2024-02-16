package oscar.cbls.core.propagation

class TestVariableElement(structure: TestPropagationStructure)
    extends TestPropagationElement(structure) {

  private var nbUpdate_ = 0

  def resetUpdate: Unit = {
    nbUpdate_ = 0
  }

  def nbUpdate : Int = nbUpdate_


  override val name: String = s"Variable $id"

  override def performPropagation(): Unit = {
    nbUpdate_ += 1
    notifyChange()
  }

  def update() = {
    scheduleForPropagation()
  }

  def notifyChange() = {
    for (v <- staticallyListeningElements)
      v.asInstanceOf[TestInvariantElement].notifyVarChange()
  }

  def setDefiningInvariant(inv: TestInvariantElement) = {
    registerStaticallyListenedElement(inv)
  }

}
