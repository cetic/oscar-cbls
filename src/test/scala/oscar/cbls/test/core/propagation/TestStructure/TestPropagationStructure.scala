package oscar.cbls.core.propagation

/** A propagation structure aimed at testing the propagation functions.
  *
  * The idea of this structure is the following:
  *   - There are two types of elements: invariants and variables
  *   - The invariant have variables as input
  *   - Variables can be defined as invariants (they are the output of the invariant)
  *
  * In this structure, the invariants and variables do not compute anything; the variables can be
  * updated (whatever an update means...); when a variable is updated, it is registered for
  * propagation; propagating a variable means notifying the listening invariants of this variable;
  * when a invariant is notified by an input variable, it updates its output variable
  *
  * Moreover, the structure maintains flags to know which elements need to be updated during a
  * propagation and checks if the number of time each element is updated.
  *
  * @param debugLevel
  *   the debug level (same debug level as the propagation structure)
  */

class TestPropagationStructure(val debugLevel: Int = 0) extends PropagationStructure(debugLevel) {

  // The elements of the propagation structure
  def elements: List[TestPropagationElement] =
    getPropagationElements.map(_.asInstanceOf[TestPropagationElement])

  // The invariants of the propagation structure
  def invariants: List[TestInvariantElement] =
    getPropagationElements
      .filter(_.isInstanceOf[TestInvariantElement])
      .map(_.asInstanceOf[TestInvariantElement])

  // The variables of the propagation structure
  def variables: List[TestVariableElement] =
    getPropagationElements.flatMap(e =>
      e match {
        case element: TestVariableElement => Some(element)
        case _ => None
      }
    )

  private def names: Map[Int, String] = elements.map(_.id).zip(elements.map(_.name)).toMap

  private var currentTarget: Option[TestPropagationElement] = None

  private var partialPropagationTarget: List[TestPropagationElement] = List()

  // A method that allow to compute a dot file for this test structure
  def myToDot(target: Option[TestPropagationElement] = None): String = {
    val labels = elements
      .map(e => {
        val shapeString = if (e.isInstanceOf[TestInvariantElement]) ",shape=\"box\"" else ""
        val color = target match {
          case None => ""
          case Some(t) =>
            if (t.transitivePredecessors.contains(e)) {
              ",color=\"red\""
            } else {
              if (t.transitiveSuccessors.contains(e))
                ",color=\"green\""
              else {
                if (e == t)
                  ",color=\"yellow\""
                else
                  ""
              }
            }
        }
        s"${e.id} [label=\"${e.name}\"$shapeString$color];"
      })
      .mkString("\n    ")
    val path = pathList
    s"""digraph PropagationStructure {
    $labels
    ${path.map(p => p.mkString("->")).mkString(";\n    ")}
}
"""
  }

  override def registerForPartialPropagation(p: PropagationElement): Unit = {
    partialPropagationTarget = p.asInstanceOf[TestPropagationElement] :: partialPropagationTarget
    super.registerForPartialPropagation(p)
  }

  def validateLayerAssignation: Unit = {
    elements.foreach(_.validateLayer)
  }

  def lastPropagationWasTotal: Boolean = {
    currentTarget.isEmpty
  }

  private def setUpdateRequired(): Unit = {
    for (e <- elements) {
      currentTarget match {
        case None => e.updateRequiredThisPropagation = e.updateRequired
        case Some(target) =>
          if (e == target)
            e.updateRequiredThisPropagation = e.updateRequired
          if (target.transitivePredecessors.contains(e))
            e.updateRequiredThisPropagation = e.updateRequired
      }
    }
  }

  private def mkPropagation(reset: Boolean): Unit = {
    setUpdateRequired()
    propagate(currentTarget)
    checkPropagationCount
    if (reset)
      resetPropagationFlags()
  }

  /** Makes a total propagation
    *
    * @param reset
    *   If reset is true, the propagation flags are reset after the propagation (they cannot be
    *   accessed later, e.g. for tests)
    */
  def totalPropagation(reset: Boolean = true): Unit = {
    currentTarget = None
    mkPropagation(reset)
  }

  /** Makes a partial propagation up to a given target
    *
    * @param target
    *   The target
    * @param reset
    *   If reset is true, the propagation flags are reset after the propagation (they cannot be
    *   accessed later, e.g. for tests)
    */
  def partialPropagation(target: TestPropagationElement, reset: Boolean = true): Unit = {
    currentTarget =
      if (partialPropagationTarget.contains(target) && debugLevel < 3) Some(target) else None
    mkPropagation(reset)
  }

  private def checkPropagationCount: Unit = if (closed) elements.foreach(_.checkUpdate)

  /** resets the propagation flags */
  def resetPropagationFlags(): Unit =
    elements.foreach(_.resetFlags())

  /** Closes the propagation structure */
  def close(): Unit = {
    setupPropagationStructure()
    closed = true
  }
}
