package oscar.cbls.core.propagation

class TestPropagationStructure extends PropagationStructure(0) {

  def elements: List[TestPropagationElement] =
    getPropagationElements.map(_.asInstanceOf[TestPropagationElement])

  def invariants: List[TestInvariantElement] =
    getPropagationElements
      .filter(_.isInstanceOf[TestInvariantElement])
      .map(_.asInstanceOf[TestInvariantElement])

  def variables: List[TestVariableElement] =
    getPropagationElements.flatMap(e =>
      if (e.isInstanceOf[TestVariableElement]) Some(e.asInstanceOf[TestVariableElement]) else None
    )

  private def names: Map[Int, String] = elements.map(_.id).zip(elements.map(_.name)).toMap

  private var currentTarget: Option[TestPropagationElement] = None

  private var partialPropagationTarget: List[TestPropagationElement] = List()

  def myToDot(target : Option[TestPropagationElement] = None) : String = {
    //println(target.map(_.theoreticalPredecessors.map(_.name)))

    val labels = elements.map(e => {
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
    }).mkString("\n    ")
    val path = pathList
    s"""digraph PropagationStructure {
    $labels
    ${path.map(p => p.mkString("->")).mkString(";\n    ")}
}
"""
  }

  override def toDot(
    names: Map[Int, String] = names,
    shapes: Map[Int, String] = invariants.map(e => (e.id, "box")).toMap
  ): String = {
    super.toDot(names, shapes)
  }

  override def registerForPartialPropagation(p: PropagationElement): Unit = {
    partialPropagationTarget = p.asInstanceOf[TestPropagationElement] :: partialPropagationTarget
    super.registerForPartialPropagation(p)
  }

  def validateLayerAssignation: Unit = {
    elements.foreach(_.validateLayer)
  }

  private def setUpdateRequired : Unit = {
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

  private def mkPropagation(reset : Boolean) : Unit = {
    setUpdateRequired
    // printUpdates
    propagate(currentTarget.getOrElse(null))
    // printUpdates
    checkPropagationCount
    if (reset)
      resetPropagationFlags
  }

  def totalPropagation(reset : Boolean = true): Unit = {
    currentTarget = None
    mkPropagation(reset)
  }

  def partialPropagation(target : TestPropagationElement,reset : Boolean = true): Unit = {
    currentTarget = if (partialPropagationTarget.contains(target)) Some(target) else None
    mkPropagation(reset)
  }

  private def checkPropagationCount: Unit = elements.foreach(_.checkUpdate)

  def resetPropagationFlags : Unit =
    elements.foreach(_.resetFlags)

  def close: Unit = {
    setupPropagationStructure()
    closed = true
  }

  def printUpdates = {
    println(elements.sortBy(_.id).map(e => s"${e.name}: (Update:${e.updateRequired},UpdateThisTime:${e.updateRequiredThisPropagation},nbUpdate:${e.nbUpdate})").mkString("\n"))
  }
}
