package oscar.cbls.core.propagation

class TestPropagationStructure extends PropagationStructure(0) {

  private def elements: List[TestPropagationElement] =
    getPropagationElements.map(_.asInstanceOf[TestPropagationElement])

  private def invariants: List[TestInvariantElement] =
    getPropagationElements
      .filter(_.isInstanceOf[TestInvariantElement])
      .map(_.asInstanceOf[TestInvariantElement])

  private def variables: List[TestVariableElement] =
    getPropagationElements.flatMap(e =>
      if (e.isInstanceOf[TestVariableElement]) Some(e.asInstanceOf[TestVariableElement]) else None
    )

  private def names: Map[Int, String] = elements.map(_.id).zip(elements.map(_.name)).toMap

  private var lastTarget: Option[Int] = None

  private var partialPropagationTargetId: List[Int] = List()

  def myToDot(target : Option[TestPropagationElement] = None) : String = {
    //println(target.map(_.theoreticalPredecessors.map(_.name)))

    val labels = elements.map(e => {
      val shapeString = if (e.isInstanceOf[TestInvariantElement]) ",shape=\"box\"" else ""
      val color = target match {
        case None => ""
        case Some(t) => if (t.theoreticalPredecessors.contains(e)) ",color=\"red\"" else ""
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

  // override def registerForPartialPropagation(p: PropagationElement): Unit = {
  //   partialPropagationTargetId = p.id :: partialPropagationTargetId
  //   super.registerForPartialPropagation(p)
  // }

  def validateLayerAssignation: Unit = {
    elements.foreach(_.validateLayer)
  }

  def totalPropagation: Unit = {
    lastTarget = None
    resetUpdateCount
    propagate()
  }

  def partialPropagation(targetId: Int): Unit = {
    lastTarget = Some(targetId)
    resetUpdateCount
    propagate(getPropagationElements(targetId))
  }

  def checkPropagationCount: Unit = {
    val trackAsList = lastTarget match {
      case None => List()
      case Some(id) =>
        if (partialPropagationTargetId.contains(id)) elements(id).theoreticalPredecessors
        else List()
    }

    def checkVariables(v: List[TestVariableElement] = variables): Unit = {
      v match {
        case Nil =>
        case h :: t =>
          if (trackAsList.contains(h)) require(h.nbUpdate == 1) else require(h.nbUpdate == 0)
          checkVariables(t)
      }
    }
  }

  def close: Unit = {
    setupPropagationStructure()
    closed = true
  }

  private def resetUpdateCount = {
    variables.foreach(_.resetUpdate)
  }
}
