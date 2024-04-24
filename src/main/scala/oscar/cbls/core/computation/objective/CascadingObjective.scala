package oscar.cbls.core.computation.objective

/** The CascadingObjective is suited for problem including several strong constraints.
  *
  * It uses a list of individual [[Objective]], usually a list of strong constraints followed by the
  * objective to minimize. It will evaluate the first strong constraint. If it's violated, it stops
  * there, otherwise it checks the next one... It's recommended to put first the fastest to evaluate
  * constraints and finish by the objective to minimize.
  */
object CascadingObjective {

  /** Creates a CascadingObjective based on the given list of [[Objective]]
    *
    * @param objectives
    *   the list of Objectives to consider
    */
  def apply(objectives: List[Objective]): CascadingObjective = {
    require(objectives.nonEmpty, "Building CascadingObjective with an empty list of Objectives")
    require(objectives.size >= 2, "Building CascadingObjective with less than 2 Objectives")

    def buildCascading(objectives: List[Objective], currentDepth: Int): AbstractObjective = {
      objectives match {
        case head :: Nil => head
        case head :: tail =>
          CascadingObjective(head, buildCascading(tail, currentDepth + 1), currentDepth + 1)
      }
    }

    CascadingObjective(objectives.head, buildCascading(objectives.tail, 0), 0)
  }

  /** Creates a CascadingObjective based on the given list of [[Objective]]
    *
    * @param mustBeZeroObjective
    *   The first objective to evaluate.
    * @param secondObjective
    *   The second objective to evaluate
    * @param cascadingDepth
    *   The depth of the CascadingObjective. The first one has a depth of 0 next one, 1 ...
    */
  def apply(
    mustBeZeroObjective: Objective,
    secondObjective: AbstractObjective,
    cascadingDepth: Int
  ): CascadingObjective = {
    new CascadingObjective(mustBeZeroObjective, secondObjective, cascadingDepth)
  }
}

/** A CascadingObjective evaluates its mustBeZeroObjective first and then the secondObjective.
  *
  * If the mustBeZeroObjective is not equal to zero, the global resulting value is Long.MaxValue.
  * Else it's the value of the secondObjective.
  *
  * @param mustBeZeroObjective
  *   The first evaluated Objective. Usually a strong constraint
  * @param secondObjective
  *   The second Objective to evaluate
  * @param depth
  *   The depth of the CascadingObjective. The first one has a depth of 0 next one, 1 ...
  */
class CascadingObjective(
  mustBeZeroObjective: Objective,
  secondObjective: AbstractObjective,
  depth: Int
) extends AbstractObjective(mustBeZeroObjective.model) {

  private val baseIndent: String = "\t" * depth
  override def value: Long = {
    if (mustBeZeroObjective.value != 0) Long.MaxValue
    else secondObjective.value
  }

  def detailedString(short: Boolean): String = {
    if (short) {
      if (mustBeZeroObjective.value == 0L) {
        s"${baseIndent}CascadingObjective(\n" +
          s"$baseIndent\tmustBeZeroObjective :=0L\n" +
          s"$baseIndent\tsecondObjective:${secondObjective.detailedString(short)}\n" +
          s"$baseIndent)"
      } else {
        s"${baseIndent}CascadingObjective(\n" +
          s"$baseIndent\tmustBeZeroObjective:${mustBeZeroObjective.detailedString(short)}\n" +
          s"$baseIndent)"
      }
    } else {
      s"${baseIndent}CascadingObjective(\n" +
        s"$baseIndent\tmustBeZeroObjective:${mustBeZeroObjective.detailedString(short)}\n" +
        s"$baseIndent\tsecondObjective:${secondObjective.detailedString(short)}\n" +
        s"$baseIndent)"
    }
  }

  override def toString: String = detailedString(false)
}
