package oscar.cp.scheduling.precedencegraph.nogoods

class NoGood(val lits : Seq[PrecedenceLiteral]) {
  val literals : Array[_ <: PrecedenceLiteral] = lits.toArray

  def size = literals.length
}
