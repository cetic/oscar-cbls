package oscar.cp.core.variables

trait CPIntVarViewLinear extends CPIntVar {
  def linearView: (Int, Int, CPIntVar)
}
