package oscar.algo.search

import oscar.algo.vars.{BoolVarLike, IntVarLike}

/**
  * Represents a dependency in the search between a boolean selection variable and
  * other optional variables: we want to branch on the optional variables only if the selection variable is set to true.
  *
  * @param selectionVar The boolean selection variable
  * @param optionalVars The optional variables
  * @param optVarsBranching The branching to be used on the optional variables
  *
  * @author Pierre Schaus  pschaus@gmail.com
  * @author Charles Thomas cftmthomas@gmail.com
  */
class OptionalSelection[B <: BoolVarLike, I <: IntVarLike](
                                                            val selectionVar: B,
                                                            val optionalVars: Seq[I],
                                                            val optVarsBranching: Branching
                                                         ) {

  def optVarsHeuristic(): Seq[Alternative] = optVarsBranching.alternatives()
}

object OptionalSelection{
  def apply[B <: BoolVarLike, I <: IntVarLike](selectionVar: B, optionalVars: Seq[I], optVarsBranching: Branching) =
    new OptionalSelection(selectionVar, optionalVars, optVarsBranching)
}