package oscar.algo.branchings

import oscar.algo.reversible.{ReversibleInt, ReversibleSparseSet}
import oscar.algo.search._
import oscar.algo.vars.{BoolVarLike, IntVarLike}

/**
  * This search branches on boolean selection variables linked to optional variables:
  * When branching on a selection variable, it creates two child nodes, left=true, right=false.
  * On the true (left) branch, all the optional variables must be assigned before attempting a next selection.
  * On the false (right) branch, the optional variables are not branched on at all.
  * It tries to maximize the number of selection variables set to true.
  *
  * @param selections The OptionalSelection objects linking the boolean selection variables with their optional variables
  * @param selectionHeuristic The branching to be used on the selection variables
  *
  * @author Pierre Schaus  pschaus@gmail.com
  * @author Charles Thomas cftmthomas@gmail.com
  */
class MaxSelectionBranching[B <: BoolVarLike, I <: IntVarLike](selections: Seq[OptionalSelection[B, I]], selectionHeuristic: Branching) extends Branching {

  require(selections.nonEmpty)

  private[this] val context = selections.head.selectionVar.context

  private[this] val currentSelection = new ReversibleInt(context, -1) //Index of current branching var (-1 if none)
  private[this] val unbound = new ReversibleSparseSet(context, 0, selections.length-1) //Unbound variables

  //Finds last var bound to true if any
  private def findNextI: Int = {
    var next = -1
    val unboundVars = unbound.toArray
    var j = unboundVars.length - 1
    while (next < 0 && j >= 0) {
      val selection = selections(unboundVars(j)).selectionVar
      if (selection.isBound) {
        if (selection.isTrue) next = unboundVars(j)
        else unbound.removeValue(unboundVars(j))
      }
      j -= 1
    }
    next
  }

  override def alternatives(): Seq[Alternative] = {

    var selectNextSelection = false //Indicates if we need to make a selection decision

    //While we need to branch on optional vars:
    while (!selectNextSelection) {

      //If selection var already selected:
      if (currentSelection.value >= 0) {
        val toBranchOn = selections(currentSelection.value).optVarsHeuristic() //Getting branching in sub-tree
        if (toBranchOn.nonEmpty) return toBranchOn //If branching still possible: returning it
        else currentSelection.setValue(-1) //Else: indicating that we need to find the next var that has been bound
      }

      //If no selection var selected:
      else {
        val next = findNextI //Finding next var bound to true
        if (next >= 0) {
          unbound.removeValue(next)
          currentSelection.setValue(next)
        }
        else selectNextSelection = true //If no var found: ending loop
      }
    }

    selectionHeuristic.alternatives() //Branching on selection vars
  }
}

object MaxSelectionBranching {
  def apply[B <: BoolVarLike, I <: IntVarLike](selections: Seq[OptionalSelection[B, I]], selectionHeuristic: Branching) =
    new MaxSelectionBranching(selections, selectionHeuristic)
}
