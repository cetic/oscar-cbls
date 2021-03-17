package oscar.cp.scheduling.precedencegraph.branching

import oscar.algo.search.Branching
import oscar.cp._

/**
  * Created by saschavancauwelaert on 23/03/2017.
  */
class AssignAllVarsToMin(vars: Seq[CPIntVar]) extends Branching {

  val nVars = vars.size
  val store = vars(0).store

  private[this] var nAssignedVars = 0

  final override def alternatives(): Seq[Alternative] = {

    nAssignedVars = 0
    while (nAssignedVars < nVars && vars(nAssignedVars).isBound) nAssignedVars += 1

    if(nAssignedVars == nVars)
      noAlternative
    else {
      val onlyBranch : () => Unit = () => {
        //TODO: the reversible here is not working for some reason
//        var i = nAssignedVars
        var i = 0
        while(i < nVars){
          store.post(vars(i) === vars(i).min)
          i += 1
        }
      }
      Seq(onlyBranch)
    }
  }
}
