package oscar.cp.scheduling.precedencegraph.nogoods

/**
  * Created by saschavancauwelaert on 27/04/16.
  */

class NoGoodManager(base: NoGoodBase) {
  def precedenceInference(machine: Int, from: Int, to: Int): Unit = {
    base.inferences(machine,from,to)
  }
}