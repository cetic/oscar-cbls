package oscar.cp.scheduling.precedencegraph.nogoods

import oscar.cp.scheduling.precedencegraph.PrecedenceGraphWithNoGoods
import oscar.cp.scheduling.precedencegraph.branching.PrecGraphDecisionForNoGood

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class NoGoodRecorder(nMachine: Int, nActivityByMachine: Int, pgs: Array[PrecedenceGraphWithNoGoods]) {

  val base = new NoGoodBase(nMachine, nActivityByMachine, pgs)
  val unaryBase = mutable.Set[PrecedenceLiteralWatcher]() //used to remove impossible precedences at the root node

  def storeNoGoods(branch: Array[PrecGraphDecisionForNoGood], maxNoGoodLength: Int = Int.MaxValue) = {
    val delta = ArrayBuffer[PrecedenceLiteralWatcher]()

    var i = 0
    while(i < branch.length && delta.length < maxNoGoodLength) {
      val decision = branch(i)
      if(decision.isLeftDecision) {
        delta += new PrecedenceLiteralWatcher(decision.machineIndex, decision.firstTask, decision.secondTask)
      }
      else {
          if(delta.isEmpty) {
            unaryBase.add(new PrecedenceLiteralWatcher(decision.machineIndex, decision.secondTask, decision.firstTask))
          }
          else {
            delta += new PrecedenceLiteralWatcher(decision.machineIndex, decision.secondTask, decision.firstTask)
//            if(delta.size >= 2) {
              val noGood = new NoGoodWithWatchedLiterals(delta.toSeq)
              val l1 = noGood.watchedLiteral1
              val l2 = noGood.watchedLiteral2
              base.watchers(l1.machine)(l1.from)(l1.to).watch(noGood)
              base.watchers(l2.machine)(l2.from)(l2.to).watch(noGood)
//            }
          }
        }
      i += 1
      }
    }
}
