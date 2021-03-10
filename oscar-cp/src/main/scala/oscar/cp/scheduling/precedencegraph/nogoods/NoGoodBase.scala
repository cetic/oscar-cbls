package oscar.cp.scheduling.precedencegraph.nogoods

import oscar.algo.Inconsistency
import oscar.cp.scheduling.precedencegraph.PrecedenceGraphWithNoGoods

class NoGoodBase(nMachine: Int, nActivityByMachine: Int, pgs: Array[PrecedenceGraphWithNoGoods]) {

  val watchers = Array.tabulate(nMachine, nActivityByMachine,nActivityByMachine)((m, t1, t2) => new PrecedenceLiteralWatcher(m,t1,t2))

  //what happens if we know that on machine machine, there is a precedence from from to to
  def inferences(machine: Int, from: Int, to: Int) = {
    val watcher = watchers(machine)(from)(to)
    val watchedList = watcher.watchedList.toArray //TODO: use a fillArray method
    var w = 0
    while(w < watchedList.length) {
      val ng = watchedList(w)
      val otherWatcher = if(ng.watchedLiteral1.equals(watcher)) ng.watchedLiteral2 else ng.watchedLiteral1
      if(!canFindAnotherWatch(ng,watcher)){
        if(pgs(otherWatcher.machine).isPrecNonDetectable(otherWatcher.from, otherWatcher.to) || pgs(otherWatcher.machine).isPrecDetectable(otherWatcher.from, otherWatcher.to))
          throw Inconsistency
        pgs(otherWatcher.machine).addNonDetectablePrecAndUpdateTransitiveClosure(otherWatcher.to, otherWatcher.from)
      }
      w += 1
    }
  }

  def canFindAnotherWatch(ng: NoGoodWithWatchedLiterals, watcher: PrecedenceLiteralWatcher): Boolean = {
    val otherWatcher = if(ng.watchedLiteral1.equals(watcher)) ng.watchedLiteral2 else ng.watchedLiteral1
    val literals = ng.watchedLiterals
    var l = 0
    var newWatcherFound = false
    while(!newWatcherFound && l < literals.length) {
      val potentialNewWatcher = literals(l)
      if(!potentialNewWatcher.equals(otherWatcher)){
        if(!pgs(potentialNewWatcher.machine).isPrecNonDetectable(potentialNewWatcher.from, potentialNewWatcher.to)){
          val realNewWatcher = watchers(potentialNewWatcher.machine)(potentialNewWatcher.from)(potentialNewWatcher.to) //TODO: when we create a new no good, we should use the objects of watchers, no new ones !
          if(ng.watchedLiteral1.equals(watcher)) {
            ng.watchedLiteral1 = realNewWatcher
            realNewWatcher.watch(ng)
            watcher.unwatch(ng)
          }
          else {
            ng.watchedLiteral2 = realNewWatcher
            realNewWatcher.watch(ng)
            watcher.unwatch(ng)
          }
          newWatcherFound = true
        }
      }
      l += 1
    }
    newWatcherFound
  }

  override def toString() : String = {
    var s = ""
     (0 until nMachine).foreach(m => {
      (0 until nActivityByMachine).foreach(t1 => {
        (0 until nActivityByMachine).foreach(t2 => {
          val w = watchers(m)(t1)(t2)
          if(w.watchedList.size > 0) {
            s += s"$w watches : \n"
            w.watchedList.foreach(ng => s += (ng + " %%% "))
            s += "\n"
          }
        })
      })
    })
    s
  }

  def size : Int = {
    var s = 0
    (0 until nMachine).foreach(m => {
      (0 until nActivityByMachine).foreach(t1 => {
        (0 until nActivityByMachine).foreach(t2 => {
          val w = watchers(m)(t1)(t2)
          s += w.watchedList.size
        })
      })
    })
    s/2
  }

//  def remove

}
