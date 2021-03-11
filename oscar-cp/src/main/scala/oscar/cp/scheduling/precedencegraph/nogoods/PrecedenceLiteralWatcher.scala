package oscar.cp.scheduling.precedencegraph.nogoods

import scala.collection.mutable

class PrecedenceLiteralWatcher(machine: Int, from: Int, to:Int) extends PrecedenceLiteral(machine, from, to) {

  val watchedList = mutable.HashSet[NoGoodWithWatchedLiterals]()

  def watch(ng: NoGoodWithWatchedLiterals) = watchedList += ng
  def unwatch(ng: NoGoodWithWatchedLiterals) = watchedList -= ng

  override def toString: String = {
    "m-" + machine + " " + from +" -> " + to
  }

  def equals(that: PrecedenceLiteralWatcher): Boolean = machine == that.machine && from == that.from && to == that.to


}
