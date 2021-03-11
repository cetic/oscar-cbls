package oscar.cp.scheduling.precedencegraph.datastructures

import oscar.algo.reversible.{ReversibleContext, ReversibleInt}


//reversible set of integers only with additions
class ReversibleSetOfActivities(s: ReversibleContext, nActivities: Int) {

  private[this] val _size = new ReversibleInt(s, 0) //the set is empty at construction
  private[this] val values = Array.tabulate(nActivities)(i => i)
  private[this] val positions = Array.tabulate(nActivities)(i => i)

  def size: Int = _size.value
  def hasValue(v: Int) : Boolean = positions(v) < size
  def add(v: Int) = if(!hasValue(v)) {
    val oldPosV = positions(v)
    val oldSize = size
    val swappedValue = values(oldSize)

    values(oldSize) = v
    values(oldPosV) = swappedValue

    positions(v) = oldSize
    positions(swappedValue) = oldPosV

    _size.incr()
  }

  def fillArray(a: Array[Int]) : Int = {
    var i = 0
    val currentSize = size
    while(i < currentSize){
      a(i) = values(i)
      i += 1
    }
    currentSize
  }

  override def toString() : String = {
    values.take(size).mkString(",") + "||" + values.drop(size).mkString(",")
  }

}