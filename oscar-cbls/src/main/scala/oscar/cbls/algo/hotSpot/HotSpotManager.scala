package oscar.cbls.algo.hotSpot

import oscar.cbls.algo.quick.QList

//TODO Test this or deprecate
class HotSpotManager(maxValue:Int) {

  var explorers:List[HotSpotExplorer] = List.empty
  def newExplorer:HotSpotExplorer = {
    val toReturn = new HotSpotExplorer(maxValue)
    explorers = toReturn :: explorers
    toReturn
  }

  def enqueue(value:Int): Unit ={
    for(e <- explorers){
      e.enqueue(value)
    }
  }
  def enqueueAll(): Unit = {
    for(w <- 0 to maxValue) enqueue(w)
  }
}

class HotSpotExplorer(maxValue:Int) extends Iterable[Long] {

  override def iterator: Iterator[Long] = new HotSpotExplorerIterator(this)

  var toExplore:QList[Int] = _
  val isMarked:Array[Boolean] = Array.fill(maxValue+1)(false)

  def enqueue(value:Int): Unit ={
    if(!isMarked(value)){
      isMarked(value) = true
      toExplore = QList(value,toExplore)
    }
  }
}

class HotSpotExplorerIterator(base:HotSpotExplorer) extends Iterator[Long] {
  override def hasNext: Boolean = {
    base.toExplore != null
  }

  override def next(): Long = {
    val current = base.toExplore.head
    base.isMarked(current) = false
    base.toExplore = base.toExplore.tail
    current
  }
}
