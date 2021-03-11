package oscar.cp.scheduling.precedencegraph.datastructures

class IncreasingSet(maxSize: Int) {

  private[this] val values = Array.tabulate(maxSize)(i => i)
  private[this] val positions = Array.tabulate(maxSize)(i => i)

  private[this] val tmpArray : Array[Int] = Array.ofDim(maxSize)
  private[this] var _size = 0

  def size = _size

  def this(initialValue: Int, maxSize: Int) {
    this(maxSize)
    add(initialValue)
  }

  def clear() : Unit = _size = 0
  def hasValue(v: Int) : Boolean = positions(v) < size
  def add(v: Int) = {
    if(!hasValue(v)) {
      val oldPosV = positions(v)
      val oldSize = size
      val swappedValue = values(oldSize)

      values(oldSize) = v
      values(oldPosV) = swappedValue

      positions(v) = oldSize
      positions(swappedValue) = oldPosV

      _size += 1
    }
  }

  def addAll(set : IncreasingSet) = {
    val nNewValues = set.fillArray(tmpArray)
    var i = 0
    while(i < nNewValues) {
      add(tmpArray(i))
      i += 1
    }
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

}
