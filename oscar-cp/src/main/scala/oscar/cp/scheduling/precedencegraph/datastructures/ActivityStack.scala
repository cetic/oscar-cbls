package oscar.cp.scheduling.precedencegraph.datastructures

class ActivityStack(capacity: Int) {

  private[this] val a : Array[Int] = Array.ofDim(capacity)
  private[this] var _size = 0

  def push(v: Int): Unit = {
    a(_size) = v
    _size += 1
  }

  def pop(): Int = {
    val v = a(_size - 1)
    _size -= 1
    v
  }

  def peek(): Int = a(_size - 1)

  def isEmpty(): Boolean = _size == 0

}
