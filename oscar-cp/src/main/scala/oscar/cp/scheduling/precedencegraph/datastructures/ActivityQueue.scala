package oscar.cp.scheduling.precedencegraph.datastructures

class ActivityQueue(nActivities: Int) {

  private[this] val a : Array[Int] = Array.ofDim(nActivities)
  private[this] var _size = 0
  private[this] var head = 0
  private[this] var tail = 0

  def enqueue(v: Int): Unit = {
    a(tail) = v
    tail += 1
    if(tail == nActivities)
      tail = 0
    _size += 1
  }

  def dequeue(): Int = {
    val v = a(head)
    head += 1
    if(head == nActivities)
      head = 0
    _size -= 1
    v
  }

  def isEmpty(): Boolean = _size == 0

}
