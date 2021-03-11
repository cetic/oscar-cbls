package oscar.algo.paretofront

import oscar.util.RandomGenerator

class LinearList[U: Numeric, T <: ParetoElement[U]] extends ParetoFront[U, T] with Iterable[T] {
  var elements = List[T]()
  
  /** Attempts to insert an element in the Pareto front and modifies the tree accordingly */
  def insert(candidate: T): Unit = {
    def newArchive(arch: List[T], dom: Boolean, ls: List[T]): List[T] = {
      arch match {
        case head :: tail => {
          val domi = candidate.dominance(head)
          if (domi == -1) arch ::: ls
          else if (domi == 0) newArchive(tail, dom, head :: ls) 
          else newArchive(tail, true, ls)
        }
        case Nil => {
          priorityQueue.enqueue(candidate)
          candidate :: ls
        }
      }
    }
    elements = newArchive(elements, false, Nil)
  }
  
  /** Returns a Set containing the evaluations of the non-dominated elements in the Pareto front */
  def getEvaluations(): Set[Array[Double]] = {
    elements.map((e: T) => e.objectives).toSet
  }
  
  override def foreach[E](f: T => E): Unit = {
    elements.foreach(f)
  }

  override def iterator: Iterator[T] = elements.iterator
  
  def randomElement: T = {
    val randIndex = RandomGenerator.nextInt(elements.length)
    elements(randIndex)
  }
  
  def score[T1 <: ParetoElement[U]](candidate: T1): Int = {
    var acc = 0
    for (elem <- elements) {
      val domi = candidate.dominance(elem)
      if (domi > 0) acc += 1
      else if (domi < 0) acc -= 1
    }
    acc
  }
  
  def contains[T1 <: ParetoElement[U]](elem: T1): Boolean = {
    for (element <- elements) {
      if (elem.objectives == element.objectives) return true
    }
    false
  }
}

object LinearList {
  def apply[U: Numeric, T <: ParetoElement[U]]() = new LinearList[U, T]()
}

object LinearListDouble {
  def apply[T <: ParetoElement[Double]]() = new LinearList[Double, T]()
}

object LinearListInt {
  def apply[T <: ParetoElement[Int]]() = new LinearList[Int, T]()
}