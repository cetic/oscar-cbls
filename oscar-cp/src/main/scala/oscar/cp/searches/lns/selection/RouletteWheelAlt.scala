package oscar.cp.searches.lns.selection

import oscar.cp.searches.lns.operators.ALNSElement

import scala.collection.mutable
import scala.util.Random

/**
 * The purpose of the Roulette wheel is to select randomly an element based on adaptive probabilities.
 */
class RouletteWheelAlt[T <: ALNSElement](
                                       val elems: Array[T],
                                       val weights: Array[Double],
                                       var decay: Double,
                                       val reversed: Boolean,
                                       var perfMetric: (T) => Double
                      ) extends AdaptiveStore[T]{

  private val lastSelected = mutable.HashMap[T, Int]() //caching for quick access to last selected elem(s)
  private val active = mutable.HashSet[Int](elems.indices: _*)

  private var avgWeight = if(active.nonEmpty) active.map(weights(_)).sum / active.size else 0.0

  private def defWeight(index: Int): Double = {
    if(elems(index).execs > 0) weights(index) else avgWeight
  }

  def this(elems: Array[T], rFactor: Double, reversed:Boolean, perfMetric: (T) => Double){
    this(elems, Array.fill(elems.length){(Double.MaxValue - 1) / elems.length.toDouble}, rFactor, reversed, perfMetric)
  }

  private def getIndex(elem: T): Int = lastSelected.getOrElse(elem, elems.indexOf(elem))

  private def isActive(index: Int): Boolean = active.contains(index)

  private def isCached(index: Int): Boolean = lastSelected.contains(elems(index))

  private def processProbas(activeSeq: Seq[Int]): Seq[Double] = {
    val activeWeights = activeSeq.map(i => defWeight(i))
    var wSum = activeWeights.sum
    if(wSum.isInfinity) wSum = Double.MaxValue - 1

    //If all weights are at 0, giving equiprobability to all elems:
    if(wSum == 0.0) Seq.fill(activeSeq.length)(1.0/activeSeq.length)

    //If probabilities must be reversed, reversing weights:
    else if (reversed) {
      val reversedWeights = activeWeights.map(1.0 - _/wSum)
      val rwSum = reversedWeights.sum
      if(rwSum == 0.0) Seq.fill(activeSeq.length)(1.0/activeSeq.length)
      else reversedWeights.map(_  / rwSum)
    }

    else activeWeights.map(_ / wSum)
  }

  override def select(): T = {
    val rand = Random.nextDouble()
    val activeSeq = active.toSeq

    // Computing cumulative probabilities and returning first index for which it is greater than rand
    val selected = activeSeq(processProbas(activeSeq).scanLeft(0.0)(_+_).drop(1).indexWhere(_ > rand))
    lastSelected += elems(selected) -> selected
    elems(selected)
  }

  //TODO: Use listeners
  override def adapt(elem: T): Double = {
    val index = getIndex(elem)
    if(index == -1) throw  new Exception("Element " + elem + " is not in store.")
    //Warning: received val is divided by the number of elems to avoid overflow when processing max values!
    weights(index) = /*if(elem.execs > 1) */(1.0 - decay) * weights(index) + decay * (perfMetric(elem) / elems.length.toDouble)
    /*else perfMetric(elem) / elems.length.toDouble*/
    //    println("elem " + elem + " has now weight: " + weights(index))
    if(isCached(index)) lastSelected.remove(elem)
    if(!elem.isActive){
      deactivate(index)
      avgWeight = if(active.nonEmpty) active.map(weights(_)).sum / active.size else 0.0
      -1.0
    }
    else{
      avgWeight = if(active.nonEmpty) active.map(weights(_)).sum / active.size else 0.0
      weights(index) * elems.length
    }
  }

  override def getElements: Seq[T] = elems

  override def nElements: Int = elems.length

  override def getActive: Iterable[T] = active.map(elems(_))

  override def nActive: Int = active.size

  override def isActiveEmpty: Boolean = active.isEmpty

  override def nonActiveEmpty: Boolean = !isActiveEmpty

  def deactivate(index: Int): Unit = {
    if(index == -1) throw new Exception("Element " + elems(index) + " is not in store.")
    active.remove(index)
    if(isCached(index)) lastSelected.remove(elems(index))
  }

  override def deactivate(elem: T): Unit = deactivate(getIndex(elem))

  override def reset(): Unit = {
    weights.indices.foreach(i => weights(i) = perfMetric(elems(i)))
    lastSelected.clear()
    active.clear()
    active ++= elems.indices.filter(elems(_).isActive)
  }

  override def getScore(elem: T): Double = weights(getIndex(elem))
}
