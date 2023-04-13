package oscar.cbls.algo.pareto

//smaller is better
object ParetoOrder extends Enumeration {
  type ParetoOrder = Value

  //boolean lattice
  //
  //      Incomparable
  //         /   \
  //  Dominates  Dominated
  //        \    /
  //         Equal

  val Dominates,Dominated,Incomparable,Equal = Value

  def union(a:ParetoOrder,b:ParetoOrder):ParetoOrder = {
    a match{
      case Dominates =>
        b match{
          case Dominates => Dominates
          case Dominated => Incomparable
          case Incomparable => Incomparable
          case Equal => Dominates
        }
      case Dominated =>
        b match{
          case Dominates => Incomparable
          case Dominated => Dominated
          case Incomparable => Incomparable
          case Equal => Dominated
        }
      case Incomparable => Incomparable
      case Equal => b
    }
  }

  def inter(a:ParetoOrder,b:ParetoOrder):ParetoOrder = {
    a match{
      case Dominates =>
        b match{
          case Dominates => Dominates
          case Dominated => Equal
          case Incomparable => Dominates
          case Equal => Equal
        }
      case Dominated =>
        b match{
          case Dominates => Equal
          case Dominated => Dominated
          case Incomparable => Dominated
          case Equal => Equal
        }
      case Incomparable => b
      case Equal => Equal
    }
  }

  def compare1(a:Long,b:Long):ParetoOrder = {
    if (a == b) Equal
    else if (a < b) Dominates
    else Dominated
  }

  //dominates,dominating,both
  def compare(key1:Array[Long],key2:Array[Long]):ParetoOrder = {
    var soFar = Equal
    var i = key1.length
    while(i != 0){
      i = i - 1
      soFar = union(compare1(key1(i),key2(i)),soFar)
      if(soFar == Incomparable) return soFar
    }
    soFar
  }
}

import oscar.cbls.algo.pareto.ParetoOrder._

import scala.util.Random


/**
 * this stores a set of pareto points, and for each pareto point, and additional value of type T.
 * only non-dominated points are kept; in case of equality, the last one is kept.
 * lower is better
 * @param nbDimensions
 * @tparam T
 */
abstract class ParetoMap[T](nbDimensions:Int)
  extends Iterable[(Array[Long],T)] {

  /**
   *
   * @param key
   * @param value
   * @return true if it was inserted, false otherwise
   */
  def insert(key:Array[Long],value:T):Boolean

  def content:List[(Array[Long],T)]

  override def iterator: Iterator[(Array[Long], T)] = content.iterator

  def getDominatingOrEqual(key: Array[Long]): List[(Array[Long], T)] = {

    def internalGetDominating(elements: List[(Array[Long], T)]): List[(Array[Long], T)] = {
      if (elements == null) {
        //when we get to here, we might dominate some keys, and we might be incomparable to some others we insert
        Nil
      } else {
        val h = elements.head
        val t = elements.tail
        compare(h._1, key) match {
          case Dominates | Equal =>
            h :: internalGetDominating(t)
          case Dominated | Incomparable =>
            internalGetDominating(t)
        }
      }
    }
    internalGetDominating(content)
  }

  def getDominatedOrEqual(key: Array[Long]): List[(Array[Long], T)] = {

    def internalGetDominated(elements: List[(Array[Long], T)]): List[(Array[Long], T)] = {
      if (elements == null) {
        //when we get to here, we might dominate some keys, and we might be incomparable to some others we insert
        null
      } else {
        val h = elements.head
        val t = elements.tail
        compare(h._1, key) match {
          case Dominated | Equal =>
            h :: internalGetDominated(t)
          case Dominates | Incomparable =>
            internalGetDominated(t)
        }
      }
    }
    internalGetDominated(content)
  }

  def isDominatedOrEqual(key:Array[Long]):Boolean = {
    var x = content
    while (x != null) {
      val h = x.head
      x = x.tail
      compare(h._1, key) match {
        case Dominates | Equal =>
          return true
        case Dominated | Incomparable =>
          ;
      }
    }
    false
  }

  def randomElement: Option[(Array[Long], T)] = {
    val qtNodeList = this.toList
    if(qtNodeList.isEmpty)None
    else{
      val randIndex = Random.nextInt(qtNodeList.length)
      Some(qtNodeList(randIndex))
    }
  }
}


