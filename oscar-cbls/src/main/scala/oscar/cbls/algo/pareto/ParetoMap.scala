package oscar.cbls.algo.pareto

//smaller is better!!
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

import ParetoOrder._
import oscar.cbls.algo.quick.QList

/**
 * this stores a set of pareto points, and for each pareto point, and additional value of type T.
 * only non-dominated points are kept; in case of equality, the last one is kept.
 * lower is better
 * @param n
 * @tparam T
 */
abstract class ParetoMap[T](n:Int) {

  /**
   *
   * @param key
   * @param value
   * @return true if it was inserted, false otherwise
   */
  def insert(key:Array[Long],value:T):Boolean

  def content:QList[(Array[Long],T)]

  def size:Int

  def getDominatingOrEqual(key: Array[Long]): QList[(Array[Long], T)] = {

    def internalGetDominating(elements: QList[(Array[Long], T)]): QList[(Array[Long], T)] = {
      if (elements == null) {
        //when we get to here, we might dominate some keys, and we might be incomparable to some others we insert
        null
      } else {
        val h = elements.head
        val t = elements.tail
        compare(h._1, key) match {
          case Dominates | Equal =>
            QList(h, internalGetDominating(t))
          case Dominated | Incomparable =>
            internalGetDominating(t)
        }
      }
    }
    internalGetDominating(content)
  }

  def getDominatedOrEqual(key: Array[Long]): QList[(Array[Long], T)] = {

    def internalGetDominated(elements: QList[(Array[Long], T)]): QList[(Array[Long], T)] = {
      if (elements == null) {
        //when we get to here, we might dominate some keys, and we might be incomparable to some others we insert
        null
      } else {
        val h = elements.head
        val t = elements.tail
        compare(h._1, key) match {
          case Dominated | Equal =>
            QList(h, internalGetDominated(t))
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
}

class ListParetoMap[T](n:Int)
  extends ParetoMap[T](n) {

  var elements: QList[(Array[Long], T)] = _

  override def content: QList[(Array[Long], T)] = elements

  override def insert(key: Array[Long], value: T): Boolean = {
    def internalInsert(melements: QList[(Array[Long], T)]): (QList[(Array[Long], T)], Boolean) = {
      if (melements == null) {
        //when we get to here, we might dominate some keys, and we might be incomparable to some others we insert
        (QList((key, value)), true)
      } else {
        val h = melements.head
        val t = melements.tail
        compare(key, h._1) match {
          case Dominates =>
            //this new key dominates h; h to be removed and proceed
            internalInsert(t)
          case Dominated =>
            //this new key is dominated, discard it
            (melements, false)
          case Incomparable =>
            //proceed
            val (x, y) = internalInsert(t)
            (QList(h, x), y)
          case Equal =>
            //override h and stop
            (QList((key, value), t), true)
        }
      }
    }

    val (x, y) = internalInsert(elements)
    elements = x
    y
  }

  override def size: Int = QList.size(elements)
}

object TestParetoList extends App{

  val a:Array[Long] = Array(-1,0,0)
  val b:Array[Long] = Array(0,-1,0)
  val c:Array[Long] = Array(0,0,-1)

  val l = new ListParetoMap[Array[Long]](3)

  def mInsert(x:Array[Long]): Unit = {
    println()
    println("inserting " + x.mkString(","))
    println("inserted:" + l.insert(x,x))

    for(y <- l.content.toList){
      println(x.mkString(",") + " " + ParetoOrder.compare(x,y._1) + " " + y._1.mkString(","))
      println(y._1.mkString(",") + " " + ParetoOrder.compare(y._1,x) + " " + x.mkString(","))
    }
    println()
  }

  mInsert(a)
  mInsert(b)
  mInsert(c)

  for((x,_) <- l.content.toList){
    println(x.mkString(","))
  }


  val r = List(Dominates,Dominated,Incomparable,Equal)

  println("\t\t" + r.mkString("\t"))
  for(i <- r){
    print(i)
    for(j <- r){
      print("\t" + union(i,j))
    }
    println
  }
}
