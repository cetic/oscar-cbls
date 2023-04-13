package oscar.cbls.algo.pareto

import oscar.cbls.algo.pareto.ParetoOrder._

class ListParetoMap[T](nbDimensions:Int)
  extends ParetoMap[T](nbDimensions) {

  private var elements: List[(Array[Long], T)] = Nil

  override def content: List[(Array[Long], T)] = elements

  override def insert(key: Array[Long], value: T): Boolean = {
    def internalInsert(melements: List[(Array[Long], T)]): (List[(Array[Long], T)], Boolean) = {
      melements match {
        case Nil =>
          (List((key, value)), true)
        case h::t =>
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
              (h :: x, y)
            case Equal =>
              //override h and stop
              ((key, value) :: t, true)
          }
      }
    }

    val (x, y) = internalInsert(elements)
    elements = x
    y
  }

  override def size: Int = elements.size
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

    for(y <- l.content){
      println(x.mkString(",") + " " + ParetoOrder.compare(x,y._1) + " " + y._1.mkString(","))
      println(y._1.mkString(",") + " " + ParetoOrder.compare(y._1,x) + " " + x.mkString(","))
    }
    println()
  }

  mInsert(a)
  mInsert(b)
  mInsert(c)

  for((x,_) <- l.content){
    println(x.mkString(","))
  }


  val r = List(Dominates,Dominated,Incomparable,Equal)

  println("\t\t" + r.mkString("\t"))
  for(i <- r){
    print(i)
    for(j <- r){
      print("\t" + union(i,j))
    }
    println()
  }
}

