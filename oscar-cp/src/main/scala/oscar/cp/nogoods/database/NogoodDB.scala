package oscar.cp.nogoods.database

import oscar.cp.nogoods.core.Nogood

/** @author Renaud Hartert ren.hartert@gmail.com */
abstract class NogoodDB {
  
  /** Remove entailed nogoods from the data base. */
  def reduceEntailed(): Unit

  /** Returns an array with all nogoods **/
  def allNogoods(): Array[Nogood]

  /** Remove all nogoods from the DB **/
  def clear(): Unit

  /** Add a new nogood in the data base. */
  def add(nogood: Nogood): Unit 
  
  /** Add several new nogoods to the data base. */
  def add(nogoods: Array[Nogood]): Unit 
  
  /** Returns a random nogood from the data base. */
  def randomNogood: Nogood
  
  /** Returns the largest nogood from the data base. */
  def largestNogood: Nogood
  
  def foreach[U](f: Nogood => U): Unit
  
  def size: Int
  
  def addEmpty(): Unit
  
  def remove(nogood: Nogood): Unit
}

object NogoodDB {
  def apply(): NogoodDB = new NogoodDBImpl
}

class NogoodDBImpl extends NogoodDB {
  
  private[this] var nogoods: Array[Nogood] = new Array(100)
  private[this] var nNogoods: Int = 0
  
  /** Remove entailed nogoods from the data base. */
  def reduceEntailed(): Unit = ()

  def clear() = nNogoods = 0



  /** Add a new nogood in the data base. */
  def add(nogood: Nogood): Unit = {
    if (nNogoods == nogoods.length) growDB()
    nogoods(nNogoods) = nogood
    nNogoods += 1
  }
  
  def addEmpty(): Unit = {
    if (nNogoods == nogoods.length) growDB()
    nogoods(nNogoods) = new Nogood(Array.empty)
    nNogoods += 1
  }
  
  /** Add several new nogoods to the data base. */
  def add(nogoods: Array[Nogood]): Unit = ???
  
  /** Returns a random nogood from the data base. */
  def randomNogood: Nogood = ???
  
  def remove(nogood: Nogood): Unit = {
    var i = 0
    var j = 0
    while (i < nNogoods) {
      val n = nogoods(i)
      if (n != nogood) {
        nogoods(j) = n
        j += 1
      }
      i += 1
    }
    nNogoods = j
  }
  
  /** Returns the largest nogood from the data base. */
  def largestNogood: Nogood = {
    var nogood: Nogood = null
    var max = Int.MinValue
    var i = nNogoods
    while (i > 0) {
      i -= 1
      val n = nogoods(i)
      val size = n.decisions.length
      if (size > max) {
        max = size 
        nogood = n
      }
    }
    nogood
  }
  
  def size: Int = nNogoods
  
  def foreach[U](f: Nogood => U): Unit = {
    var i = nNogoods
    while (i > 0) {
      i -= 1
      f(nogoods(i))
    }
  }

  def allNogoods(): Array[Nogood] = nogoods.take(nNogoods)


  @inline private def growDB(): Unit = {
    val newStack = new Array[Nogood](nogoods.length * 2)
    System.arraycopy(nogoods, 0, newStack, 0, nogoods.length)
    nogoods = newStack
  }
  
  override def toString: String = {
    var min = Int.MaxValue
    var m: Nogood = null
    var max = Int.MinValue
    var sum = 0
    var i = nNogoods
    while (i > 0) {
      i -= 1
      val nogood = nogoods(i)
      val size = nogood.size
      if (size < min) {
        min = size
        m = nogood
      }
      if (size > max) max = size
      sum += size
    }
    val average = sum.toDouble / nNogoods
    s"size: $nNogoods, minLiterals: $min, maxLiterals: $max, averageLiterals: $average"
  }
}