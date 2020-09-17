/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by
  *         by Jean Noël Monette, Gustav Björdal
  ******************************************************************************/
package oscar.cbls.core.computation

import oscar.cbls._

import scala.collection.immutable.{NumericRange, SortedSet}
import scala.util.Random

//TODO: remplacer çà par Option(Long,Long)

object Domain{

  def empty:Domain = Domain(0 to 0)

  implicit def rangeToDomain(r:Range):Domain = {
    if (r.isEmpty) Domain(0, 0) //we put something or it crashes.
    else DomainRange(r.start,r.last)
  }

  implicit def coupleToDomain(i:(Long,Long)):Domain = {
    if(i._1 == i._2) SingleValueDomain(i._1)
    else DomainRange(i._1,i._2)
  }

  implicit def intToDomain(i:Long) = SingleValueDomain(i)

  implicit def minMaxCoupleLongLongToDomain(minMaxCouple:(Long,Long)):Domain = DomainRange(minMaxCouple._1,minMaxCouple._2)
  implicit def minMaxCoupleIntIntToDomain(minMaxCouple:(Int,Int)):Domain = DomainRange(minMaxCouple._1,minMaxCouple._2)
  implicit def minMaxCoupleIntLongToDomain(minMaxCouple:(Int,Long)):Domain = DomainRange(minMaxCouple._1,minMaxCouple._2)
  implicit def minMaxCoupleLongIntToDomain(minMaxCouple:(Long,Int)):Domain = DomainRange(minMaxCouple._1,minMaxCouple._2)

  def apply(s:SortedSet[Long]) = DomainRange(s.firstKey,s.lastKey)
  def apply(min:Long,max:Long) = DomainRange(min,max)
  def apply(min:Int,max:Int) = DomainRange(min,max)
  def apply(min:Int,max:Long) = DomainRange(min,max)
  def apply(min:Long,max:Int) = DomainRange(min,max)
  def apply(range:Range) = DomainRange(range.min,range.max)
  def apply(minMaxCouple:(Long,Long)) = DomainRange(minMaxCouple._1,minMaxCouple._2)
  def apply(v:Iterable[Long]) =  DomainRange(v.min,v.max)

}

sealed abstract class Domain{
  def min: Long
  def max: Long
  def size: Long
  def sizeInt: Int ={
    val sizeLong = size
    val sizeInt = sizeLong.toInt
    if (sizeInt != sizeLong) throw new ArithmeticException(s"integer overflow:$sizeLong")
    sizeInt
  }
  def contains(v:Long): Boolean
  //  def intersect(d:Domain):Domain

  def adjust(v:Long):Long = (v max this.min) min this.max

  def values:Iterable[Long]
  def randomValue():Long
  def restrict(d:Domain):Domain = intersect(d)
  def intersect(d:Domain):Domain

  def union(d:Domain):Domain

  def iterator: Iterator[Long] = values.iterator

  def isEmpty: Boolean = size == 0L
}

/**this is an inclusive domain*/
case class DomainRange(override val min: Long, override val max: Long) extends Domain {
  require(min <= max, s"domain should not be empty, got min:$min max:$max")

  def contains(v:Long): Boolean = min <= v && max >= v

  override def size: Long =
    if(min < 0 && min + Long.MaxValue <= max) Long.MaxValue
    else if(max==Long.MaxValue && min==Long.MinValue) Long.MaxValue
    else math.max(max-min+1L,0L)

  override def values: Iterable[Long] = min to max

  override def randomValue(): Long = {
    require(max - min < Int.MaxValue, "Range size must be < Int.MaxValue.")
    (min to max)(Random.nextInt((max - min + 1).toInt))
  }

  override def intersect(d: Domain): Domain = {
    val newDomain:Domain = d match{
      case r:DomainRange => (math.max(r.min,min), math.min(r.max,max))
      case FullRange => this
      case d:SingleValueDomain => d.intersect(this)
      case FullIntRange => this
    }
    if (newDomain.isEmpty) throw new EmptyDomainException
    newDomain
  }

  override def union(d: Domain): Domain = {
    val newDomain:Domain = d match{
      case r:DomainRange =>
        ( math.min(r.min,min) , math.max(r.max,max))
      case FullRange => FullRange
      case SingleValueDomain(v) =>
        if(v < min) (v , max)
        else if (max < v) (min , v)
        else this
      case FullIntRange => FullIntRange
    }
    if (newDomain.isEmpty)
      throw new EmptyDomainException
    newDomain
  }

  def toRange:NumericRange[Long] = min to max

  override def toString: String = s"DomainRange(min:$min, max:$max)"
}

case object FullRange extends Domain{
  override def min: Long = Long.MinValue
  override def max: Long = Long.MaxValue
  override def size: Long = Long.MaxValue
  override def randomValue(): Long = Random.nextLong()
  override def contains(v: Long): Boolean = true
  override def values: Iterable[Long] =  min to max
  override def intersect(d: Domain): Domain = d
  override def union(d: Domain): Domain = this
  override def toString: String = "FullRange"
}

case object FullIntRange extends Domain{
  override def min: Long = Int.MinValue
  override def max: Long = Int.MaxValue
  override def size: Long = Int.MaxValue
  override def randomValue(): Long = Random.nextInt()
  override def contains(v: Long): Boolean = if(v <= max && v >= min) true else false
  override def values: Iterable[Long] = min to max
  override def intersect(d: Domain): Domain = Domain(Math.max(min, d.min), Math.min(max, d.max))
  override def union(d: Domain): Domain = Domain(Math.min(min, d.min), Math.max(max, d.max))
  override def toString: String = "FullIntRange"
}

object PositiveOrNullRange extends DomainRange(0L, Long.MaxValue)

case class SingleValueDomain(value:Long) extends Domain{
  override def min: Long = value
  override def max: Long = value
  override def size: Long = 1
  override def contains(v: Long): Boolean = v == value

  override def randomValue(): Long = value

  override def intersect(d: Domain): Domain =
    if (d.contains(value)) this else throw new EmptyDomainException

  override def union(d: Domain): Domain = {
    d match {
      case SingleValueDomain(v) =>
        if(v == value) this
        else (math.min(v,value) , math.max(v,value))
      case _ => d.union(this)
    }
  }
  override def values: Iterable[Long] = List(value)

  override def toString: String = s"SingleValueDomain($value)"
}

class EmptyDomainException extends Exception("domain is empty")

/**
  * this object provides a few methods that perform safe operzations that do not overflow.
  * in case of overflow, the returned value is set to Min or MaxValue, depending on the considered operation.
  */
object DomainHelper{

  def safeAdd(a:Long, b:Long):Long = {
    val tmp = a.toLong + b.toLong

    if (a > 0 && b > 0 && tmp < 0) Long.MaxValue
    else if (a <0 && b <0 && tmp > 0) Long.MinValue
    else tmp
  }

  def safeAdd(a: Int, b: Int):Int = {
    val tmp = a + b

    if (a > 0 && b > 0 && tmp < 0) Int.MaxValue
    else if (a <0 && b <0 && tmp > 0) Int.MinValue
    else tmp
  }

  def safeMul(a:Long, b:Long):Long = {
    val tmp = a.toLong * b.toLong

    if (a > 0 && b > 0 && tmp < 0) Long.MaxValue
    else if (a < 0 && b < 0 && tmp > 0) Long.MaxValue
    else if (a < 0 && b > 0 && tmp > 0) Long.MinValue
    else if (a > 0 && b < 0 && tmp > 0) Long.MinValue
    else tmp
  }

  def safeMul(a:Int, b:Int):Int = {
    val tmp = a * b

    if (a > 0 && b > 0 && tmp < 0) Int.MaxValue
    else if (a < 0 && b < 0 && tmp > 0) Int.MaxValue
    else if (a < 0 && b > 0 && tmp > 0) Int.MinValue
    else if (a > 0 && b < 0 && tmp > 0) Int.MinValue
    else tmp
  }
}
