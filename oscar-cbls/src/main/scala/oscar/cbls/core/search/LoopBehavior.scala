/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.cbls.core.search

import scala.language.reflectiveCalls
import scala.util.Random

object LoopBehavior{
  def first(maxNeighbors:() => Long = () => Long.MaxValue) = First(maxNeighbors)
  def best(maxNeighbors:() => Long = () => Long.MaxValue) = Best(maxNeighbors)
  def timeout(enclosedBehavior:LoopBehavior,timeOutMilliSeconds:Long) = Timeout(enclosedBehavior,timeOutMilliSeconds)
}

sealed abstract class LoopBehavior(){
  def toIterable[T](baseIterable:Iterable[T]):(BoundedIterable[T],()=>Unit)
  def toIterator[T](baseIterable:Iterable[T]):(BoundedIterator[T],()=>Unit) = {
    val (iterable,stop) = toIterable(baseIterable)
    (iterable.iterator,stop)
  }
}

trait BoundedIterator[T] extends Iterator[T]{
  def hasUnboundedNext():Boolean
  def unboundedNext():T
}

trait BoundedIterable[T] extends Iterable[T]{
  override def iterator : BoundedIterator[T]
}

case class Timeout(enclosedBehavior:LoopBehavior,timeOutMilliSeconds:Long) extends LoopBehavior(){
  override def toIterable[T](baseIterable: Iterable[T]): (BoundedIterable[T], () => Unit) = {
    val (enclosedBoundedIterable,enclosedNotifyFound) = enclosedBehavior.toIterable(baseIterable)

    val timeOutedIterable = new BoundedIterable[T]{
      override def iterator : BoundedIterator[T] = new BoundedIterator[T]{
        val startTimeMS = System.nanoTime()/(1000*1000)
        val baseIterator = enclosedBoundedIterable.iterator

        override def hasUnboundedNext(): Boolean = baseIterator.hasUnboundedNext()

        override def unboundedNext(): T = baseIterator.unboundedNext()

        override def hasNext: Boolean = {
          val currentTimeMs = System.nanoTime()/(1000*1000)
          if(startTimeMS + timeOutMilliSeconds < currentTimeMs){
            //println("timeout")
            false
          }
          else {
            baseIterator.hasNext
          }
        }

        override def next(): T = baseIterator.next() //we do not check timing here because it might have changed since next was tested
      }
    }
    (timeOutedIterable,enclosedNotifyFound)
  }
}
//TODO: randomized
//TODO: best, cap after xxx sucessive not better neighbors

case class First(maxNeighbors:() => Long = () => Long.MaxValue, randomized:Boolean = false) extends LoopBehavior(){
  override def toIterable[T](baseIterable : Iterable[T]) : (BoundedIterable[T],()=>Unit) = {
    val iterable = new BoundedIterable[T]{
      var foundMove:Boolean = false
      var remainingNeighbors = maxNeighbors()
      override def iterator : BoundedIterator[T] = new BoundedIterator[T]{
        val baseIterator = if(randomized){
          Random.shuffle(baseIterable.toList).iterator
        } else baseIterable.iterator
        override def hasNext : Boolean = baseIterator.hasNext && !foundMove && remainingNeighbors>0L
        override def next() : T = {remainingNeighbors -= 1L; baseIterator.next}
        override def hasUnboundedNext() : Boolean = baseIterator.hasNext
        override def unboundedNext() : T = baseIterator.next
      }
    }

    def notifyFound(){
      iterable.foundMove = true
    }

    (iterable,notifyFound _)
  }
}

//TODO: this is not maxAcceptedNeighbors!!
case class Best(maxNeighbors:() => Long = () => Long.MaxValue) extends LoopBehavior(){
  override def toIterable[T](baseIterable : Iterable[T]) : (BoundedIterable[T],()=>Unit) = {
    val iterable = new BoundedIterable[T]{
      var remainingNeighbors = maxNeighbors()
      override def iterator : BoundedIterator[T] = new BoundedIterator[T]{
        val baseIterator = baseIterable.iterator
        override def hasNext : Boolean = baseIterator.hasNext && remainingNeighbors>0L
        override def next() : T = {remainingNeighbors -= 1L; baseIterator.next}
        override def hasUnboundedNext() : Boolean = baseIterator.hasNext
        override def unboundedNext() : T = baseIterator.next
      }
    }

    def notifyFound(){}
    (iterable,notifyFound _)
  }
}