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
package oscar.invariants

import scala.collection.immutable._

trait Depending{
   def dispose(): Unit
  def until(d: Occuring[_]): Unit ={
    once (d){_ => 
      dispose() 
    }
  }
}


abstract class Reaction[A](val f: A => Boolean, occuring: Occuring[A]) extends Depending {
  def apply(msg: A) = {
    if (!f(msg)){
    	dispose()
    	false
    }else
      true
  }
 
}

trait Occuring[A]{
  
  def foreach( f: A => Boolean): Reaction[A]
  def apply( f: A => Unit ) = new ReactionDescription(this, {msg:A=>f(msg);true})
  def ~>( f: A => Unit ) = new ReactionDescription(this, {msg:A=>f(msg);true})
  def ===(a: A): Occuring[A] = filter(_ == a)
  def filter(f: A => Boolean): Occuring[A] = new ConditionalOccuring(this,f)
}


class ConditionalOccuring[A](d: Occuring[A], f:A=>Boolean) extends Occuring[A]{
  def foreach(f2: A => Boolean)={
    for ( msg <- d ){
      if ( f(msg) ){
        f2(msg)
      }else{
        true
      }
    }
  }
}

trait BaseEvent[A] extends Occuring[A]{
	class EventReaction(var event: BaseEvent[A], f: A => Boolean) extends Reaction[A](f, event) {
		val cf = dependants.add(this)
		def dispose(): Unit ={
			dependants.remove(cf)
		}
	}

	protected val dependants = new MyDLL[EventReaction]
	override def foreach(f: A => Boolean) ={
		new EventReaction(this,f)
	}
	def emit(msg: A): Unit
	def hanging = dependants.size
}


class OrEvent extends Reactive with NotifyAllEvent[Int]{
  var i = 0
  def |[A](eb: ReactionDescription[A]) ={
    val j = i
    dependsOn(eb.e){ msg: A =>
      eb.f(msg)
      this emit (j)
      dispose()
    }
    i += 1
    this
  }
}

class AndEvent extends Reactive with NotifyAllEvent[Int] {
  var n = 0
  var tot = 0
  def &[A](eb: ReactionDescription[A])= {
    val j = tot
    dependsOn(eb.occuring){ msg: A =>
      n += 1
      once (this) { _ => eb.f(msg)}
      if ( n == tot ) this emit (j)
      false
    }
    tot += 1
    this
  }
}

class ReactionDescription[A](val e: Occuring[A], val f: A => Boolean) {
  def occuring = e
  def | (eb: ReactionDescription[_]) = {
    val res = new OrEvent
    res | this | eb
    res
  }
  def & (eb: ReactionDescription[_]) = {
    val res = new AndEvent
    res & this & eb
    res
  }  
  
  def post() = occuring.foreach(f)
}

class Signal[A](private var value: A) extends Event[A]{
  
  override def emit(msg: A): Unit ={
    val old = value
    value = msg
    if ( msg != old ) super.emit(msg)
  }
  
  def apply() = value
  override def filter(f: A => Boolean) = new EventFromNow(this, f)
}

class Event[A] extends NotifyAllEvent[A]{}
class EventFromNow[A](sig: Signal[A], fil: A => Boolean) extends ConditionalOccuring[A](sig, fil){
  
  // could be improved in the case the call to f(sig()) is false;
  // then we could avoid to create the Reaction
  override def foreach(f: A => Boolean) = {
    val r = super.foreach(f)
    if ( fil(sig() )) r(sig())
    r
  }
}


trait NotifyAllEvent[A] extends BaseEvent[A]{  
  override def emit(msg: A): Unit = {
    for (d <- dependants) d(msg)
  }
}

trait NotifyOneEvent[A] extends Event[A]{
  override def emit(a: A): Unit ={
    if ( dependants.first != null ) dependants.first.apply(a)
  }
}

class EventOne[A] extends NotifyOneEvent[A] {}

class SignalOne[A](value: A) extends Signal[A](value) with NotifyOneEvent[A]{
  override def emit(msg:A): Unit ={
    super[Signal].emit(msg)
    super[NotifyOneEvent].emit(msg)
  }
}

class SignalAll[A](value: A) extends Signal[A](value) with NotifyAllEvent[A]{
  override def emit(msg:A): Unit ={
    super[Signal].emit(msg)
    super[NotifyAllEvent].emit(msg)
  }
}
