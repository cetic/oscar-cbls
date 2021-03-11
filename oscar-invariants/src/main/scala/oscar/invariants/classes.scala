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
import oscar.invariants._


abstract class Dependency[A](val reactive: Reactive, var reaction: Reaction[A]) extends Depending{ 
  def apply(msg: A) = {
    if ( !reaction.apply(msg) )
      dispose()
  }
  def nowReactsOn(d: Occuring[A]): Unit ={
    //dispose()
    reaction.dispose()
    reaction = d.foreach(reaction.f)
  }
}


class Reactive {
  val dependingOn = new MyDLL[RDependency[_]]
  
  class RDependency[A](reaction: Reaction[A]) extends Dependency[A](this, reaction){
    val elem = dependingOn.add(this)
    def dispose(): Unit ={
      reaction.dispose()
      dependingOn.remove(elem)
    }
  }
  def dependsOn[C](d: Occuring[C])(f: C => Boolean): Dependency[C] = {
    new RDependency[C]( for (msg <- d) f(msg))
  }
  def dispose() = {
    for ( d <- dependingOn) d.dispose()
    true
  }
}



class Var[A](_value: A) extends Signal[A](_value) {	
  
  def :=(v: A) = emit (v)
  
  def <= (f: this.type => Any): this.type = {
    f(this)
    this
  }
}

object Var {
  def apply[A](v: A) = {
    v match {
      case Int => new VarInt(v.asInstanceOf[Int])
    }
  }
}

class VarInt(v: Int) extends Var[Int](v) {
  val incChanges = new Event[(Int, Int)]
  override final def :=(v: Int): Unit = {
    val old = this()
    super.:=(v)
    incChanges emit (old, v)
  }
  def :+=(v: Int): Unit = { this := this() + v}
  def :-=(v: Int): Unit = { this := this() - v}
}

class VarList[A]() extends Var[Seq[A]](Nil){
  val isIncreased = new Event[A]
  val isDecreased = new Event[A]
  def add(elem: A): Unit ={
    this := this() :+ elem
    isIncreased emit(elem)
  }
  def remove(elem: A): Unit ={
    this := this().drop(this().indexOf(elem))
    isDecreased emit(elem)    
  }
}

abstract class StaticInvariant[R] extends Reactive {


}

class ElementArray[A](arr: scala.collection.immutable.IndexedSeq[Var[A]]) {
  @inline def at(y: Var[Int]) = {v: Var[A] =>
    new Element1(arr, y, v)
  }
}
//class ElementArray2[A](arr: IndexedSeq[IndexedSeq[Var[A]]]) {
//  def at(y: Var[Int], z: Var[Int]) = {
//    new Element2(arr, y, z)
//  }
//}

class Element1[A](x: IndexedSeq[Var[A]], y: Var[Int], v: Var[A]) extends StaticInvariant[A] {
  v := x(y)
  def scope() = y +: ( for (v <- x) yield v)
  var dep = dependsOn(x(y)) { (w: A) =>
    v := w
    true
  }
  val a = dependsOn(y) { (w: Int) =>
    v := x(w)
    dep nowReactsOn(x(w))
    true
  }
}


class SumInvariant(result: VarInt, list: List[VarInt]) extends StaticInvariant[Int] {
  def scope = (for ( v <- list ) yield v.incChanges).toIndexedSeq
  var a = 0
  for ( v <- list.iterator ){
    a += v
    dependsOn (v incChanges){ case (o,n) =>
      result :+= n-o     
      true
    }
  }
  result := a
}

class SumInvariantOnList(result: VarInt, list: VarList[Int]) extends StaticInvariant[Int] {
  var a = 0
  for ( v <- list().iterator ){
    a += v
  }
  result := a
  dependsOn (list.isIncreased ){ w=>
    result :+= w
    true
  }
}


class SumInvariantOnListOfVars(result: VarInt, list: VarList[VarInt]) extends StaticInvariant[Int] {
  var a = 0
  val mmap = new scala.collection.mutable.HashMap[VarInt,Dependency[(Int,Int)]]
  for ( v <- list().iterator ){
    mmap.put(v, dependsOn(v.incChanges){ case(o,n) =>
    	result :+= n-o
    	true
    })
    a += v
  }
  result := a
  dependsOn (list.isIncreased ){ v=>
    result :+= v
    mmap.put(v, dependsOn(v.incChanges){ case(o,n) =>
    	result :+= n-o
    	true
    })
    true
  }
  dependsOn (list.isDecreased ){ v=>
    result :-= v
    mmap.get(v).get.dispose()
    true
  }
}

