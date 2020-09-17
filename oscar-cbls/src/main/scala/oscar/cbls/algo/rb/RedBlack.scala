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
package oscar.cbls.algo.rb

import oscar.cbls.algo.quick.QList

import scala.annotation.tailrec

/*** Okasaki-style red-black tree maps. ***/

private object RedBlackTreeMapLib{
  val R = true
  val B = false

  // blacken: Turn a node black.
  def blacken[V] (n : RedBlackTreeMap[V])  : RedBlackTreeMap[V] = {
    n match {
      case L() => n
      case T(_,l,k,v,r) => T(B,l,k,v,r)
    }
  }

  // balance: Balance a tree with balanced subtrees.
  def balance[V] (c : Boolean,
                  l : RedBlackTreeMap[V],
                  k : Int,
                  v : Option[V],
                  r : RedBlackTreeMap[V]) : RedBlackTreeMap[V] = {
    (c,l,k,v,r) match {
      case (B,T(R,T(R,a,xK,xV,b),yK,yV,c),zK,zV,d) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,T(R,a,xK,xV,T(R,b,yK,yV,c)),zK,zV,d) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,a,xK,xV,T(R,T(R,b,yK,yV,c),zK,zV,d)) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (B,a,xK,xV,T(R,b,yK,yV,T(R,c,zK,zV,d))) => T(R,T(B,a,xK,xV,b),yK,yV,T(B,c,zK,zV,d))
      case (c,a,xK,xV,b) => T(c,a,xK,xV,b)
    }
  }
}

class IntVCouple[@specialized V](val k:Int,val value:V)

import oscar.cbls.algo.rb.RedBlackTreeMapLib._

//must use trait here because of specialization, a trait is needed here.
// we ensure that this trait is compiled into a java interface by avoiding method code in the trait.
//as a consequence, there are duplicates in the classes implementing this trait.
trait RedBlackTreeMap[@specialized(Int) V]{

  /* We could have required that K be <: Ordered[K], but this is
  actually less general than requiring an implicit parameter that can
  convert K into an Ordered[K].

  For example, Int is not compatible with Ordered[Int].  This would
  make it unusable with this map; however, it's simple to define an
  injector from Int into Ordered[Int].

  In fact, the standard prelude already defines just such an implicit:
  intWrapper. */

  // modWith: Helper method; top node could be red.
  protected[rb] def modWith (k : Int, f : (Int, Option[V]) => Option[V]) : RedBlackTreeMap[V]

  // get: Retrieve a value for a key.
  def get(k : Int) : Option[V]

  def getOrElse(k:Int,default: =>V):V

  def contains(k:Int):Boolean

  def biggestLowerOrEqual(k:Int):Option[(Int,V)]

  protected[rb] def getBiggestLowerAcc(k:Int, bestKSoFar:Int,bestVSoFar:V):IntVCouple[V]

  def smallestBiggerOrEqual(k:Int):Option[(Int,V)]

  def smallest:Option[(Int,V)]

  def biggest:Option[(Int,V)]

  def biggestPosition:Option[RedBlackTreeMapExplorer[V]]

  def smallestPosition:Option[RedBlackTreeMapExplorer[V]]

  protected[rb] def getSmallestBiggerAcc(k:Int, bestKSoFar:Int, bestVSoFar:V):IntVCouple[V]

  // insert: Insert a value at a key.
  def insert (k : Int, v : V) : RedBlackTreeMap[V]

  // remove: Delete a key.
  def remove (k : Int) : RedBlackTreeMap[V]

  def size:Int
  def isEmpty:Boolean

  def values:List[V]
  protected [rb] def valuesAcc(valuesAfter:List[V]):List[V]
  def content:List[(Int,V)]
  protected [rb] def contentAcc(valuesAfter:List[(Int,V)]):List[(Int,V)]

  def keys:List[Int]
  protected [rb] def keysAcc(keysAfter:List[Int]):List[Int]

  def qKeys:QList[Int]
  protected [rb] def qKeysAcc(keysAfter:QList[Int]):QList[Int]

  def positionOf(k: Int):Option[RedBlackTreeMapExplorer[V]]
  protected[rb] def positionOfAcc(k:Int,positionAcc:QList[(T[V],Boolean)]):Option[RedBlackTreeMapExplorer[V]]

  def anyValue:Option[V]

  /**
   * updates a set of values in the tree, designated by an interval on the keys
   * there is a requirement that the deltaKey must not transform a key in the interval in such a way
   * that it becomes bigger or smaller than a key out of te interval if it was not the case before the transformation
   * this is to keep an identical structure of the tree, and maintain the same r/b colouring and balancing of the tree.
   * this ensures that this method is somehow fast.
   *
   * @param fromKeyIncluded the start of the interval defining the set of keys to update
   * @param toKeyIncluded the end of the interval defining the set of keys to update
   * @param deltaKey the delata to apply to the keys in the interval
   * @param transform the transform to apply on the values stored in the transformed interval
   * @return a new updated balanced rb tree
   */
  def updateDelta(fromKeyIncluded:Int,toKeyIncluded:Int,deltaKey:Int,transform:(V=>V)):RedBlackTreeMap[V]

  /**
   * this method ensures that the key are traversed in ascending order.
   * @param fromKeyIncluded
   * @param toKeyIncluded
   * @param transform
   * @return
   */
  def update(fromKeyIncluded:Int,toKeyIncluded:Int,transform:((Int,V) => (Int,V))):RedBlackTreeMap[V]

  def updateAll(deltaKey:Int,transform:(V=>V)):RedBlackTreeMap[V] = {
    (this.smallest,this.biggest) match{
      case (None,None) => this
      case (Some((smallestKey,_)),Some((biggestKey,_))) => updateDelta(smallestKey,biggestKey,deltaKey,transform)
      case _ => throw new Error("unexpected error")
    }
  }
}

// A leaf node.
case class L[@specialized(Int) V]() extends RedBlackTreeMap[V]  {

  def anyValue:Option[V] = None

  def get(k : Int) : Option[V] = None

  def getOrElse(k:Int,default: =>V):V = get(k) match{
    case None => default
    case Some(x) => x
  }

  override def contains(k : Int) : Boolean = false

  override protected[rb] def modWith (k : Int, f : (Int, Option[V]) => Option[V]) : RedBlackTreeMap[V] = {
    f(k,None) match{
      case None => this
      case something => T(R, this, k, something, this)
    }
  }

  def biggestLowerOrEqual(k:Int):Option[(Int,V)] = None

  override protected[rb] def getBiggestLowerAcc(k:Int, bestKSoFar:Int,bestVSoFar:V):IntVCouple[V] = new IntVCouple[V](bestKSoFar,bestVSoFar)

  override def smallestBiggerOrEqual(k: Int):Option[(Int,V)] = None

  override protected[rb] def getSmallestBiggerAcc(k:Int, bestKSoFar:Int, bestVSoFar:V) = new IntVCouple(bestKSoFar,bestVSoFar)

  override def size: Int = 0
  override def isEmpty = true

  protected [rb] def valuesAcc(valuesAfter:List[V]):List[V] = valuesAfter
  protected [rb] def contentAcc(valuesAfter:List[(Int,V)]):List[(Int,V)] = valuesAfter
  protected [rb] def keysAcc(keysAfter:List[Int]):List[Int] = keysAfter
  override protected[rb] def qKeysAcc(keysAfter: QList[Int]): QList[Int] = keysAfter

  protected[rb] override def positionOfAcc(k : Int, positionAcc : QList[(T[V],Boolean)]) : Option[RedBlackTreeMapExplorer[V]] = None


  //duplicates
  def values:List[V] = List.empty

  def content:List[(Int,V)] = List.empty

  override def keys : List[Int] = List.empty

  override def qKeys: QList[Int] = null

  override def positionOf(k: Int):Option[RedBlackTreeMapExplorer[V]] = None

  // insert: Insert a value at a key.
  override def insert (k : Int, v : V) = T(B, L(), k , Some(v), L())

  // remove: Delete a key.
  override def remove (k : Int) = this

  override def smallest:Option[(Int,V)] = None

  override def biggest:Option[(Int,V)] = None

  override def biggestPosition:Option[RedBlackTreeMapExplorer[V]] = None

  override def smallestPosition:Option[RedBlackTreeMapExplorer[V]] = None

  override def updateDelta(fromKeyIncluded : Int, toKeyIncluded : Int, deltaKey : Int, transform : (V) => V) : RedBlackTreeMap[V] = this

  override def update(fromKeyIncluded:Int,toKeyIncluded:Int,transform:((Int,V) => (Int,V))):RedBlackTreeMap[V] = this
}

object T{
  def unapply[V](t:T[V]):Option[(Boolean, RedBlackTreeMap[V], Int, Option[V], RedBlackTreeMap[V])] = {
    t.unapply
  }

  def apply[V](c : Boolean, l : RedBlackTreeMap[V], k : Int, v : Option[V], r : RedBlackTreeMap[V]) =
    new T(c,l,k,v,r)
}

// A tree node.
class T[@specialized(Int) V](private[this]val c : Boolean,
                             private[this] val l : RedBlackTreeMap[V],
                             private[this] val k : Int,
                             private[this] val v : Option[V],
                             private[this] val r : RedBlackTreeMap[V])
  extends RedBlackTreeMap[V] {

  def anyValue:Option[V] = v

  def unapply = Some(c,l,k,v,r)

  def pk = k
  def pl = l
  def pr = r
  def pv = v

  assert(v.nonEmpty)

  override val size:Int = l.size + r.size + 1
  override def isEmpty = false

  def get(k : Int) : Option[V] = {
    if (k < this.k) l.get(k)
    else if (k > this.k) r.get(k)
    else v
  }

  def getOrElse(k:Int,default: =>V):V = get(k) match{
    case None => default
    case Some(x) => x
  }

  override def contains(k : Int) : Boolean = {
    if (k < this.k) l.contains(k)
    else if (k > this.k) r.contains(k)
    else true
  }

  def biggestLowerOrEqual(k:Int):Option[(Int,V)] = {
    if (k < this.k) l.biggestLowerOrEqual(k)
    else if (this.k < k){
      val result = r.getBiggestLowerAcc(k,this.k,v.get)
      Some((result.k,result.value))
    }else Some(k,v.get)
  }

  override protected[rb] def getBiggestLowerAcc(k:Int, bestKSoFar:Int, bestVSoFar:V):IntVCouple[V] = {
    if (k < this.k) l.getBiggestLowerAcc(k, bestKSoFar, bestVSoFar)
    else if (this.k < k) r.getBiggestLowerAcc(k, this.k, v.get)
    else new IntVCouple(k,v.get)
  }

  override def smallestBiggerOrEqual(k: Int):Option[(Int,V)] = {
    if (k < this.k) {
      val result = l.getSmallestBiggerAcc(k, this.k, v.get)
      Some((result.k,result.value))
    } else if (this.k < k) r.smallestBiggerOrEqual(k)
    else Some(k,v.get)
  }

  override protected[rb] def getSmallestBiggerAcc(k:Int, bestKSoFar:Int, bestVSoFar:V):IntVCouple[V] = {
    if (k < this.k) l.getSmallestBiggerAcc(k, this.k, v.get)
    else if (this.k < k) r.getSmallestBiggerAcc(k, bestKSoFar:Int, bestVSoFar:V)
    else new IntVCouple(k,v.get)
  }

  override protected[rb] def modWith (k : Int, f : (Int, Option[V]) => Option[V]) : RedBlackTreeMap[V] = {
    if (k <  this.k) balance(c,l.modWith(k,f),this.k,this.v,r)
    else if (k == this.k) {
      f(this.k, this.v) match{
        case None =>
          if(l.isEmpty) r
          else if (r.isEmpty) l
          else{
            r.smallest match{
              case Some((rk,rv)) => T(c, l, rk, Some(rv), r.remove(rk))
              case None => throw new Error("non smallest on non-empty RB?")
            }
          }
        case x => T(c, l, k, x, r)
      }
    }else {
      balance(c,l,this.k,this.v,r.modWith(k, f))
    }
  }

  override protected[rb] def valuesAcc(valuesAfter : List[V]) : List[V] = l.valuesAcc(v.get :: r.valuesAcc(valuesAfter))

  override protected [rb] def contentAcc(valuesAfter:List[(Int,V)]):List[(Int,V)] = l.contentAcc((k,v.get) :: r.contentAcc(valuesAfter))

  override protected [rb] def keysAcc(keysAfter:List[Int]):List[Int] = l.keysAcc(k :: r.keysAcc(keysAfter))

  override protected[rb] def qKeysAcc(keysAfter: QList[Int]): QList[Int] = l.qKeysAcc(QList(k,r.qKeysAcc(keysAfter)))

  protected[rb] override def positionOfAcc(k : Int, positionAcc : QList[(T[V],Boolean)]) : Option[RedBlackTreeMapExplorer[V]] = {
    if (k < this.k) l.positionOfAcc(k, QList((this,false),positionAcc))
    else if (k > this.k) r.positionOfAcc(k, QList((this,true),positionAcc))
    else Some(new RedBlackTreeMapExplorer[V](QList((this,true),positionAcc)))
  }

  def hasLeft:Boolean = l.isInstanceOf[T[V]]
  def hasRight:Boolean = r.isInstanceOf[T[V]]

  //duplicates
  override def values:List[V] = valuesAcc(List.empty)

  override def content : List[(Int, V)] = contentAcc(List.empty)

  override def keys : List[Int] = keysAcc(List.empty)

  override def qKeys:QList[Int] = qKeysAcc(null)

  override def positionOf(k: Int):Option[RedBlackTreeMapExplorer[V]] = positionOfAcc(k:Int,null)

  // insert: Insert a value at a key.
  override def insert (k : Int, v : V) = blacken(modWith(k, (_,_) => Some(v)))

  // remove: Delete a key.
  override def remove (k : Int) = blacken(modWith(k, (_,_) => None))

  override def smallest:Option[(Int,V)] = smallestBiggerOrEqual(Int.MinValue)

  override def biggest:Option[(Int,V)] = biggestLowerOrEqual(Int.MaxValue)

  override def biggestPosition:Option[RedBlackTreeMapExplorer[V]] = {
    biggestLowerOrEqual(Int.MaxValue) match{
      case Some((rk,_)) => positionOf(rk)
      case None => None
    }
  }

  override def smallestPosition:Option[RedBlackTreeMapExplorer[V]] = {
    smallestBiggerOrEqual(Int.MinValue) match{
      case Some((rk,_)) => positionOf(rk)
      case None => None
    }
  }

  override def update(fromKeyIncluded : Int, toKeyIncluded : Int, transform : (Int, V) => (Int, V)) : RedBlackTreeMap[V] = {
    val newLeft = if(fromKeyIncluded < k) {
      l.update(fromKeyIncluded, toKeyIncluded, transform)
    }else{
      l
    }
    //this method ensures that the keys are traversed in ascending order,
    //so the code is structures in this  way with identical fragments of code that must not not be factorized
    if(fromKeyIncluded <= k && k <= toKeyIncluded){
      //this one must be transformed as well
      val (newK,newV) = transform(k,v.get)
      val newRight = if(k < toKeyIncluded){
        r.update(fromKeyIncluded,toKeyIncluded,transform)
      }else{
        r
      }
      new T(c,
        newLeft,
        newK,
        Some(newV),
        newRight)
    }else{
      //this one does not need transform
      val newRight = if(k < toKeyIncluded){
        r.update(fromKeyIncluded,toKeyIncluded,transform)
      }else{
        r
      }
      if(newLeft == l && newRight == r){
        this
      }else{
        new T(c,
          newLeft,
          k ,
          v,
          newRight)
      }
    }
  }

  override def updateDelta(fromKeyIncluded : Int, toKeyIncluded : Int, deltaKey : Int, transform : V => V) : RedBlackTreeMap[V] = {
    val newLeft = if(fromKeyIncluded < k) {
      l.updateDelta(fromKeyIncluded, toKeyIncluded, deltaKey, transform)
    }else{
      l
    }
    val newRight = if(k < toKeyIncluded){
      r.updateDelta(fromKeyIncluded,toKeyIncluded,deltaKey,transform)
    }else{
      r
    }
    if(fromKeyIncluded <= k && k <= toKeyIncluded){
      //this one must be transformed as well
      new T(c,
        newLeft,
        k + deltaKey,
        Some(transform(v.get)),
        newRight)
    }else{
      //this one does not need transform
      if(newLeft == l && newRight == r){
        this
      }else{
        new T(c,
          newLeft,
          k ,
          v,
          newRight)
      }
    }
  }
}

// A helper object.
object RedBlackTreeMap {

  // empty: Converts an order-able type into an empty RBMap.
  def empty[@specialized(Int) V] : RedBlackTreeMap[V] = L[V]()

  // apply: Assumes an implicit conversion.
  def apply[@specialized(Int) V](args : Iterable[(Int,V)]) : RedBlackTreeMap[V] = {
    var currentMap : RedBlackTreeMap[V] = L()
    for ((k,v) <- args) {
      currentMap = currentMap.insert(k,v)
    }
    currentMap
  }

  /**
   * make the red black tree out of already sorted couples (key,value)
   * they must be sorted by increasing order of key, and a key can only be present once.
   * There is no check of these properties
   * This is O(n); thus faster than a n*log(n) if you were building it from unsorted pairs
   * @param args
   * @tparam V
   * @return
   */
  def makeFromSorted[@specialized(Int) V](args:Iterable [(Int,V)]): RedBlackTreeMap[V] = {
    //root is to be black, beside alternate red and black
    val a = args.toArray
    if(args.size <=3) this.apply(args)
    else myMakeFromSorted(a,0,a.length-1,targetIsRed = false)
  }

  def makeFromSortedContinuousArray[@specialized V](args:Array[V]):RedBlackTreeMap[V] = {
    if(args.length == 0) RedBlackTreeMap.empty [V]
    else myMakeFromContinuousSorted(args, 0, args.length - 1, targetIsRed = false)
  }

  private def myMakeFromContinuousSorted[@specialized(Int) V](args:Array[V],fromIncluded:Int,toIncluded:Int,targetIsRed:Boolean): RedBlackTreeMap[V] = {
    //root is to be black, beside alternate red and black
    if(fromIncluded == toIncluded){
      val value = args(fromIncluded)
      T(targetIsRed, L(),  fromIncluded, Some(value), L())
    }else if (fromIncluded + 1 == toIncluded) {
      val valueL = args(fromIncluded)
      val valueH = args(toIncluded)
      T(targetIsRed, T(!targetIsRed, L(),  fromIncluded, Some(valueL), L()),  toIncluded, Some(valueH), L())
    }else{
      //there is a middle point
      val middlePoint = (fromIncluded + toIncluded)/2
      val left = myMakeFromContinuousSorted(args,fromIncluded,middlePoint-1,!targetIsRed)
      val right = myMakeFromContinuousSorted(args,middlePoint+1,toIncluded,!targetIsRed)
      val value = args(middlePoint)
      T(targetIsRed, left,  middlePoint, Some(value), right)
    }
  }

  /**
   * make the red black tree out of already sorted couples (key,value)
   * they must be sorted by increasing order of key, and a key can only be present once.
   * There is no check of these properties
   * This is O(n); thus faster than a n*log(n) if you were building it from unsorted pairs
   * @param args
   * @tparam V
   * @return
   */
  def makeFromSortedArray[@specialized(Int) V](args:Array[(Int,V)]): RedBlackTreeMap[V] = {
    //root is to be black, beside alternate red and black
    if(args.length <=1) this.apply(args)
    else myMakeFromSorted(args,0,args.length-1,targetIsRed = false)
  }


  private def myMakeFromSorted[@specialized(Int) V](args:Array[(Int,V)],fromIncluded:Int,toIncluded:Int,targetIsRed:Boolean): RedBlackTreeMap[V] = {
    //root is to be black, beside alternate red and black
    if(fromIncluded == toIncluded){
      val(key,value) = args(fromIncluded)
      T(targetIsRed, L(),  key, Some(value), L())
    }else if (fromIncluded + 1 == toIncluded) {
      val(keyL,valueL) = args(fromIncluded)
      val(keyH,valueH) = args(toIncluded)

      assert(keyH > keyL, "Unsorted array")

      T(targetIsRed,
        T(!targetIsRed, L(),  keyL, Some(valueL), L()),
        keyH,
        Some(valueH),
        L())
    }else{
      //there is a middle point
      val middlePoint = (fromIncluded + toIncluded)/2
      val left = myMakeFromSorted(args,fromIncluded,middlePoint-1,!targetIsRed)
      val right = myMakeFromSorted(args,middlePoint+1,toIncluded,!targetIsRed)
      val(key,value) = args(middlePoint)

      assert(left.asInstanceOf[T[V]].pk < key, "Unsorted array")
      assert(right.asInstanceOf[T[V]].pk > key, "Unsorted array")

      T(targetIsRed, left,  key, Some(value), right)
    }
  }
}

//le booléen: true le noeud a déjà été montré (dans un parcour gauche à droite)
class RedBlackTreeMapExplorer[@specialized(Int) V](position:QList[(T[V],Boolean)]){
  def key:Int = position.head._1.pk
  def value:V = position.head._1.pv.get

  override def toString : String = "RBPosition(key:" + key + " value:" + value + " stack:" + position.toList + ")"

  def next:Option[RedBlackTreeMapExplorer[V]] = {

    @tailrec
    def unstack1(position:QList[(T[V],Boolean)]):QList[(T[V],Boolean)] = {
      if (position == null) return null
      val head = position.head
      if (!head._2){
        //not presented yet, so we present this one
        QList((head._1,true),position.tail)
      }else{
        //already presented, so unstack
        unstack1(position.tail)
      }
    }

    @tailrec
    def descendToLeftMost(position:QList[(T[V],Boolean)]):QList[(T[V],Boolean)] = {
      val headTree = position.head._1
      headTree.pl match{
        case t:T[V] => descendToLeftMost(QList((t,false),position))
        case _ => QList((headTree,true),position.tail)
      }
    }

    val newStack = position.head._1.pr match {
      case t : T[V] => descendToLeftMost(QList((t,false),position))
      case _ => unstack1(position)
    }

    if(newStack == null) None
    else Some(new RedBlackTreeMapExplorer[V](newStack))
  }

  def prev:Option[RedBlackTreeMapExplorer[V]] = {
    @tailrec
    def unstack1(position:QList[(T[V],Boolean)]):QList[(T[V],Boolean)] = {
      if (position == null) return null
      val head = position.head
      if (head._2){
        //already presented, so roll back to it.
        QList((head._1,true),position.tail)
      }else{
        //already presented, so unstack
        unstack1(position.tail)
      }
    }

    @tailrec
    def descendToRightMost(position:QList[(T[V],Boolean)]):QList[(T[V],Boolean)] = {
      val headTree = position.head._1
      headTree.pr match{
        case t:T[V] => descendToRightMost(QList((t,true),position))
        case _ => QList((headTree,true),position.tail)
      }
    }

    val newStack = position.head._1.pl match {
      case t : T[V] => descendToRightMost(QList((t,true),QList((position.head._1,false),position.tail)))
      case _ => unstack1(position.tail)
    }

    if(newStack == null) None
    else {
      assert(new RedBlackTreeMapExplorer[V](newStack).next.head.key == this.key, "prev.next.key != this.key; this:" + this + " prev:" + new RedBlackTreeMapExplorer[V](newStack))
      Some(new RedBlackTreeMapExplorer[V](newStack))
    }
  }
}
