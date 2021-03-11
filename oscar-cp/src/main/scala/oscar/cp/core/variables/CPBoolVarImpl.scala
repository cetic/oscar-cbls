package oscar.cp.core.variables

import oscar.algo.Inconsistency

import scala.util.Random
import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.TrailEntry
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.watcher.WatcherListL2
import oscar.cp.core.watcher.WatcherListL1
import oscar.cp.core.watcher.Watcher
import oscar.cp.core.delta.DeltaIntVar

import scala.collection.mutable

/**
 * @author Renaud Hartert ren.hartert@gmail.com
 */

class CPBoolVarImpl private(final override val store: CPStore, initDomain: Int, final override val name: String = "") extends CPBoolVar with TrailEntry {
  
  import CPBoolVarImpl._

  override val context = store

  // Registered constraints
  private[this] val onBindL2 = new WatcherListL2(store)
  private[this] val onBoundsL1 = new WatcherListL1(store)
  private[this] val onBindL1 = new WatcherListL1(store)
  private[this] val onDomainL1 = new WatcherListL1(store)

  // Number of constraints registered on the variable
  private[this] val degree = new ReversibleInt(store, 0) // should not change often
  
  // 00 : False
  // 11 : True
  // 10 : Unassigned
  // 01 : Empty
  private[this] var domain: Int = initDomain
  
  // Domain to restore when a backtrack occurs
  private[this] var trailedDomain: Int = UNASSIGNED

  final override def transform(v: Int) = v

  final override def isBound = domain != UNASSIGNED

  final override def size = {
    if (domain == UNASSIGNED) 2
    else if (domain == EMPTY) 0
    else 1
  }

  final override def isEmpty = domain == EMPTY
  
  final override def min: Int = domain & 1 // min is faster than max

  final override def max: Int = (domain & 2) >> 1
  
  final override def isTrue: Boolean = domain == TRUE

  final override def isFalse: Boolean = domain == FALSE

  final override def isBoundTo(value: Int): Boolean = {
    if (value == 0) domain == FALSE
    else if (value == 1) domain == TRUE
    else false
  }
  
  final override def containsTrue: Boolean = {
    if (domain == EMPTY) false
    else domain >= UNASSIGNED 
  }
  
  final override def containsFalse: Boolean = {
    if (domain == EMPTY) false
    else domain <= 2
  }

  final override def hasValue(value: Int): Boolean = {
    if (domain == EMPTY) false
    else if (value == 0) domain <= UNASSIGNED
    else if (value == 1) domain >= UNASSIGNED
    else false
  }

  final override def valueAfter(value: Int): Int = {
    if (value <= 0) if (domain <= UNASSIGNED) 0 else 1
    else value
  }

  final override def valueBefore(value: Int): Int = {
    if (value >= 1) if (domain >= UNASSIGNED) 1 else 0
    else value
  }

  final override def randomValue(rand: Random): Int = {
    if (domain == UNASSIGNED) rand.nextInt(2)
    else domain & 1 // min value
  }

  final override def updateMin(value: Int): Unit = {
    if (value == 1) {
      if (domain == UNASSIGNED) setDomainTrue()
      else if (domain != TRUE) setDomainEmpty()
    }
    else if (value > 0) setDomainEmpty()
  }

  final override def updateMax(value: Int): Unit = {
    if (value == 0) {
      if (domain == UNASSIGNED) setDomainFalse()
      else if (domain != FALSE) setDomainEmpty()
    }
    else if (value < 1) setDomainEmpty()
  }
  
  final override def assignTrue(): Unit = {
    if (domain == UNASSIGNED) setDomainTrue()
    else if (domain != TRUE) setDomainEmpty()
  }

  final override def assignFalse(): Unit = {
    if (domain == UNASSIGNED) setDomainFalse()
    else if (domain != FALSE) setDomainEmpty()
  }
    
  final override def assign(value: Int): Unit = {
    if (value == 0) assignFalse()
    else if (value == 1) assignTrue()
    else throw Inconsistency
  }

  final override def removeValue(value: Int) = {
    if (value == 0) assignTrue() 
    else if (value == 1) assignFalse()
  }
  
  final override def restore(): Unit = {
    domain = trailedDomain
    trailedDomain = UNASSIGNED
  }

  @inline private def setDomainTrue(): Unit = {
    store.trail(this)
    trailedDomain = domain
    domain = TRUE
    // Notify constraints
    onDomainL1.enqueueRemove(0)
    onBoundsL1.enqueueBounds()
    onBindL1.enqueueBind()
    onBindL2.enqueue()
  }

  @inline private def setDomainFalse(): Unit = {
    store.trail(this)
    trailedDomain = domain
    domain = FALSE
    // Notify constraints
    onDomainL1.enqueueRemove(1)
    onBoundsL1.enqueueBounds()
    onBindL1.enqueueBind()
    onBindL2.enqueue()
  }

  @inline private def setDomainEmpty(): Unit = {
    store.trail(this)
    trailedDomain = domain
    domain = EMPTY
    throw Inconsistency
  }

  final override def iterator = {
    if (domain == UNASSIGNED) Iterator(0, 1)
    else if (domain == FALSE) Iterator(0)
    else if (domain == TRUE) Iterator(1)
    else Iterator.empty
  }
  
  final override def restrict(newDomain: Array[Int], newSize: Int): Unit = {
    assert(newSize > 0 && newSize <= size )
    if (newSize == 1) {
      val value = newDomain(0)
      assert(value == 1 || value == 0)
      if (value == 0) {
        assert(domain == FALSE)
        setDomainFalse()
      } else {
        assert(domain == TRUE)
        setDomainTrue()
      }
    }   
  }

    
  final override def constraintTrue(): Constraint = new oscar.cp.constraints.EqCons(this, 1)

  final override def constraintFalse(): Constraint = new oscar.cp.constraints.EqCons(this, 0)

  final override lazy val not: CPBoolVar = new CPBoolVarNot(this)

  final override def toString: String = {
    domain match { // tableswitch
      case FALSE => "0"
      case EMPTY => "empty"
      case UNASSIGNED => "{0, 1}"
      case TRUE => "1"
      case _ => sys.error("unknown domain")
    }
  }
  
  final override def constraintDegree: Int = degree.value

  final override def callPropagateWhenBind(c: Constraint): Unit = {
    degree.incr()
    onBindL2.register(c)
  }

  final override def callPropagateWhenBoundsChange(c: Constraint): Unit = {
    degree.incr()
    onBindL2.register(c)
  }

  final override def callPropagateWhenBoundsChange(c: Constraint, cond: => Boolean): Unit = {
    degree.incr()
    onBindL2.register(c, cond)
  }

  final override def callPropagateWhenDomainChanges(c: Constraint): Unit = {
    degree.incr()
    onBindL2.register(c)
  }
  
  final override def callPropagateOnChangesWithDelta(c: Constraint): DeltaIntVar = {
    val snap = delta(c)
    degree.incr()
    onBindL2.register(c)
    snap
  }
  
  final override def callPropagateOnChangesWithDelta(c: Constraint, cond: => Boolean): DeltaIntVar = {
    val snap = delta(c)
    degree.incr()
    onBindL2.register(c, cond)
    snap
  }
  
  def callPropagateWhenDomainChanges(c: Constraint, cond: => Boolean): Unit = {
    degree.incr()
    onBindL2.register(c, cond)
  }

  def awakeOnChanges(watcher: Watcher): Unit = {
    degree.incr()
    onBindL2.register(watcher)
  }

  final override def callValBindWhenBind(c: Constraint): Unit = {
    callValBindWhenBind(c, this)
  }

  final override def callValBindWhenBind(c: Constraint, variable: CPIntVar): Unit = {
    degree.incr()
    onBindL1.register(c, variable)
  }

  final override def callUpdateBoundsWhenBoundsChange(c: Constraint): Unit = {
    callUpdateBoundsWhenBoundsChange(c, this)
  }

  final override def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntVar): Unit = {
    degree.incr()
    onBoundsL1.register(c, variable)
  }

  final override def callValRemoveWhenValueIsRemoved(c: Constraint): Unit = {
    callValRemoveWhenValueIsRemoved(c, this)
  }

  final override def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar): Unit = {
    degree.incr()
    onDomainL1.register(c, variable)
  }

  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int): Unit = {
    callValRemoveIdxWhenValueIsRemoved(c, this, idx)
  }

  final override def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int): Unit = {
    degree.incr()
    onDomainL1.register(c, variable, idx)
  }

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int): Unit = {
    callUpdateBoundsIdxWhenBoundsChange(c, this, idx)
  }

  final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntVar, idx: Int): Unit = {
    degree.incr()
    onBoundsL1.register(c, variable, idx)
  }

  final override def callValBindIdxWhenBind(c: Constraint, idx: Int): Unit = {
    callValBindIdxWhenBind(c, this, idx)
  }

  final override def callValBindIdxWhenBind(c: Constraint, variable: CPIntVar, idx: Int): Unit = {
    degree.incr()
    onBindL1.register(c, variable, idx)
  }

  // ----------------------------------
  
  final override def fillDeltaArray(oldMin: Int, oldMax: Int, oldSize: Int, arr: Array[Int]): Int = {
    var i = 0
    if(oldMin != min){
      arr(i) = 0
      i += 1
    }
    if(oldMax != max){
      arr(i) = 1
      i += 1
    }
    i
  }

  final override def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
    val newarray = new Array[Int](oldSize - size)
    fillDeltaArray(oldMin, oldMax, oldSize, newarray)
    newarray.iterator
  }

  @inline override def _foreach[U](f: Int => U): Unit = {
    throw new RuntimeException("This should never be called, as it is implemented in CPIntVar")
  }
}

object CPBoolVarImpl {
  
  // The first bit corresponds to the min value.
  // The second bit corresponds to the max value. 
  // Empty is represented by 1
  //
  // 00 : False
  // 11 : True
  // 10 : Unassigned
  // 01 : Empty
  private final val FALSE = 0
  private final val TRUE = 3
  private final val UNASSIGNED = 2
  private final val EMPTY = 1
  
  def apply(store: CPStore, assignedValue: Boolean, name: String): CPBoolVar = {
    if (assignedValue) new CPBoolVarImpl(store, TRUE, name)
    else new CPBoolVarImpl(store, FALSE, name)
  }
  
  def apply(store: CPStore, assignedValue: Int, name: String): CPBoolVar = {
    if (assignedValue == 0) new CPBoolVarImpl(store, FALSE, name)
    else if (assignedValue == 1) new CPBoolVarImpl(store, TRUE, name)
    else sys.error("assignedValue needs to be 0 or 1.")
  }
  
  def apply(store: CPStore, name: String): CPBoolVar = new CPBoolVarImpl(store, UNASSIGNED, name)
}