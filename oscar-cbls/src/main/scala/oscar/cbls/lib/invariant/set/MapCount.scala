package oscar.cbls.lib.invariant.set

import oscar.cbls.core.computation.ChangingSetValue
import oscar.cbls.core.propagation.Checker
import oscar.cbls.core.{Invariant, SetNotificationTarget}
import oscar.cbls.{CBLSIntVar, SetValue}

import scala.collection.immutable.SortedSet

class MapCount(set:SetValue, map:Long=>Int,counts:Array[CBLSIntVar])
  extends Invariant with SetNotificationTarget{

  registerStaticAndDynamicDependency(set)
  finishInitialization()
  for(count <- counts){
    count := 0
    count.setDefiningInvariant(this)
  }

  for(v <- set.value){
    counts(map(v):Int) :+= 1
  }

  override def notifySetChanges(v: ChangingSetValue,
                                id: Int,
                                addedValues: Iterable[Long],
                                removedValues: Iterable[Long],
                                oldValue: SortedSet[Long],
                                newValue: SortedSet[Long]): Unit = {

    for(a <- addedValues){
      counts(map(a):Int) :+= 1
    }
    for(r <- removedValues){
      counts(map(r):Int) :-= 1
    }
  }

  override def checkInternals(c: Checker): Unit = {

    val scratchCounts:Array[Int] = Array.fill(counts.size)(0)
    for(v <- set.value){
      val i:Int = map(v):Int
      scratchCounts(i) = scratchCounts(i) + 1
    }

    for(i <- counts.indices){
      require(counts(i).value == scratchCounts(i))
    }
  }
}
