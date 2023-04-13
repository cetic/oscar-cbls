package oscar.cbls.lib.invariant.set

import oscar.cbls.core.computation.{CBLSIntVar, ChangingSetValue, Invariant, SetNotificationTarget, SetValue}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

class MapCount(set:SetValue, map:Int=>Int,counts:Array[CBLSIntVar])
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
                                addedValues: Iterable[Int],
                                removedValues: Iterable[Int],
                                oldValue: SortedSet[Int],
                                newValue: SortedSet[Int]): Unit = {

    for(a <- addedValues){
      counts(map(a):Int) :+= 1
    }
    for(r <- removedValues){
      counts(map(r):Int) :-= 1
    }
  }

  override def checkInternals(c: Checker): Unit = {

    val scratchCounts:Array[Int] = Array.fill(counts.length)(0)
    for(v <- set.value){
      val i:Int = map(v):Int
      scratchCounts(i) = scratchCounts(i) + 1
    }

    for(i <- counts.indices){
      require(counts(i).value == scratchCounts(i))
    }
  }
}
