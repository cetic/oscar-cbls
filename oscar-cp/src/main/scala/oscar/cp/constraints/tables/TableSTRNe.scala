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


package oscar.cp.constraints.tables

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.{CPPropagStrength, CPStore, Constraint}
import oscar.cp.core.variables.{CPIntVar, CPVar}

/**
  *
  *  Making Simple Tabular Reduction Works on Negative Table Constraints
  *  Hongbo Li, Yanchun Liang, Jinsong Guo and Zhanshan Li, AAAI13
  *
  * @author ThanhKM thanhkhongminh@gmail.com
  */
class TableSTRNe(val variables: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(variables(0).store, "TableSTRNe") {
  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1

  override def associatedVars(): Iterable[CPVar] = variables

  private[this] val arity = variables.length
  // Ssup, Sval
  private[this] var sSupLimit = -1
  private[this] val sSup = Array.tabulate(arity)(i => i)  
  private[this] var sValLimit = -1
  private[this] val sVal = Array.tabulate(arity)(i => i)

  // Global time-stamp increased at each propagate
  private[this] var timeStamp = 0
  // domValues is used for speedup validity check 
  private[this] val values = Array.ofDim[Int](variables.map(_.size).max)
  private[this] val domValues = Array.tabulate(arity)(i => Array.fill(variables(i).max-variables(i).min+1)(-1))
  
  // Dense + sparse set for (variable, value) (unsupported set)
  private[this] val unsDense = Array.tabulate(arity)(i => new Array[Int](variables(i).size)) // store values
  private[this] val unsSparse = Array.tabulate(arity)(i => new Array[Int](variables(i).max - variables(i).min + 1)) // sparse
  private[this] val minValues = Array.tabulate(arity)(i => variables(i).min) // min values of each variables 
  private[this] val unsSizes = Array.tabulate(arity)(i => variables(i).size) // size of sparse set

  //// Remove invalid tuples to obtain filtered table
  private[this] val fTable = table.filter(t => (0 until arity).forall(i => variables(i).hasValue(t(i))))
  // Sparse set for current table
  private[this] val position = Array.tabulate(fTable.length)(i => i)
  private[this] val currentLimit = new ReversibleInt(s, fTable.length - 1)
  
  // variable's domain size since last invocation 
  private[this] val lastSizes = Array.tabulate(variables.length)(i => new ReversibleInt(s, -1))  
  
  //TODO: fix 'count' and use gac to speedup unsupport
  // count(x,a): number of support possibles for each literal (x,a)
  private[this] val count = Array.tabulate(arity)(i => new Array[Int](variables(i).max - variables(i).min+1))

  override def setup(l: CPPropagStrength): Unit = {
    propagate()
    variables.filter(!_.isBound).foreach(_.callPropagateWhenDomainChanges(this))
  }

  override def propagate(): Unit = {
    timeStamp += 1
    var limit = currentLimit.getValue()
    //---------------------- initialize -------------------------------------/
    sValLimit = -1 // reset Sval
    sSupLimit = -1 // reset Ssup
    var i = 0
    while (i < arity) {
    	copyDomainsToUns(i)
      sSupLimit += 1
      sSup(sSupLimit) = i

      // SVal
      if (variables(i).size != lastSizes(i).getValue) {
        sValLimit += 1
        sVal(sValLimit) = i
        lastSizes(i).setValue(variables(i).size)        
        // update domValues, speed up check validity
        var k = variables(i).fillArray(values)
        while (k > 0) {
          k -= 1
          domValues(i)(values(k)-minValues(i)) = timeStamp
        }
      }
      i += 1
    }
    
    // calculate the number of tuple possibles for this constraint 
    i = 0
    var nTuplePossibles = 1
    while (i < arity) {
      nTuplePossibles *= variables(i).size
      i += 1
    }
    // calculate count(x,a) for each variable. count(x,a) = nTuplePossibles / dom(x)
    // if count(x,a) > tableSize, it means (x,a) is GAC
    i = arity - 1
    while (i >= 0) {
      val nTuples = nTuplePossibles / variables(i).size
      if (nTuples > limit + 1) {
        sSup(i) = sSup(sSupLimit)
        sSupLimit -= 1
      } else {
        // update count(x,a)
        var k = variables(i).fillArray(values)
        while (k > 0) {
          k -= 1
          count(i)(values(k)-minValues(i)) = nTuples
        }
      }
      i -= 1
    }
    //-------------- iterate global table ------------------------------/
    i = limit
    while (i >= 0) {
      val tau = fTable(position(i))
      if (!isValidTupleWithSVal(tau)) {
        // remove tuple
        swap(position, i, limit)
        limit -= 1
      }
      i -= 1
    }
    i = limit
    while (i >= 0) { // && sSupLimit > -1
      val tau = fTable(position(i))
      unsupport(tau, limit+1)
      i -= 1
    }

    currentLimit.setValue(limit)
    //---------------- update domains ---------------------------------/
    if (limit == -1) {
      deactivate()
      return
    }
		updateDomains()
  }
  
  /*****************************************************************************/
  /************************** Helper functions *********************************/
  /**
   * Copy domain of variables to sparse set
   */
  @inline private def copyDomainsToUns(varId: Int): Unit = {
    val size = variables(varId).fillArray(unsDense(varId))
    unsSizes(varId) = size
    var i = 0
    while (i < size) {
      val value = unsDense(varId)(i)
      unsSparse(varId)(value - minValues(varId)) = i
      i += 1
    }
  }
  
  /**
   * Remove (varId, value) from Uns.
   * @return new size of Uns(varId)
   */
  @inline private def removeFromUns(varId: Int, value: Int): Unit = {
    val valIndex = unsSparse(varId)(value - minValues(varId))
    // value
    val size = unsSizes(varId)
    if (valIndex < size) {
      val value2 = unsDense(varId)(size - 1)
      val valIndex2 = size - 1
      swap(unsDense(varId), valIndex, valIndex2)
      swap(unsSparse(varId), value - minValues(varId), value2 - minValues(varId))
      unsSizes(varId) = size - 1
    }
  }
  
  /**
   *  Reduce count for each value of variable in this tuple
   */
  @inline private def unsupport(tau: Array[Int], tbSize: Int): Unit = {
    var i = sSupLimit
    while (i >= 0) {
      val varId = sSup(i)
      val offValue = tau(varId) - minValues(varId)
      count(varId)(offValue) -= 1
      // when count(x,a) > tbSize, it means (x,a) is GAC, we have to remove it
      if (count(varId)(offValue) > tbSize) {
        // Remove from Uns
        removeFromUns(varId, offValue + minValues(varId))
        if (unsSizes(varId) == 0) {
          sSup(i) = sSup(sSupLimit)
          sSupLimit -= 1
        }
      }
      i -= 1
    }
  }
  
  /**
   * Update variables' domain and return Outcome i.e. Suspend, Failure,...
   */
  @inline private def updateDomains(): Unit = {
    var i = 0
    while (i <= sSupLimit) {
      val varId = sSup(i)
      val size = unsSizes(varId)
      var j = 0
      while (j < size) {
        val value = unsDense(varId)(j)        
        if (count(varId)(value - minValues(varId)) == 0) {
          if (variables(varId).size == 1)
            throw Inconsistency
          variables(varId).removeValue(value)
        }
        j += 1
      }
      // Can not do like STR2 because after remove some values, the global table is not GAC
      // i.e. not all tuple are GAC.
      // lastSizes(varId).setValue(variables(varId).size)
      i += 1
    }
  }
  
  /**
   *  Check tuple validity with SVal
   */
  @inline private def isValidTupleWithSVal(tau: Array[Int]): Boolean = {
    var i = 0
    while (i <= sValLimit) {
      val varId = sVal(i)
      if (domValues(varId)(tau(varId)-minValues(varId)) != timeStamp) return false
      //if (!variables(varId).hasValue(tau(varId)))  return false
      i += 1
    }
    true
  }

  /**
   *  Check valid tuple by presence of all values in variables' domain
   */
  @inline private def isValidTuple(tau: Array[Int]): Boolean = {
    var varId = 0
    while (varId < arity) {
      if (!variables(varId).hasValue(tau(varId)))
        return false        
      varId += 1
    }
    true
  }
  
  @inline private def swap(arrays: Array[Int], i1: Int, i2: Int) = {
    val tmp = arrays(i1)
    arrays(i1) = arrays(i2)
    arrays(i2) = tmp
  }
  
  @inline private def printArray(arrays: Array[Int]) = {
    for (a <- arrays) print(s"$a ")
    println
  }
}
