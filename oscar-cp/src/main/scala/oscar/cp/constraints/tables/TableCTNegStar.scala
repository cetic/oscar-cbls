package oscar.cp.constraints.tables

import oscar.algo.reversible.ReversibleSparseBitSet
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.{CPIntVar, CPIntVarViewOffset, CPVar}
import oscar.cp.core.{CPStore, Constraint, _}

import scala.collection.mutable.ArrayBuffer

/**
 * Implementation of the Compact Table algorithm (CT) for the table constraint.
 * @param X the variables restricted by the constraint.
 * @param table the list of tuples composing the table.
 * @author Pierre Schaus pschaus@gmail.com
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 *
 * Reference(s) :
 *  - Extending Compact-Table to Negative and Short Tables, Helene Verhaeghe, Christophe Lecoutre, Pierre Schaus, AAAI17
 */
final class TableCTNegStar(X: Array[CPIntVar], table: Array[Array[Int]], star: Int = -1, needPreprocess: Boolean = true) extends Constraint(X(0).store, "TableCTNegStar") {

  override def associatedVars(): Iterable[CPVar] = X

  /* Set default star value */
  private[this] val _star = -1

  /* Setting idempotency & lower priority for propagate() */
  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1

  /* Basic information and precomputed datas*/
  private[this] val arity = X.length

  private[this] val spans = Array.tabulate(arity)(i => X(i).max - X(i).min + 1)

  private[this] val offsets = Array.tabulate(arity)(i => X(i).min)
  private[this] val filteredTable = table.filter(t => (0 until arity).forall(i => X(i).hasValue(t(i)) || t(i) == star))

  private[this] val T0 = Array.tabulate(filteredTable.length, arity) { case (t, i) => if (filteredTable(t)(i) == star) _star else filteredTable(t)(i) - offsets(i) }

  private[this] val x = Array.tabulate(arity)(i => new CPIntVarViewOffset(X(i), -offsets(i)))

  private[this] val preprocessedTable = if (needPreprocess) preprocess else T0
  private[this] val nbTuples = preprocessedTable.length

  /* Maximum number of combination of star positions */
  private[this] val maxNbGroup = Math.pow(2, arity).toInt
  private[this] val sizeTemp: Array[Int] = Array.tabulate(arity)(i => x(i).size)
  private[this] val multiplicator = Array.fill(maxNbGroup)(1)
  private[this] val multiplicatorsNeeded = Array.fill(maxNbGroup)(0)
  private[this] val multCheck = Array.fill(arity, maxNbGroup)(() => ())
  private[this] val multiplicatorFormulaCheck = Array.fill(maxNbGroup)(() => ())
  private[this] val multiplicatorLook = Array.fill(maxNbGroup)(0)
  private[this] val mult = {
    val temp = Array.fill(arity, maxNbGroup)(() => 1)
    var n = 1
    var j = arity
    while (j > 0) {
      j -= 1
      var k = maxNbGroup
      while (k > 0) {
        k -= 1
        if ((n & k) == 0) {
          val p = k
          temp(j)(k) = () => multiplicator(p)
          multCheck(j)(k) = () => multiplicatorFormulaCheck(p)()
        } else {
          val p = k - n
          temp(j)(k) = () => multiplicator(p)
          multCheck(j)(k) = () => multiplicatorFormulaCheck(p)()
        }
      }
      n *= 2
    }
    temp
  }
  private[this] val multiplicatorFormula = Array.fill(maxNbGroup)(() => 1)

  private[this] def computeFormula() = {
    var i = arity
    var n = 1
    val initarray = Array.fill(arity)(Array(0))
    while (i > 0) {
      i -= 1
      initarray(i)(0) = n
      val k = i
      multiplicatorFormula(n) = () => sizeTemp(k)
      val k2 = n
      multiplicatorFormulaCheck(n) =
        () => {multiplicatorLook(k2) = 1}
      n *= 2
    }
    def compute(setofset: Array[Array[Int]]): Unit = {
      val newarray = new Array[Array[Int]]((setofset.length + 1) / 2)
      if (setofset.length % 2 != 0)
        newarray(newarray.length - 1) = setofset(setofset.length - 1)
      var j = setofset.length / 2
      while (j > 0) {
        j -= 1
        val a1 = setofset(j * 2)
        val a2 = setofset(j * 2 + 1)
        var pos = a1.length
        val pos2 = a2.length
        val ares = new Array[Int](pos2 + pos * (pos2 + 1))
        System.arraycopy(a1, 0, ares, 0, pos)
        System.arraycopy(a2, 0, ares, pos, pos2)
        var a = pos
        pos += pos2
        while (a > 0) {
          a -= 1
          var b = pos2
          while (b > 0) {
            b -= 1
            val k1 = a1(a)
            val k2 = a2(b)
            val id = k1 + k2
            ares(pos) = id
            multiplicatorFormula(id) = () => multiplicator(k1) * multiplicator(k2)
            multiplicatorFormulaCheck(id) = () => {multiplicatorLook(id) = 1;multiplicatorFormulaCheck(k1)();multiplicatorFormulaCheck(k2)()}
            pos += 1
          }
        }
        newarray(j) = ares
      }
      if (newarray.length > 1)
        compute(newarray.toArray)
    }
    compute(initarray)
  }
  computeFormula()

  private[this] val T: Map[Int, Array[Array[Int]]] = preprocessedTable.groupBy(tup => tup.foldLeft(0)((a, b) => a * 2 + (if (b == _star) 1 else 0)))
  private[this] val nonEmptyGroupId = {
    val buf = ArrayBuffer[Int]()
    var i = maxNbGroup
    while (i > 0) {
      i -= 1
      if (T.contains(i)) {
        buf += i
        var j = arity
        while(j > 0){
          j-=1
          multCheck(j)(i)()
        }
      }
    }
    buf.toArray
  }
  private[this] val mutiplicatorNeededSparse= multiplicatorLook.zipWithIndex.filter(a => a._1 == 1).map(_._2)
  private[this] val multiplicatorNeededSize = mutiplicatorNeededSparse.length
  private[this] val mutiplicatorNeededSparseByVar= {
    val temp = new Array[Array[Int]](arity)
    var n = 1
    var i = arity
    while (i >0){
      i-=1
      temp(i) = mutiplicatorNeededSparse.filter(m => (m & n) != 0)
      n*=2
    }
    temp
  }
  private[this] val multiplicatorNeededSizeByVar = Array.tabulate(arity)(i => mutiplicatorNeededSparseByVar(i).length)
  private[this] val nonEmptyGroupIdSize = nonEmptyGroupId.length

  /* Computed information about the repartition by group into the BitSets*/
  private[this] val blockOffset = Array.fill(maxNbGroup)(0)
  private[this] val setID = scala.collection.mutable.Set[Int]()
  private[this] val nbBlock = {
    var currentOffset = 0
    var i = 0
    while (i < maxNbGroup) {
      blockOffset(i) = currentOffset
      if (T.contains(i)) {
        setID ++= T(i).indices.map(_ + currentOffset * 64)
        currentOffset += (((T(i).length - 1) / 64) + 1)
      }
      i += 1
    }
    currentOffset
  }
  private[this] val realOffset = blockOffset.map(_ * 64)
  private[this] val hashMult = {
    val temp = Array.fill(nbBlock)(0)
    var i = 0
    var hash = 0
    while (hash < maxNbGroup - 1) {
      var j = blockOffset(hash + 1) - blockOffset(hash)
      while (j > 0 && i < nbBlock) {
        j -= 1
        temp(i) = hash
        i += 1
      }
      hash += 1
    }
    while (i < nbBlock) {
      temp(i) = hash
      i += 1
    }
    temp
  }

  private[this] val maxDomain = X.maxBy(_.size).size
  private[this] val domainArray = new Array[Int](maxDomain)
  private[this] var domainArraySize = 0

  private[this] val dangerousTuples = new ReversibleSparseBitSet(s, nbBlock * 64, setID)
  private[this] val variableValueAntiSupportsRM = Array.tabulate(arity)(i => new Array[dangerousTuples.BitSet](spans(i)))
  private[this] val variableValueAntiSupports = Array.tabulate(arity)(i => new Array[dangerousTuples.BitSet](spans(i)))
  private[this] val deltas: Array[DeltaIntVar] = new Array[DeltaIntVar](arity)

  /**
   * Update the multiplicators for each group of tuples and each variable concerned
   */
  /*private[this]*/ def updateMultiplicator() = {
    var i = 0
    while (i < multiplicatorNeededSize) {
      // has to be updated in this order!!
      val id = mutiplicatorNeededSparse(i)
      multiplicator(id) = multiplicatorFormula(id)()
      i += 1
    }
  }

  /*private[this]*/ def updateMultiplicator(varId:Int) = {
    var i = 0
    while (i < multiplicatorNeededSizeByVar(varId)) {
      // has to be updated in this order!!
      val id = mutiplicatorNeededSparseByVar(varId)(i)
      multiplicator(id) = multiplicatorFormula(id)()
      i += 1
    }
  }

  /**
   * Method to compute the table without intersections
   * @return The new table, without any overlap
   */
  private def preprocess: Array[Array[Int]] = {

    val t = System.currentTimeMillis()
    val orderedVars = Array.tabulate(arity)(i => i)
    orderedVars.sortBy(x(_).size)

    val range = 0 until arity
    val ordTuple: Ordering[(Array[Int], Int)] = Ordering.by(t => t._2)
    val priorQueue = new scala.collection.mutable.PriorityQueue[(Array[Int], Int)]()(ordTuple)
    val keepSet = scala.collection.mutable.Set[Array[Int]]()

    T0.foreach { tuple =>
      var count = 1
      range.foreach(i => if (tuple(i) == _star) count *= x(i).size)
      priorQueue += ((tuple, count))
    }

    while (priorQueue.nonEmpty) {
      val elem = priorQueue.dequeue()
      if (keepSet.forall(tuple => !range.forall(i => tuple(i) == elem._1(i) || tuple(i) == star || elem._1(i) == star))) {
        keepSet += elem._1
      } else {
        if (elem._2 != 1) {
          /* discard direct if no star and overlap */
          val index = orderedVars.find(i => elem._1(i) == _star).get
          val newPriority = elem._2 / x(index).size
          for (vl <- x(index).iterator) {
            val newelem = elem._1.clone()
            newelem(index) = vl
            priorQueue.enqueue((newelem, newPriority))
          }
        }
      }

    }

    val array = keepSet.toArray
    array
  }

  override def setup(l: CPPropagStrength): Unit = {

    /* Success if table is empty initially or after initial filtering */
    if (nbTuples == 0) {
      deactivate()
      return
    }

    /* Retrieve the current valid tuples */
    val (dangerous, dangerousByHash) = collectDangerousTuples()

    if (dangerous.isEmpty){
      deactivate()
      return
    }

    /* Remove non dangerous tuples */
    dangerousTuples.collect(new dangerousTuples.BitSet(dangerous))
    dangerousTuples.intersectCollected()


    /* Compute AntiSupports = Compute for each for each variable/value pair the dangerous tuples */
    computeAntiSupports(dangerousByHash)

    /* Call propagate() when domains change */
    var i = 0
    while (i < arity) {
      deltas(i) = x(i).callPropagateOnChangesWithDelta(this)
      i += 1
    }

    /* Propagate a first time */
    basicPropagate()
  }

  private[this] def showTable(): Unit = {
    table.foreach { t =>
      println(t.mkString("\t"))
    }
    println("star value:" + star)
    println("domains:" + X.mkString(","))
  }

  /**
   * Invalidates tuples by handling delta, the set of values removed from D(x) since the last call to this function.
   * @param varIndex the index of x in the array of variables.
   * @param delta the set of values removed since the last call.
   * @return the outcome i.e. Failure or Success.
   */
  @inline private def updateDelta(varIndex: Int, delta: DeltaIntVar): Boolean = {

    val intVar = x(varIndex)
    val varSize = intVar.size

    dangerousTuples.clearCollected()

    /* Update the value of dangerousTuples by considering D(x) or delta */

    if (varSize == 1) {

      /* The variable is assigned */
      dangerousTuples.collect(variableValueAntiSupports(varIndex)(intVar.min))
      dangerousTuples.intersectCollected()

    } else {

      if (delta.size < varSize) {

        /* Use delta to update dangerousTuples */
        domainArraySize = delta.fillArray(domainArray)
        var i = 0
        /* Collect all the removed tuples by doing or's with precomputed masks */
        while (i < domainArraySize) {
          dangerousTuples.collect(variableValueAntiSupportsRM(varIndex)(domainArray(i)))
          i += 1
        }
        /* Remove from the anti-supports all the collected tuples, no longer dangerous */
        dangerousTuples.removeCollected()

      } else {

        /* Don't use delta = reset strategy = recompute from the domain */
        domainArraySize = intVar.fillArray(domainArray)
        var i = 0
        while (i < domainArraySize) {
          dangerousTuples.collect(variableValueAntiSupports(varIndex)(domainArray(i)))
          i += 1
        }
        /* Intersect the set of dangrous tuples with the dangerous tuples collected */
        dangerousTuples.intersectCollected()

      }
    }

    /* Success if there are no more dangerous tuples */
    dangerousTuples.isEmpty()
  }

  /**
   * Perform a consistency check : for each variable value pair (x,a), we check if
   * the number of dangerous tuples doesn't exceed all the possible tuples with the value.
   * Unsupported values are removed.
   * @return the outcome i.e. Failure or Success.
   */
  override def propagate(): Unit = {

    var varIndex = arity
    while (varIndex > 0) {
      varIndex -= 1
      if (deltas(varIndex).size > 0) {
        if (updateDelta(varIndex, deltas(varIndex))) {
          this.deactivate()
          return
        }
      }
    }
    basicPropagate()
  }

  /**
   * Heart of the propagation step
   * Loop on the variable-values until no changes
   * Remove the pair if the number of tuple as reach the threshold
   * @return the outcome i.e. Failure or Suspend
   */
  @inline def basicPropagate(): Unit = {

    var varIndex = arity
    while (varIndex > 0) {
      varIndex -= 1
      sizeTemp(varIndex) = x(varIndex).size
    }

    updateMultiplicator()

    var cardinalSizeInit = 1L
    varIndex = arity
    while (varIndex > 0) {
      varIndex -= 1
      cardinalSizeInit *= sizeTemp(varIndex)
    }

    varIndex = arity
    while (varIndex > 0) {
      varIndex -= 1

      domainArraySize = x(varIndex).fillArray(domainArray)
      var i = domainArraySize
      var value = 0
      val cardinalSize = cardinalSizeInit / sizeTemp(varIndex)

      while (i > 0) {
        i -= 1
        value = domainArray(i)
        val count = dangerousTuples.intersectCount(variableValueAntiSupports(varIndex)(value), hashMult, mult(varIndex))
        if (count == cardinalSize) {
          x(varIndex).removeValue(value)
          dangerousTuples.clearCollected()
          dangerousTuples.collect(variableValueAntiSupportsRM(varIndex)(value))
          dangerousTuples.removeCollected()
          if (dangerousTuples.isEmpty()) {
            this.deactivate()
            return
          }
          cardinalSizeInit /= sizeTemp(varIndex)
          sizeTemp(varIndex) -= 1
          cardinalSizeInit *= sizeTemp(varIndex)
          updateMultiplicator(varIndex)
        }
      }
    }
  }

  /* ----- Functions used during the setup of the constraint ----- */

  /**
   * Retrieve the dangerous tuples from the table and store their index in dangerousTuplesBuffer.
   * @return the ArrayBuffer containing the dangerous tuples.
   */
  @inline private def collectDangerousTuples(): (ArrayBuffer[Int], Array[ArrayBuffer[Int]]) = {
    val dangerousTuplesBuffer = ArrayBuffer[Int] ()
    val dangerousByHash = Array.fill(maxNbGroup)(ArrayBuffer[Int] ())

    var i = nonEmptyGroupIdSize
    while (i > 0) {
      i -= 1
      val hash = nonEmptyGroupId(i)
      var tupleIndex = T(hash).length
      while (tupleIndex > 0) {
        tupleIndex -= 1
        if (isTupleDangerous(hash, tupleIndex)) {
          dangerousTuplesBuffer += tupleIndex + realOffset(hash)
          dangerousByHash(hash) += tupleIndex
        }
      }
    }

    (dangerousTuplesBuffer, dangerousByHash)
  }

  /**
   * Check if a tuple is dangerous.
   * @param tupleIndex the index of the tuple in the table.
   * @return true if the tuple is dangerous, false otherwise.
   */
  @inline private def isTupleDangerous(hash: Int, tupleIndex: Int): Boolean = {
    var varIndex = 0
    while (varIndex < arity) {
      if (!x(varIndex).hasValue(T(hash)(tupleIndex)(varIndex)) && T(hash)(tupleIndex)(varIndex) != _star) {
        return false
      }
      varIndex += 1
    }
    true
  }

  /**
   * Compute the mask for each variable value pair (x,a).
   */
  @inline private def computeAntiSupports(dangerous: Array[ArrayBuffer[Int]]): Unit = {

    val varValueSupports = Array.tabulate(x.length)(i => Array.tabulate(spans(i))(v => new ArrayBuffer[Int]()))
    val varValueSupportsStar = Array.fill(x.length)(new ArrayBuffer[Int]())

    /* Collect the supports */
    var j = nonEmptyGroupIdSize
    while (j > 0) {
      j -= 1
      val hash = nonEmptyGroupId(j)
      var dangerousIndex = 0
      while (dangerousIndex < dangerous(hash).length) {
        val tupleIndex = dangerous(hash)(dangerousIndex)
        var varIndex = 0
        while (varIndex < arity) {
          val value = T(hash)(tupleIndex)(varIndex)
          if (value == _star)
            varValueSupportsStar(varIndex) += tupleIndex + realOffset(hash)
          else
            varValueSupports(varIndex)(value) += tupleIndex + realOffset(hash)
          varIndex += 1
        }
        dangerousIndex += 1
      }
    }

    /* Create the final support bitSets */
    for (varIndex <- variableValueAntiSupports.indices) {
      for (valueIndex <- variableValueAntiSupports(varIndex).indices) {
        variableValueAntiSupports(varIndex)(valueIndex) = new dangerousTuples.BitSet(varValueSupports(varIndex)(valueIndex) ++ varValueSupportsStar(varIndex))
        variableValueAntiSupportsRM(varIndex)(valueIndex) = new dangerousTuples.BitSet(varValueSupports(varIndex)(valueIndex))
      }
    }
  }
}


