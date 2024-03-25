package oscar.cbls.core.computation.set

abstract class ChangingSetValue(initialValue: SortedSet[Int], initialDomain: Domain)
    extends AbstractVariable
    with SetValue {

  require(
    initialDomain.max <= Int.MaxValue,
    s"The max value of the domain is lesser or equal to Int.MaxValue. Got : ${initialDomain.max}"
  )

  private var privatedomain: Domain      = initialDomain
  private var m_NewValue: SortedSet[Int] = initialValue
  private var OldValue: SortedSet[Int]   = m_NewValue
  private[this] var domainSizeDiv10      = privatedomain.size / 10

  def domain: Domain = privatedomain

  override def snapshot: ChangingSetValueSnapShot =
    new ChangingSetValueSnapShot(this.uniqueID, this.value)

  def valueAtSnapShot(s: Solution): SortedSet[Int] = s(this) match {
    case s: ChangingSetValueSnapShot => s.savedValue
    case _ => throw new Error("cannot find value of " + this + " in snapshot")
  }

  override def createCheckpoint: VariableCheckpoint = ???

  /** this must be protected because invariants might rework this after isntanciation for CBLSVars,
    * no problems
    */
  protected def restrictDomain(d: Domain): Unit = {
    privatedomain = privatedomain.intersect(d)
    domainSizeDiv10 = privatedomain.size / 10
  }

  override def toString: String = name + ":={" +
    (if (model == null || model.propagateOnToString) value
     else m_NewValue).mkString(",") + "}"

  /** this method is a toString that does not trigger a propagation. use this when debugging your
    * software. you should specify to your IDE to render variable objects using this method isntead
    * of the toString method
    *
    * @return
    *   a string similar to the toString method
    */
  def toStringNoPropagate: String = name + ":={" + m_NewValue.foldLeft("")((acc, intval) =>
    if (acc.equalsIgnoreCase("")) "" + intval else acc + "," + intval
  ) + "}"

  // mechanism that manage key with value changes
  private val listeningElementsNonValueWise: DoublyLinkedList[(PropagationElement, Int)] =
    getDynamicallyListeningElements.permaFilter({ case (pe, id) => id != Int.MinValue })

  override protected[core] def registerDynamicallyListeningElementNoKey(
    listening: PropagationElement,
    i: Int
  ): Unit = {
    super.registerDynamicallyListeningElementNoKey(listening, i)
  }

  def instrumentKeyToValueWiseKey(key: KeyForElementRemoval): ValueWiseKey = {
    createValueWiseMechanicsIfNeeded()
    new ValueWiseKey(key, this, null)
  }

  /** The values that have bee impacted since last propagation was performed. null if set was
    * assigned
    */
  private[this] var addedValues: QList[Int]   = null
  private[this] var removedValues: QList[Int] = null
  private[this] var nbTouched: Int            = 0

  protected def insertValue(v: Int): Unit = {
    if (!m_NewValue.contains(v)) insertValueNotPreviouslyIn(v)
  }

  protected def insertValueNotPreviouslyIn(v: Int): Unit = {
    if (nbTouched != -1) {
      addedValues = QList(v, addedValues)
      nbTouched += 1
      if (nbTouched > domainSizeDiv10) nbTouched = -1
    }
    m_NewValue += v
    notifyChanged()
  }

  protected def deleteValue(v: Int): Unit = {
    if (m_NewValue.contains(v)) deleteValuePreviouslyIn(v)
  }

  protected def deleteValues(values: Iterable[Int]): Unit = {
    values.foreach(deleteValue)
  }

  protected def insertValues(values: Iterable[Int]): Unit = {
    values.foreach(insertValue)
  }

  protected def deleteValuePreviouslyIn(v: Int): Unit = {
    if (nbTouched != -1) {
      removedValues = QList(v, removedValues)
      nbTouched += 1
      if (nbTouched > domainSizeDiv10) nbTouched = -1
    }
    m_NewValue -= v
    notifyChanged()
  }

  /** We suppose that the new value is not the same as the actual value. otherwise, there is a huge
    * waste of time.
    *
    * @param v
    *   the new value to set to the variable
    */
  protected def setValue(v: SortedSet[Int]): Unit = {
    removedValues = null
    addedValues = null
    nbTouched = -1
    m_NewValue = v
    notifyChanged()
  }

  private def createValueWiseMechanicsIfNeeded(): Unit = {
    if (valueToValueWiseKeys == null) {
      require(
        Int.MinValue <= domain.min,
        "when using valueWise mechanism, the domain of sets should have a min >= Int.MinValue"
      )

      offsetForValueWiseKey = domain.min.toInt
      val nbValues = this.domain.max.toInt - this.domain.min.toInt + 1
      require(
        Int.MinValue <= nbValues,
        "when using valueWise mechanism, the domain of sets should be reasonably small"
      )

      valueToValueWiseKeys = Array.tabulate(nbValues)(_ => new DoublyLinkedList[ValueWiseKey]())
    }
  }

  private[this] var valueToValueWiseKeys: Array[DoublyLinkedList[ValueWiseKey]] = null
  private[this] var offsetForValueWiseKey: Int                                  = Int.MaxValue

  @inline
  final def addToValueWiseKeys(key: ValueWiseKey, value: Int): DLLStorageElement[ValueWiseKey] = {
    valueToValueWiseKeys(value - offsetForValueWiseKey).addElem(key)
  }

  @inline
  private def valueWiseKeysAtValue(value: Int): DoublyLinkedList[ValueWiseKey] =
    valueToValueWiseKeys(value - offsetForValueWiseKey)

  override def performPropagation(): Unit = {
    performSetPropagation()
  }

  @inline
  final protected def performSetPropagation(): Unit = {
    if (getDynamicallyListeningElements.isEmpty) {
      // no need to do it gradually
      OldValue = m_NewValue
    } else {
      val (addedValues, deletedValues): (Iterable[Int], Iterable[Int]) = if (nbTouched == -1) {
        // need to call every listening one, so gradual approach required
        if (m_NewValue == OldValue) (List.empty, List.empty)
        else (m_NewValue.diff(OldValue), OldValue.diff(m_NewValue))

      } else {
        // TODO: this is slow, and it delives TreeSet. TreeSet are slow fo valueWise notifications
        // we have the set of values that have been touched (added or deleted)
        // but we need to check for each opf them if they have been both added and deleted
        var addedUnique   = SortedSet.empty[Int] ++ this.addedValues
        var removedUnique = SortedSet.empty[Int] ++ this.removedValues
        for (inter <- addedUnique.intersect(removedUnique)) {
          val inNew = m_NewValue.contains(inter)
          val inOld = OldValue.contains(inter)
          if (!inOld || inNew) {
            removedUnique = removedUnique - inter
          }
          if (!inNew || inOld) {
            addedUnique = addedUnique - inter
          }
        }
        (addedUnique, removedUnique)
      }

      assert((OldValue ++ addedValues -- deletedValues).equals(m_NewValue))

      if (addedValues.nonEmpty || deletedValues.nonEmpty) {
        // notifying the PE that listen to the whole set
        val dynListElements = listeningElementsNonValueWise
        val headPhantom     = dynListElements.phantom
        var currentElement  = headPhantom.next
        while (currentElement != headPhantom) {
          val e                          = currentElement.elem
          val inv: SetNotificationTarget = e._1.asInstanceOf[SetNotificationTarget]
          assert({
            this.model.notifiedInvariant = inv.asInstanceOf[Invariant];
            true
          })
          inv.notifySetChanges(this, e._2, addedValues, deletedValues, OldValue, m_NewValue)
          assert({
            this.model.notifiedInvariant = null;
            true
          })
          // we go to the next to be robust against invariant that change their dependencies when notified
          // this might cause crash because dynamicallyListenedInvariants is a mutable data structure
          currentElement = currentElement.next
        }

        if (valueToValueWiseKeys != null) {
          val currentValueWisePropagationWaveIdentifier = new ValueWisePropagationWaveIdentifier()

          notifyForValues(
            addedValues,
            addedValues,
            deletedValues,
            currentValueWisePropagationWaveIdentifier
          )
          notifyForValues(
            deletedValues,
            addedValues,
            deletedValues,
            currentValueWisePropagationWaveIdentifier
          )
        }

      }
      // puis, on fait une affectation en plus, pour garbage collecter l'ancienne structure de donnees.
      OldValue = m_NewValue
    }
    this.addedValues = null
    this.removedValues = null
    nbTouched = 0
  }

  @inline // This method is awfully slow, ad we do not know why
  private def notifyForValues(
    values: Iterable[Int],
    addedValues: Iterable[Int],
    deletedValues: Iterable[Int],
    currentValueWisePropagationWaveIdentifier: ValueWisePropagationWaveIdentifier
  ): Unit = {
    val valuesIt = values.iterator
    while (valuesIt.hasNext) {
      val value                                           = valuesIt.next()
      val valueWiseKeys                                   = valueWiseKeysAtValue(value)
      val headPhantom                                     = valueWiseKeys.phantom
      var currentElement: DLLStorageElement[ValueWiseKey] = headPhantom.next
      while (currentElement != headPhantom) {
        val e: ValueWiseKey = currentElement.elem
        if (
          e.currentValueWisePropagationWaveIdentifier != currentValueWisePropagationWaveIdentifier
        ) {
          e.currentValueWisePropagationWaveIdentifier = currentValueWisePropagationWaveIdentifier
          val target = e.target
          assert({
            this.model.notifiedInvariant = target.asInstanceOf[Invariant]
            true
          })
          target.notifySetChanges(
            this,
            Int.MinValue,
            addedValues,
            deletedValues,
            OldValue,
            m_NewValue
          )
          assert({
            this.model.notifiedInvariant = null
            true
          })
        }
        // we go to the next to be robust against invariant that change their dependencies when notified
        // this might cause crash because dynamicallyListenedInvariants is a mutable data structure
        currentElement = currentElement.next
      }
    }
  }

  def value: SortedSet[Int] = getValue(false)

  protected def newValue: SortedSet[Int] = getValue(true)

  private def getValue(NewValue: Boolean = false): SortedSet[Int] = {
    if (NewValue) {
      assert(
        model.checkExecutingInvariantOK(definingInvariant),
        s"variable [$this] queried for latest val by non-controlling invariant"
      )
      m_NewValue
    } else {
      if (model == null) return m_NewValue
      // if (definingInvariant == null && !model.propagating) return Value
      model.propagate(this)
      OldValue
    }
  }

  /** We suppose that the new value is not the same as the actual value. otherwise, there is a huge
    * waste of time.
    *
    * @param v
    *   the new value to set to the variable
    */
  protected def :=(v: SortedSet[Int]): Unit = {
    setValue(v)
  }

  protected def :+=(i: Int): Unit = {
    this.insertValue(i)
  }

  protected def :-=(i: Int): Unit = {
    this.deleteValue(i)
  }

  def createClone: CBLSSetVar = {
    val clone = new CBLSSetVar(model, this.value, this.domain, s"clone of ${this.name}")
    clone <== this
    clone
  }

  override def checkInternals(c: Checker): Unit = {
    assert(
      this.definingInvariant == null || OldValue.intersect(m_NewValue).size == m_NewValue.size,
      s"internal error: Value: $m_NewValue OldValue: $OldValue"
    )
  }
}

object ChangingSetValue {
  implicit val ord: Ordering[ChangingSetValue] = (o1: ChangingSetValue, o2: ChangingSetValue) =>
    o1.compare(o2)
}
