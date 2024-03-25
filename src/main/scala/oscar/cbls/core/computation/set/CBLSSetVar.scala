package oscar.cbls.core.computation.set

/** An IntSetVar is a variable managed by the [[oscar.cbls.core.computation.Store]] whose type is
  * set of integer.
  *
  * @param givenModel
  *   is the model in s-which the variable is declared, can be null if the variable is actually a
  *   constant, see [[CBLSSetConst]]
  * @param initialDomain
  *   is the domain value of the variable. Some invariants exploit this value to declare fixed size
  *   arrays
  * @param initialValue
  *   is the initial value of the variable
  * @param n
  *   is the name of the variable, used for pretty printing only. if not set, a default will be
  *   used, based on the variable number
  */
class CBLSSetVar(
  givenModel: Store,
  initialValue: SortedSet[Int],
  initialDomain: Domain,
  n: String = null
) extends ChangingSetValue(initialValue, initialDomain)
    with Variable {

  require(givenModel != null)

  model = givenModel

  override def restrictDomain(d: Domain): Unit = super.restrictDomain(d)

  override def createCheckpoint: VariableCheckpoint = new CBLSetVarCheckpoint(this, this.value)

  override def name: String = if (n == null) defaultName else n

  override def :=(v: SortedSet[Int]): Unit = {
    setValue(v)
  }

  override def :+=(i: Int): Unit = {
    this.insertValue(i)
  }

  override def :-=(i: Int): Unit = {
    this.deleteValue(i)
  }

  def <==(i: SetValue): Unit = {
    IdentitySet(this, i)
  }

  override def insertValue(v: Int): Unit = super.insertValue(v)

  override def insertValueNotPreviouslyIn(v: Int): Unit = super.insertValueNotPreviouslyIn(v)

  override def deleteValue(v: Int): Unit = super.deleteValue(v)

  override def deleteValues(values: Iterable[Int]): Unit = super.deleteValues(values)

  override def insertValues(values: Iterable[Int]): Unit = super.insertValues(values)

  override def deleteValuePreviouslyIn(v: Int): Unit = super.deleteValuePreviouslyIn(v)

  /** We suppose that the new value is not the same as the actual value. otherwise, there is a huge
    * waste of time.
    *
    * @param v
    *   the new value to set to the variable
    */
  override def setValue(v: SortedSet[Int]): Unit = super.setValue(v)

  override def value: SortedSet[Int] = super.value
}

object CBLSSetVar {
  // this conversion is forbidden because we inserted the new grammar.
  // implicit def toIntSet(v:IntSetVar):SortedSet[Int] = v.value

  def apply(
    s: Store,
    v: Iterable[Int] = List.empty,
    d: Domain = FullIntRange,
    name: String = ""
  ) = {
    val emptySet: SortedSet[Int] = SortedSet.empty
    new CBLSSetVar(s, emptySet ++ v, d, name)
  }

  implicit val ord: Ordering[CBLSSetVar] = (o1: CBLSSetVar, o2: CBLSSetVar) => o1.compare(o2)
}
