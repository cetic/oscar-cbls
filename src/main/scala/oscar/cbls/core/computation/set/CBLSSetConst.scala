//package oscar.cbls.core.computation.set
//
///** An IntSetConst is an IntSetVar that has a constant value, defined by a set of integer. It has no
//  * associated model, as there is no need to incorporate it into any propagation process.
//  *
//  * @param value
//  *   : the value of the constant
//  * @author
//  *   renaud.delandtsheer@cetic.be
//  */
//class CBLSSetConst(override val value: SortedSet[Int]) extends SetValue {
//  override def toString: String = "Set{" + value.mkString(",") + "}"
//
//  override def domain: Domain = DomainRange(value.min, value.max)
//
//  override val min: Int = if (value.isEmpty) Int.MaxValue else value.min
//  override val max: Int = if (value.isEmpty) Int.MinValue else value.max
//
//  override def name: String = toString
//}
//
//object CBLSSetConst {
//  def apply(value: SortedSet[Int]) = new CBLSSetConst(value)
//}
