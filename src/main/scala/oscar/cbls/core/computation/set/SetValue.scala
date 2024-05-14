//package oscar.cbls.core.computation.set
//
//sealed trait SetValue extends Value {
//  def value: SortedSet[Int]
//
//  def domain: Domain
//
//  def min: Int = domain.min.toInt
//
//  def max: Int = domain.max.toInt
//
//  def name: String
//
//  override final def valueString: String = "{" + value.mkString(",") + "}"
//}
//
//object SetValue {
//  implicit def intSet2IntSetVar(a: SortedSet[Int]): SetValue = CBLSSetConst(a)
//
//  implicit def toFunction(i: SetValue): () => SortedSet[Int] = () => i.value
//}
