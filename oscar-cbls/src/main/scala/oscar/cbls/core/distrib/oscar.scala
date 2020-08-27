package oscar.cbls.core.distrib


// ////////////////////////////////////////////////////////////

/*
This might be useful when going for distributed search
object Acceptation{
  def reduce:Reduce = new Reduce()
  def lowerThan(v:Long) = new LowerThan(v:Long)
  def lowerThanMetropolis(v:Long) = new LowerThanMetropolis(v:Long)
}

abstract class Acceptation{
  def isAccepted(before:Long,after:Long):Boolean
}

case class Reduce() extends Acceptation{
  override def isAccepted(before: Long, after: Long): Boolean = after < before
}

case class LowerThan(v:Long) extends Acceptation{
  override def isAccepted(before: Long, after: Long): Boolean = after < v
}

case class LowerThanMetropolis(v:Long) extends Acceptation{
  override def isAccepted(before: Long, after: Long): Boolean = after < v
}
*/
