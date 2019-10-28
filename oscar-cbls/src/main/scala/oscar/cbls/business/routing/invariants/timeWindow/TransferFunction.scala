package oscar.cbls.business.routing.invariants.timeWindow

/**
  * This abstract class defines a TransferFunction.
  * The TransferFunction's main purpose is to compute
  * the leaving time at a node or segment's end given
  * the arrival time at the node or the segment's start.
  * It uses three values
  * @param ea the earliest arrival time at the node or segment's start
  * @param la the latest arrival time at the node or segment's start
  * @param el the earliest leaving time from node or segment's end
  */
abstract class TransferFunction(val ea: Long, val la: Long, val el: Long, val from: Long, val to: Long){

  // This method is used to compute the leaving time
  def apply(t: Long): Long

  // If true it means that the TransferFunction isn't defined
  // and that apply() return always None
  def isEmpty: Boolean

  override def toString: String = {
    "earliest arrival time : " + ea + "\n latest arrival time : " + la + "\n earliest leaving time : " + el
  }
}

object DefinedTransferFunction{
  def apply(ea: Long, la: Long, el: Long, from: Long, to: Long): DefinedTransferFunction =
    new DefinedTransferFunction(ea: Long, la: Long, el: Long, from: Long, to: Long)
}

class DefinedTransferFunction(override val ea: Long, override val la: Long, override val el: Long,
                                   override val from: Long, override val to: Long) extends TransferFunction(ea,la,el,from,to){
  require(la >= ea && el >= ea, "earliest arrival time : " + ea + ", latest arrival time : " + la + ", earliest leaving time : " + el)
  override def apply(t: Long): Long = {
    if(t <= ea)
      el
    else if(t <= la)
      t + el - ea
    else
      -1L
  }

  override def isEmpty: Boolean = false

  override def toString: String = {
    "Defined transfert function : \nFrom " + from + "   To " + to + "\n" + super.toString
  }
}

case object EmptyTransferFunction extends TransferFunction(1L,-1L,-1L,-1L,-1L){
  override def apply(t: Long): Long = -1L

  override def isEmpty: Boolean = true

  override def toString: String = "Empty transfert function"
}