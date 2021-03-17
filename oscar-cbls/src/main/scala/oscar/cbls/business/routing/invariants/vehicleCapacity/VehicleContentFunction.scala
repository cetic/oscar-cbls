package oscar.cbls.business.routing.invariants.vehicleCapacity

protected abstract class VehicleContentFunction(val maxContentIfStartAt0: Long,
                                                val minContentIfStartAt0: Long,
                                                val contentAtEndIfStartAt0: Long,
                                                val from: Int, val to: Int){
  def isEmpty: Boolean
  def max(startContent: Long): Long
  def min(startContent: Long): Long
}

protected case class DefinedContentFunction(override val maxContentIfStartAt0: Long,
                                            override val minContentIfStartAt0: Long,
                                            override val contentAtEndIfStartAt0: Long,
                                            override val from: Int,
                                            override val to: Int) extends
  VehicleContentFunction(maxContentIfStartAt0, minContentIfStartAt0, contentAtEndIfStartAt0, from, to) {
  override def isEmpty: Boolean = false

  override def toString: String =
    s"""From : $from, To = $to
       |Vehicle content at end : $contentAtEndIfStartAt0
       |Max if start content is zero : $maxContentIfStartAt0
       |Min if start content is zero : $minContentIfStartAt0""".stripMargin

  override def max(startContent: Long): Long = {
    maxContentIfStartAt0 + startContent
  }

  override def min(startContent: Long): Long = {
    minContentIfStartAt0 + startContent
  }
}

protected case object EmptyContentFunction extends
  VehicleContentFunction(-1, -1, -1, -1, -1){
  override def isEmpty: Boolean = true

  override def toString: String = "Empty vehicle content"

  override def max(startContent: Long): Long = Long.MaxValue

  override def min(startContent: Long): Long = Long.MinValue
}

protected case class TwoWaysVehicleContentFunction(nonFlippedFunction: VehicleContentFunction, flippedFunction: VehicleContentFunction){

  def from(flipped: Boolean): Int =
    if(flipped)flippedFunction.from
    else nonFlippedFunction.from

  def to(flipped: Boolean): Int =
    if(flipped)flippedFunction.to
    else nonFlippedFunction.to

  def contentAtEndIfStartAt0(flipped: Boolean): Long =
    if(flipped) flippedFunction.contentAtEndIfStartAt0
    else nonFlippedFunction.contentAtEndIfStartAt0

  def apply(startContent: Long, maxVehicleContent: Long, flipped: Boolean): Boolean ={
    val vehicleContentFunction = if(flipped)flippedFunction else nonFlippedFunction
    vehicleContentFunction.max(startContent) > maxVehicleContent || vehicleContentFunction.min(startContent) < 0
  }

  def isEmpty(flipped: Boolean): Boolean =
    if(flipped)flippedFunction.isEmpty
    else nonFlippedFunction.isEmpty

  override def toString: String = {
    s"""Two ways vehicle content function :
       |Non-flipped : $nonFlippedFunction
       |Flipped : $flippedFunction
       |""".stripMargin
  }
}
