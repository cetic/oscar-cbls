package oscar.cbls.business.routing.invariants.vehicleCapacity

protected abstract class VehicleMultipleContentFunction(val size: Int,
                                                        val maxContentsIfStartAt0: Array[Long], 
                                                        val minContentsIfStartAt0: Array[Long],
                                                        val contentsAtEndIfStartAt0: Array[Long], 
                                                        val from: Int, val to: Int) {
  def isEmpty: Boolean

  def max(startContent: Array[Long]): Array[Long]

  def min(startContent: Array[Long]): Array[Long]
}

protected case class DefinedMultipleContentFunction(override val size: Int,
                                                    override val maxContentsIfStartAt0: Array[Long], 
                                                    override val minContentsIfStartAt0: Array[Long], 
                                                    override val contentsAtEndIfStartAt0: Array[Long], 
                                                    override val from: Int, 
                                                    override val to: Int) extends
  VehicleMultipleContentFunction(size, maxContentsIfStartAt0, minContentsIfStartAt0, contentsAtEndIfStartAt0, from, to) {
  override def isEmpty: Boolean = false

  override def toString: String =
    s"""From : $from, To = $to
       |Vehicle content at end : ${contentsAtEndIfStartAt0.toList}
       |Max if start content is zero : ${maxContentsIfStartAt0.toList}
       |Min if start content is zero : ${minContentsIfStartAt0.toList}""".stripMargin

  override def max(startContent: Array[Long]): Array[Long] = {
    Array.tabulate(size)(i => maxContentsIfStartAt0(i) + startContent(i))
  }

  override def min(startContent: Array[Long]): Array[Long] = {
    Array.tabulate(size)(i => minContentsIfStartAt0(i) + startContent(i))
  }
}

protected case class EmptyMultipleContentFunction(override val size: Int) extends
  VehicleMultipleContentFunction(size, Array.fill(size)(-1), Array.fill(size)(-1), Array.fill(size)(-1), -1, -1){
  override def isEmpty: Boolean = true

  override def toString: String = "Empty vehicle content"

  override def max(startContent: Array[Long]): Array[Long] = Array.fill(size)(Long.MaxValue)

  override def min(startContent: Array[Long]): Array[Long] = Array.fill(size)(Long.MinValue)
}

protected case class TwoWaysVehicleMultipleContentFunction(nonFlippedFunction: VehicleMultipleContentFunction, flippedFunction: VehicleMultipleContentFunction){

  def from(flipped: Boolean): Int =
    if(flipped)flippedFunction.from
    else nonFlippedFunction.from

  def to(flipped: Boolean): Int =
    if(flipped)flippedFunction.to
    else nonFlippedFunction.to

  def contentsAtEndIfStartAt0(flipped: Boolean): Array[Long] =
    if(flipped) flippedFunction.contentsAtEndIfStartAt0
    else nonFlippedFunction.contentsAtEndIfStartAt0

  def apply(startContents: Array[Long], maxVehicleContents: Array[Long], flipped: Boolean): Boolean ={
    val vehicleMultipleContentFunction = if(flipped)flippedFunction else nonFlippedFunction
    val maxContentsAtStartContents = vehicleMultipleContentFunction.max(startContents)
    val minContentsAtStartContents = vehicleMultipleContentFunction.min(startContents)
    (0 until vehicleMultipleContentFunction.size).
      exists(i => maxContentsAtStartContents(i) > maxVehicleContents(i) || minContentsAtStartContents(i) < 0)

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
