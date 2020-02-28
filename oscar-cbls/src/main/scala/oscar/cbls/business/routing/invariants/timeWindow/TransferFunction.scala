package oscar.cbls.business.routing.invariants.timeWindow

object TransferFunction{
  /**
   * Conditions :
   *    "I must perform a task at this location that lasts taskDuration unit of time."
   *    "I can't start this task sooner than earliestArrivalTime."
   *    "I'll leave this location at earliestArrivalTime + taskDuration at earliest"
   * @param node The node representing the location
   * @param earliestArrivalTime The earliest start of this location's task
   * @param taskDuration The duration of this location's task
   */
  def createFromEarliestArrivalTime(node: Long, earliestArrivalTime: Long, taskDuration: Long = 0L): TransferFunction ={
    val latestArrivalTime = Long.MaxValue - taskDuration
    val earliestLeavingTime = earliestArrivalTime+taskDuration
    DefinedTransferFunction(earliestArrivalTime, latestArrivalTime, earliestLeavingTime, node, node)
  }

  /**
   * Conditions :
   *    "I must perform a task at this location that lasts taskDuration unit of time."
   *    "I can't start this task later than latestArrivalTime."
   *    "I'll leave this location at latestArrivalTime + taskDuration at latest"
   * @param node The node representing the location
   * @param latestArrivalTime The latest start of this location's task
   * @param taskDuration The duration of this location's task
   */
  def createFromLatestArrivalTime(node: Long, latestArrivalTime: Long, taskDuration: Long = 0L): TransferFunction ={
    val earliestArrivalTime = 0
    val earliestLeavingTime = taskDuration
    DefinedTransferFunction(earliestArrivalTime, latestArrivalTime, earliestLeavingTime, node, node)
  }

  /**
   * Conditions :
   *    "I must perform a task at this location that lasts taskDuration unit of time."
   *    "I can't start this task sooner than earliestArrivalTime."
   *    "I can't start this task later than latestArrivalTime."
   *    "I'll leave this location at earliestArrivalTime + taskDuration at earliest"
   *    "I'll leave this location at latestArrivalTime + taskDuration at latest"
   * @param node The node representing the location
   * @param earliestArrivalTime The earliest start of this location's task
   * @param latestArrivalTime The latest start of this location's task
   * @param taskDuration The duration of this location's task
   */
  def createFromEarliestAndLatestArrivalTime(node: Long, earliestArrivalTime: Long,  latestArrivalTime: Long, taskDuration: Long = 0L): TransferFunction ={
    val earliestLeavingTime = earliestArrivalTime + taskDuration
    DefinedTransferFunction(earliestArrivalTime, latestArrivalTime, earliestLeavingTime, node, node)
  }

  /**
   * Computes the relevant predecessors of each node.
   * A neighbor is a relevant predecessors of a node if :
   *      - The earliest leaving time of the neighbor + the travel duration to the node
   *      is lesser than the latest arrival time of the node. Otherwise we will arrive too late at the node.
   * @param n The number of nodes of the problem
   * @param v The number of vehicles of the problem
   * @param singleNodesTransferFunctions The array containing the TransferFunction of each nodes of the problem
   * @param timeMatrix The matrix containing the travel duration between each nodes of the problem
   * @return A map (node -> relevant neighbors)
   */
  def relevantPredecessorsOfNodes(n: Int, v: Int, singleNodesTransferFunctions: Array[TransferFunction], timeMatrix: Array[Array[Long]]): Map[Long,Iterable[Long]] ={
    val allNodes = (0L until n).toList
    List.tabulate(n)(node => node.toLong -> allNodes.collect({
      case neighbor: Long if node != neighbor && singleNodesTransferFunctions(neighbor.toInt).el + timeMatrix(neighbor.toInt)(node) <= singleNodesTransferFunctions(node).la => neighbor
    })
    ).toMap
  }

  /**
   * This method is meant to precompute the relevant successors of all node.
   *
   * A node z is an relevant successor of another node y if
   *   earliestArrivalTimes(y) +
   *   taskDurations(y) +
   *   timeMatrix.getTravelDuration(y, earliestArrivalTimes(y) + taskDurations(y), z) +
   *   taskDurations(z) <= latestLeavingTimes(z)
   *
   * e.g : You have a list of relevant predecessor for a node y but inserting this node y
   * could delayed the arrival time to the current next node of the relevant predecessor.
   *
   *
   * Meaning if we can start the task at node y, finish it, travel to z,
   * and finish the task at z before the latestLeavingTimes of z, then x is an open relevant neighbor of y.
   *
   * All these informations are used to define the problem, therefore they are static.
   * The only information that's not static is the current next of the relevant predecessor we want to insert after.
   * But we can precompute all the relevant successor of the node we want to insert
   *
   * @param n The number of nodes of the problem
   * @param v The number of vehicles of the problem
   * @param singleNodesTransferFunctions The array containing the TransferFunction of each nodes of the problem
   * @param timeMatrix The matrix containing the travel duration between each nodes of the problem
   * @return A map (node -> relevant neighbors)
   */
  def relevantSuccessorsOfNodes(n: Int, v: Int, singleNodesTransferFunctions: Array[TransferFunction], timeMatrix: Array[Array[Long]]): Map[Long,Iterable[Long]] ={
    val allNodes = (0L until n).toList
    List.tabulate(n)(to => {
      val toTF = singleNodesTransferFunctions(to)
      to.toLong -> allNodes.collect{
        case from: Long if from != to && singleNodesTransferFunctions(from.toInt).latestLeavingTime + timeMatrix(from.toInt)(to) <= toTF.la => from
      }}).toMap
  }
}

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

  lazy val latestLeavingTime: Long = la + el - ea

  lazy val taskDuration: Long = el - ea

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

case class TwoWaysTransferFunction(nonFlippedTF: TransferFunction, flippedTF: TransferFunction){
  def from(flipped: Boolean): Long ={
    if(flipped)flippedTF.from
    else nonFlippedTF.from
  }

  def to(flipped: Boolean): Long ={
    if(flipped)flippedTF.to
    else nonFlippedTF.to
  }

  def apply(t: Long, flipped: Boolean): Long ={
    if(flipped) flippedTF(t)
    else nonFlippedTF(t)
  }

  def isEmpty(flipped: Boolean): Boolean ={
    if(flipped) flippedTF.isEmpty
    else nonFlippedTF.isEmpty
  }

  override def toString: String = {
    "Two ways transfert function : \nNon-flipped : " + nonFlippedTF.toString + "\nFlipped : " + flippedTF.toString
  }
}