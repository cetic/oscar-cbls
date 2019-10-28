package oscar.cbls.business.routing.modeling

import oscar.cbls.business.routing._
import oscar.cbls.business.routing.model.extensions._
import oscar.cbls.visual.routing.RoutingMapTypes

import scala.collection.immutable.List

/**
  * Created by fg on 9L/10L/1L7.
  */
trait RoutingExtensions {

  /**
    * This class is used to represent the concept of chains.
    * A chain is a list of nodes within which each node is the predecessor of the next node in the list.
    * This class offers a list of methods and values usefull if you want to implement custom methods for your routing problem.
    *
    * @param vrp The basic vehicle routing problem
    * @param chains The list of chains => List of List of nodes
    * @return A Chains object
    */
  def chains(vrp: VRP, chains: List[List[Long]]) =
    new Chains(vrp, chains)
  type Chains = oscar.cbls.business.routing.model.extensions.Chains

  /**
    * This class is used to display your routing problem on a map.
    * Your routing problem can be displayed on a real map or a simple map
    * (blank frame with dots at there respective position)
    *
    * For an ultimate refresh add this at the end of your search procedure : afterMove(myDisplayObject.drawRoutes())
    *
    * @param vrp The basic vehicle routing problem
    * @param nodePositions A list of node's position. nodePosition(0L) represent the position of the first node
    * @param sizeOfMap The size of your map
    * @param refreshRate The refresh rate (be carefull if the refresh rate is to high you may have greate performance issues
    * @param routingMapType The type of map you want to generate
    * @param toolTipInfo A function node => String used to display some information concerning the node.
    *                    WARNING : Using html syntax => breakline = < br >
    *                    NOTE : Basic information is already given :
    *                        "Node " + node + " at the " + position + "th position of the vehicle " + vehicle + "< br >"
    * @return A display object
    */
  def display(vrp: VRP,
              nodePositions: Array[(Double,Double)],
              sizeOfMap: Option[Long] = None,
              refreshRate: Long = 100L,
              routingMapType: RoutingMapTypes.Value = RoutingMapTypes.BasicRoutingMap,
              toolTipInfo: Option[Int => Option[() => String]] = None,
              title:String = "VRP with OscaR.cbls"
             ) =
    new Display(vrp,nodePositions,sizeOfMap,refreshRate,toolTipInfo, routingMapType,title)
  type Display = oscar.cbls.business.routing.model.extensions.Display


  /**
    * This class is only used to simplify the constraints creation.
    * The invariant used to set the time constraint is not a time dedicated invariant
    * so we need some extra information like earliestArrivalTimes, latestLeavingTimes...
    *
    * This class serve as a data package
    *
    * @param earliestArrivalTimes An array that contains the earliest arrival time of each node. If arriving before this value we must wait.
    * @param latestArrivalTimes An array that contains the latest arrival time of each node. We can't start the task after this time.
    * @param earliestLeavingTimes An array that contains the earliest leaving time of each node.
    * @param latestLeavingTimes An array that contains the latest leaving time of each node.
    * @param taskDurations An array that contains the task duration of each node.
    * @param maxWaitingDurations For each node the maximum among of time we can wait before starting the task.
    *                            e.g.: You can stay at a parking for a limited among of time.
    */
  def timeWindows(earliestArrivalTimes: Option[Array[Long]] = None,
                     latestArrivalTimes: Option[Array[Long]] = None,
                     earliestLeavingTimes: Option[Array[Long]] = None,
                     latestLeavingTimes: Option[Array[Long]] = None,
                     taskDurations: Array[Long],
                     maxWaitingDurations: Option[Array[Long]] = None): TimeWindows =
    TimeWindows(earliestArrivalTimes,latestArrivalTimes,earliestLeavingTimes,latestLeavingTimes,taskDurations,maxWaitingDurations)
  type TimeWindow = oscar.cbls.business.routing.model.extensions.TimeWindows

}
