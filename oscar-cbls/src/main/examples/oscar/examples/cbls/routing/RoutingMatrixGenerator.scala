package oscar.examples.cbls.routing

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

import oscar.cbls._
import oscar.cbls.business.routing.model.{TTFConst, TTFMatrix}

import scala.util.Random

/**
 * Created by rdl on 23/03/2015.
 */
object RoutingMatrixGenerator {
  val random = new Random(0)

  /**
    * This method generate a random distance matrix based on numbers of node and map side.
    * It also generate an array of node positions. (Usefull when you want to display it on a map)
    * @param n The number of nodes (considering depots)
    * @param side The side of the map
    * @return The distance matrix (Array[Array[Long] ] and the position of each node (Array[(Long,Long)])
    */
  def apply(n: Int, side: Long = 1000): (Array[Array[Long]],Array[(Long,Long)]) = {

    //we generate te cost distance matrix
    def randomXY: Long = (random.nextFloat() * side).toLong
    val pointPosition: Array[(Long, Long)] = Array.tabulate(n)(w => (randomXY, randomXY))

    def distance(from: (Long, Long), to: (Long, Long)) =
      math.sqrt(math.pow(from._1 - to._1, 2) + math.pow(from._2 - to._2, 2)).toLong

    //for each delivery point, the distance to each warehouse
    (Array.tabulate(n)(
      n1 => Array.tabulate(n)(
        n2 => distance(pointPosition(n1), pointPosition(n2)))),pointPosition)
  }


  /**
    * This method generate a random distance matrix based on numbers of node and map side.
    * It also generate an array of node positions. (Usefull when you want to display it on a map)
    * @param n The number of nodes (considering depots)
    * @return The distance matrix (Array[Array[Float] ] in meters and the position of each node (Array[(Double,Double)])
    */
  def geographicRandom(n: Int,minLong:Double,maxLong:Double,minLat:Double,maxLat:Double): (Array[Array[Double]],Array[(Double,Double)]) = {

    //we generate te cost distance matrix
    val deltaLat = maxLat - minLat
    val deltaLong = maxLong - minLong

    def randomLat: Double = (random.nextDouble() * deltaLat) + minLat
    def randomLong: Double = (random.nextDouble() * deltaLong) + minLong

    def distance(coord1:(Double,Double),coord2:(Double,Double)):Double = {
      val (lat1:Double,lon1:Double) = coord1
      val (lat2:Double,lon2:Double) = coord2

      val R = 6371e3 // metres

      val φ1 = lat1.toRadians
      val φ2 = lat2.toRadians
      val Δφ = (lat2 - lat1).toRadians
      val Δλ = (lon2 - lon1).toRadians

      val a = Math.sin(Δφ / 2) * Math.sin(Δφ / 2) + Math.cos(φ1) * Math.cos(φ2) * Math.sin(Δλ / 2) * Math.sin(Δλ / 2)
      val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))

      (R * c).toDouble //meters
    }

    val pointPosition: Array[(Double,Double)] = Array.fill(n)((randomLat, randomLong))

    //for each delivery point, the distance to each warehouse
    (Array.tabulate(n)(
      n1 => Array.tabulate(n)(
        n2 => distance(pointPosition(n1), pointPosition(n2)))),pointPosition)
  }


  /**
    * This method generate random restrictions for the problem.
    * A restriction is a tuple of a vehicle and a node.
    * This means that if the specified node is routed, it must be on the route of the specified vehicle
    * @param n The number of nodes (considering depots)
    * @param v The number of vehciles/depots
    * @param nbRestrictions The number of restrictions we want to generate
    * @return An iterable of tuple (node, vehicle)
    */
  def generateRestrictions(n: Int,v: Int,nbRestrictions:Long):Iterable[(Long,Long)] = {
    var toReturn = List.empty[(Long,Long)]

    var toGenerate = nbRestrictions
    while(toGenerate !=0){
      val vehicle = (random.nextFloat()*v).toLong
      val node = ((random.nextFloat()*(n-v))+v).toLong
      toReturn = (node,vehicle) :: toReturn
      toGenerate -= 1
    }
    toReturn
  }

  /**
    * Generate random precedences of size Max(2,random.nextInt(MaxSize))
    *
    * ex :  if length == 2 => (5,6)
    *       if length == 3 => (5,6,7) => (5,6) && (6,7)
    *       if length == 4 => (5,6,7,8) => (5,6) && (6,7) && (7,8)
    *
    * @param n The number of nodes (considering depots)
    * @param v The number of vehicles/depots
    * @param nbPRecedences The number wanted precedences
    * @param maxSize The max length of precedence you want to generate
    * @return A list of tuple (precedences)
    */
  def generateChainsPrecedence(n: Int, v: Int, nbPRecedences:Long, maxSize: Long = 2): (List[List[Long]], List[(Long,Long)]) = {
    val allNodes = (v until n).toList.map(_.toLong)
    val randomizedNodes = random.shuffle(allNodes).toIterator

    var currentMaxSize = maxSize
    var precedencesToGenerate = nbPRecedences

    var chains: List[List[Long]] = List.empty
    var tuples: List[(Long,Long)] = List.empty
    var usedNodes = 0L

    def reduceCurrentSizeBy = Math.max(currentMaxSize - maxSize, (currentMaxSize-2)/2 - (usedNodes/precedencesToGenerate))
    def randomSize = Math.max(2L,1L + intToLong(random.nextInt(longToInt(currentMaxSize))))

    while(precedencesToGenerate > 0 && randomizedNodes.nonEmpty){
      currentMaxSize = currentMaxSize - reduceCurrentSizeBy
      precedencesToGenerate -= 1

      val chain: List[Long] = List.tabulate(Math.min(Math.min(n-v-usedNodes, maxSize), randomSize))(_ => {
        usedNodes += 1
        randomizedNodes.next()
      })

      def toTuple(chain: List[Long], tuples: List[(Long,Long)]): List[(Long,Long)] ={
        chain match {
          case head :: Nil => tuples
          case head :: tail => toTuple(tail, (head, tail.head) :: tuples)
        }
      }
      chains = chain :: chains
      tuples = toTuple(chain, List.empty) ::: tuples
    }

    (chains,tuples)
  }

  /**
    * This method creates a List(node) for each node that doesn't belong to any chain.
    * @param n The number of node (considering depot)
    * @param chains The chains of the problem
    * @return An updated chains list (with lonely nodes)
    */
  private def addLonelyNodesToChains(n: Int, v: Int, chains: List[List[Long]]): List[List[Long]]={
    val allNodes: List[Long] = Range(v,n).toList.map(_.toLong)
    val lonelyNodes = allNodes.diff(chains.flatten)
    var precedencesWithLonelyNodes = chains
    for(lonelyNode <- lonelyNodes)
      precedencesWithLonelyNodes = List(lonelyNode) :: precedencesWithLonelyNodes
    precedencesWithLonelyNodes
  }

  /**
    * This method generate a time window for each node such that the resolver can find a solution for the problem.
    *
    * It works like this :
    *   We add each precedence to a randomly selected vehicle.
    *     We randomly generate the earliestArrivalTime, taskDuration and latestLeavingTime for each node of the precedence
    *
    * This way there is always at least one solution for the problem.
    *
    * @param n The number of node of the problem (considering depot)
    * @param v The number of vehicle/depot
    * @param timeMatrix The travel time matrix
    * @param precedences The list of precedences
    * @param maxIdlingTimeInSec The maximum idling time (the time between two precedences)
    * @param maxExtraTravelTimeInSec The maximum extra travel time (time added to earliestArrivalTimes in order to have a more permissive time window)
    * @param maxTaskDurationInSec The maximum duration of a task
    * @return The time window for each node
    */
  def generateFeasibleTimeWindows(n: Int, v: Int,
                                  timeMatrix: TTFMatrix,
                                  precedences: List[List[Long]] = List.empty,
                                  maxIdlingTimeInSec: Long = 1800L,
                                  maxExtraTravelTimeInSec: Long = 900L,
                                  maxTaskDurationInSec: Long = 300L): (Array[Long],Array[Long],Array[Long],Array[Long]) ={

    def randomVehicleSelection = random.nextInt(v)
    def randomIdleTime = intToLong(random.nextInt(longToInt(maxIdlingTimeInSec)))
    def randomExtraTravelTime = intToLong(random.nextInt(longToInt(maxExtraTravelTimeInSec)))
    def randomTaskDuration = intToLong(random.nextInt(longToInt(maxTaskDurationInSec)))

    val precedencesWithLonelyNodes = addLonelyNodesToChains(n, v, precedences)
    val endOfLastActionOfVehicles = Array.fill[Long](v)(0L)
    val lastNodeVisitedOfVehicles = Array.tabulate(v)(x => intToLong(x))
    val extraTravelTimeOfNodes = Array.tabulate(n)(node => if(node < v)0L else randomExtraTravelTime)
    val earlyLines = Array.fill[Long](n)(0L)
    val deadLines = Array.fill[Long](n)(Long.MaxValue)
    val taskDurations = Array.tabulate[Long](n)(node => if(node < v) 0L else randomTaskDuration)
    val maxWaitingDuration = Array.fill[Long](n)(Long.MaxValue)

    /**
      * This method generate a time window for a given node on a given vehicle
      * It's divided in several step :
      *   1°  We add the travel time from the previous node to the current node to the endOfLastActionOfVehicles
      *   2°  We set the earliestArrivalTime of the node
      *   3°  We add the taskDuration of the node to the endOfLastActionOfVehicles
      *   4°  We set the latestLeavingTime of the node by adding a random extraTravelTime and
      *       the random extraTravelTime from the previous node to the endOfLastActionOfVehicles
      *   5°  We update the last node visited by the vehicle
      * @param node
      * @param onVehicle
      */
    def generateTimeWindowForNode(node: Long, onVehicle: Long): Unit ={
      val previousNode = lastNodeVisitedOfVehicles(longToInt(onVehicle))
      val travelFromPreviousNode = timeMatrix.getTravelDuration(previousNode,endOfLastActionOfVehicles(longToInt(onVehicle)),node)
      endOfLastActionOfVehicles(longToInt(onVehicle)) += travelFromPreviousNode

      earlyLines(longToInt(node)) = endOfLastActionOfVehicles(longToInt(onVehicle))
      endOfLastActionOfVehicles(longToInt(onVehicle)) += taskDurations(longToInt(node))
      deadLines(longToInt(node)) = endOfLastActionOfVehicles(longToInt(onVehicle)) + extraTravelTimeOfNodes(longToInt(node)) + extraTravelTimeOfNodes(longToInt(previousNode))
      lastNodeVisitedOfVehicles(longToInt(onVehicle)) = node
    }

    for(precedence <- precedencesWithLonelyNodes){
      val vehicle = randomVehicleSelection
      //Add some idle time
      endOfLastActionOfVehicles(vehicle) += randomIdleTime
      for(node <- precedence){
        generateTimeWindowForNode(node,vehicle)
      }
    }
    (earlyLines,deadLines,taskDurations,maxWaitingDuration)
  }

  /**
    * Based on precedences and earliestArrivalTimes, this methods generate a map of maxTravelDuration.
    * These values, when added to the constraints system ensure that the travel duration between A and B
    * doesn't exceeds a specified value.
    *
    * This generator set the maxTravelDuration's values like this :
    *
    * (travelDuration between A and B)*1.5
    *
    * e.g.: If the travel duration between A and B is 600, then the maxTravelDuration will be 900.
    * It means that the resolver can add a node (C) between A and B only if :
    *   leaveTime(A) + travel duration to C + taskDuration at C + travel duration to B <= 900
    *
    * @param precedences The list of precedences
    * @param earliestArrivalTimes The array of earliestArrivalTimes (We can't start the task at this node before the earliestArrivalTime value)
    * @param travelDurationMatrix The travel time matrix
    * @return A Map[(from,to) -> maxTravelDuration]
    */
  def generateMaxTravelDurations(precedences: List[List[Long]],
                                 earliestArrivalTimes: Array[Long],
                                 travelDurationMatrix: TTFMatrix): Map[List[Long],Long] ={
    def maxTravelDurationOfPrecedence(from: Long, toProceed: List[Long], maxTravelDurations: List[(List[Long],Long)]): List[(List[Long],Long)] ={
      toProceed match{
        case Nil => maxTravelDurations
        case to::Nil => (List(from,to),(travelDurationMatrix.getTravelDuration(from,earliestArrivalTimes(longToInt(from)),to)*1.5).toLong) :: maxTravelDurations
        case to::tail => maxTravelDurationOfPrecedence(to, tail,(List(from,to),(travelDurationMatrix.getTravelDuration(from,earliestArrivalTimes(longToInt(from)),to)*1.5).toLong) :: maxTravelDurations)
      }
    }

    precedences.flatMap(p => maxTravelDurationOfPrecedence(p.head,p.tail,List.empty)).toMap
  }

  /**
    * Base on the distance matrix, this method generate a linear travel time matrix.
    *
    * e.g. :  859 distance units to go from A to B => 859 time units to go from A to B
    * @param n The nb of node in the problem (considering depots)
    * @param distanceMatrix The distance matrix (of size n*n)
    * @return A TTFMatrix object that represent the travel time matrix.
    */
  def generateLinearTravelTimeFunction(n: Int,distanceMatrix: Array[Array[Long]], multiplier: Double = 1.0): TTFMatrix = {
    val ttf = new TTFMatrix(n, new TTFConst(500))
    for (i <- 0L until n)
      for (j <- 0L until n)
        ttf.setTTF(i, j, new TTFConst((distanceMatrix(longToInt(i))(longToInt(j))*multiplier).toInt))
    ttf
  }

  /**
    * This method return a random array of content flow.
    * Meaning when a vehicle arrives at a particular node, we add the value contentFlow(node) to the vehicle content.
    * This value is allways positive for heads of precedence (List[Long]) and always negative for last of precedence.
    * In between it's positive of negative. (random)
    * But the summed value of each precedence (Lisŧ[Long]) is equal to 0.
    *
    * By default, the content flow at depot is equal to zero.
    *
    * e.g.: +4, +2, -3, +5, -4, -4   or   +2, 0, 0, -2
    *
    * @param n The number of node of the problem
    * @param precedences The list of precedences (List[List[Long] ])
    * @param maxVehicleSize The max size of all vehicles in the problem (All the vehicles don't have to have the same size)
    * @return An array of Long that represents the vehicle content evolution when arriving at a given node.
    */
  def generateContentFlow(n: Int, precedences: List[List[Long]], maxVehicleSize: Long): Array[Long] ={
    def randomContent(currentMaxContent: Long) = intToLong(random.nextInt(longToInt(currentMaxContent)))
    def isPickupStep: Boolean = random.nextBoolean()

    val contentFlow = Array.fill(longToInt(n))(0L)
    for(precedence <- precedences){
      var currentPrecedenceContent = 0L
      for (node <- precedence){
        if(node == precedence.head) {
          contentFlow(longToInt(node)) = randomContent(maxVehicleSize)+1
          currentPrecedenceContent += contentFlow(longToInt(node))
        } else if(node == precedence.last) {
          contentFlow(longToInt(node)) = -currentPrecedenceContent
        } else {
          contentFlow(longToInt(node)) =
            if(isPickupStep) randomContent(maxVehicleSize-currentPrecedenceContent)+1
            else -randomContent(currentPrecedenceContent)
          currentPrecedenceContent += contentFlow(longToInt(node))
        }
      }
    }
    contentFlow
  }

  /**
    * This method generate a random array of vehicle size
    *
    * @param v the number of vehicle
    * @param maxVehicleSize the maximum vehicle size
    * @return an array of vehicle size
    */
  def generateVehiclesSize(v: Int, maxVehicleSize: Long, minVehicleSize: Long = 1): Array[Long] ={
    Array.fill(longToInt(v))(Math.max(intToLong(random.nextInt(longToInt(maxVehicleSize)))+1,minVehicleSize))
  }
}

