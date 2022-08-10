package oscar.cbls.algo.generator

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
import oscar.cbls.business.routing.invariants.timeWindow.TransferFunction
import oscar.cbls.business.routing.model.{TTFConst, TTFMatrix}

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
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

    //we generate the cost distance matrix
    def randomXY: Long = (random.nextFloat() * side).toLong
    val pointPosition: Array[(Long, Long)] = Array.tabulate(n)(_ => (randomXY, randomXY))

    def distance(from: (Long, Long), to: (Long, Long)): Long =
      math.sqrt(math.pow(from._1.toDouble - to._1.toDouble, 2.0) + math.pow(from._2.toDouble - to._2.toDouble, 2.0)).toLong

    //for each delivery point, the distance to each warehouse
    (Array.tabulate(n)(
      n1 => Array.tabulate(n)(
        n2 => distance(pointPosition(n1), pointPosition(n2)))),pointPosition)
  }

  /**
    * This method generate a random distance matrix based on numbers of node and map side.
    * It also generate an array of node positions. (Useful when you want to display it on a map)
    * @param n The number of nodes (considering depots)
    * @return The distance matrix (Array[Array[Float] ] in meters and the position of each node (Array[(Double,Double)])
    */
  def geographicRandom(n: Int,
                       minLong: Double,
                       maxLong: Double,
                       minLat: Double,
                       maxLat: Double): (Array[Array[Double]],Array[(Double,Double)]) = {

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

      R * c //meters
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
  def generateRestrictions(n: Int, v: Int, nbRestrictions:Long):Iterable[(Long,Long)] = {
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

  def generatePrecedences(n: Int, v: Int, nbPrecedences: Int): List[(Int, Int)] = {
    val allNodes = v until n

    val randomizedNodes = random.shuffle(allNodes.toList).iterator
    var precedencesToGenerate = nbPrecedences
    var toReturn: List[(Int, Int)] = List.empty

    while (precedencesToGenerate > 0) {
      precedencesToGenerate -= 1
      toReturn = (randomizedNodes.next(), randomizedNodes.next()) :: toReturn
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
    * @param nbPrecedences The number of wanted precedences
    * @param maxSize The max length of precedence you want to generate
    * @return A list of tuple (precedences)
    */
  def generateChainsPrecedence(n: Int, v: Int, nbPrecedences: Int, maxSize: Int = 2): (List[List[Int]], List[(Int, Int)]) = {
    @tailrec
    def toTuple(chain: List[Int], tuples: List[(Int, Int)]): List[(Int, Int)] = {
      chain match {
        case Nil => throw new IllegalArgumentException("Invalid Nil chain")
        case head :: Nil => tuples
        case head :: tail => toTuple(tail, (head, tail.head) :: tuples)
      }
    }

    //////////
    val allNodes = (v until n).toList
    val randomizedNodes = random.shuffle(allNodes).iterator

    var currentMaxSize = maxSize
    var precedencesToGenerate = nbPrecedences

    var chains: List[List[Int]] = List.empty
    var tuples: List[(Int, Int)] = List.empty
    var usedNodes = 0

    def reduceCurrentSizeBy: Int = Math.max(currentMaxSize - maxSize, (currentMaxSize - 2) / 2 - (usedNodes / precedencesToGenerate))

    def randomSize: Int = Math.max(2, 1 + random.nextInt(currentMaxSize))

    while (precedencesToGenerate > 0 && randomizedNodes.nonEmpty) {
      currentMaxSize = currentMaxSize - reduceCurrentSizeBy
      precedencesToGenerate -= 1

      val chain: List[Int] = List.tabulate(Math.min(Math.min(n - v - usedNodes, maxSize), randomSize))(_ => {
        usedNodes += 1
        randomizedNodes.next()
      })

      chains = chain :: chains
      tuples = toTuple(chain, List.empty) ::: tuples
    }

    (chains, tuples)
  }

  /**
    * This method creates a List(node) for each node that doesn't belong to any chain.
    * @param n The number of node (considering depot)
    * @param chains The chains of the problem
    * @return An updated chains list (with lonely nodes)
    */
  private def addLonelyNodesToChains(n: Int, v: Int, chains: List[List[Int]]): List[List[Int]]={
    val allNodes: List[Int] = Range(v,n).toList
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
  def generateFeasibleTransferFunctions(n: Int,
                                        v: Int,
                                        timeMatrix: Array[Array[Long]],
                                        precedences: List[List[Int]] = List.empty,
                                        maxIdlingTimeInSec: Int = 1800,
                                        maxExtraTravelTimeInSec: Int = 900,
                                        maxTaskDurationInSec: Int = 300): Array[TransferFunction] ={

    def randomVehicleSelection = random.nextInt(v)
    def randomIdleTime = random.nextInt(maxIdlingTimeInSec)
    def randomExtraTravelTime = random.nextInt(maxExtraTravelTimeInSec)
    def randomTaskDuration = random.nextInt(maxTaskDurationInSec)

    val precedencesWithLonelyNodes = addLonelyNodesToChains(n, v, precedences)
    val endOfLastActionOfVehicles = Array.fill[Long](v)(0L)
    val lastNodeVisitedOfVehicles = Array.tabulate(v)(x => x)
    val extraTravelTimeOfNodes = Array.tabulate(n)(node => if(node < v)0L else randomExtraTravelTime)
    val earlyLines = Array.fill[Long](n)(0L)
    val deadLines = Array.fill[Long](n)(Long.MaxValue)
    val taskDurations = Array.tabulate[Long](n)(node => if(node < v) 0L else randomTaskDuration)

    /**
      * This method generate a time window for a given node on a given vehicle
      * It's divided in several step :
      *   1°  We add the travel time from the previous node to the current node to the endOfLastActionOfVehicles
      *   2°  We set the earliestArrivalTime of the node
      *   3°  We add the taskDuration of the node to the endOfLastActionOfVehicles
      *   4°  We set the latestLeavingTime of the node by adding a random extraTravelTime and
      *       the random extraTravelTime from the previous node to the endOfLastActionOfVehicles
      *   5°  We update the last node visited by the vehicle
      * @param node the reference of the node
      * @param onVehicle the reference of the vehicle
      */
    def generateTimeWindowForNode(node: Int, onVehicle: Int): Unit = {
      val previousNode = lastNodeVisitedOfVehicles(onVehicle)
      val travelFromPreviousNode = timeMatrix(previousNode)(node)
      endOfLastActionOfVehicles(onVehicle) += travelFromPreviousNode

      earlyLines(node) = endOfLastActionOfVehicles(onVehicle)
      endOfLastActionOfVehicles(onVehicle) += taskDurations(node)
      deadLines(node) = endOfLastActionOfVehicles(onVehicle) + extraTravelTimeOfNodes(node) + extraTravelTimeOfNodes(previousNode)
      lastNodeVisitedOfVehicles(onVehicle) = node
    }

    for(precedence <- precedencesWithLonelyNodes){
      val vehicle = randomVehicleSelection
      //Add some idle time
      endOfLastActionOfVehicles(vehicle) += randomIdleTime
      for(node <- precedence){
        generateTimeWindowForNode(node,vehicle)
      }
    }
    Array.tabulate(n)(node =>
      TransferFunction.createFromEarliestAndLatestArrivalTime(node,
        earlyLines(node),
        deadLines(node) - taskDurations(node),
        taskDurations(node)))
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
  def generateMaxTravelDurations(precedences: List[List[Int]],
                                 earliestArrivalTimes: Array[Long],
                                 travelDurationMatrix: Array[Array[Long]]): List[(Int, Int, Long)] ={
    @tailrec
    def maxTravelDurationOfPrecedence(from: Int, toProceed: List[Int], maxTravelDurations: List[(Int,Int,Long)]): List[(Int,Int,Long)] ={
      toProceed match{
        case Nil => maxTravelDurations
        case to::Nil => List((from, to,(travelDurationMatrix(from)(to)*1.5).toLong)) ::: maxTravelDurations
        case to::tail => maxTravelDurationOfPrecedence(to, tail,List((from, to,(travelDurationMatrix(from)(to)*1.5).toLong)) ::: maxTravelDurations)
      }
    }

    precedences.flatMap(p => maxTravelDurationOfPrecedence(p.head,p.tail,List.empty))
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
    for (i <- 0 until n)
      for (j <- 0 until n)
        ttf.setTTF(i, j, new TTFConst((distanceMatrix(i)(j)*multiplier).toInt))
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
  def generateContentFlow(n: Int, precedences: List[List[Int]], maxVehicleSize: Int): Array[Long] ={
    def randomContent(currentMaxContent: Int) = random.nextInt(currentMaxContent)
    def isPickupStep: Boolean = random.nextBoolean()

    val contentFlow = Array.fill(n)(0)
    for(precedence <- precedences){
      var currentPrecedenceContent = 0
      for (node <- precedence){
        if(node == precedence.head) {
          contentFlow(node) = randomContent(maxVehicleSize)+1
          currentPrecedenceContent += contentFlow(node)
        } else if(node == precedence.last) {
          contentFlow(node) = -currentPrecedenceContent
        } else {
          contentFlow(node) =
            if(isPickupStep) randomContent(maxVehicleSize-currentPrecedenceContent)+1
            else -randomContent(currentPrecedenceContent)
          currentPrecedenceContent += contentFlow(node)
        }
      }
    }
    contentFlow.map(_.toLong)
  }

  /**
   * Generate the content flow of all nodes considering the fact that we have multiple content type.
   * Each content type is transported by specifics vehicles.
   * Each precedence is linked to a specific content type.
   * e.g.: 1 -> 2 -> 3       ==> Get some apples at 1, deliver some of them at 2 and the rest at 3
   *
   * @param n                        The total number of nodes
   * @param c                        The number of content type
   * @param precedences              The precedences
   * @param maxContentPerContentType The maximum content of a certain type picked-up for one precedence.
   * @return
   */
  def generateMultipleContentsFlow(n: Int, c: Int, precedences: List[List[Int]], maxContentPerContentType: Array[Long]): Array[Array[Long]] = {
    require(maxContentPerContentType.length == c)

    def randomContentType = random.nextInt(c)

    def randomContent(currentMaxContent: Long) = random.nextLong(currentMaxContent)

    def isPickupStep: Boolean = random.nextBoolean()

    val contentFlow = Array.fill(n)(Array.fill(c)(0L))
    for (precedence <- precedences) {
      var currentPrecedenceContent = 0L
      val contentType = randomContentType
      for (node <- precedence) {
        if (node == precedence.head) {
          contentFlow(node)(contentType) = randomContent(maxContentPerContentType(contentType)) + 1
          currentPrecedenceContent += contentFlow(node)(contentType)
        } else if (node == precedence.last) {
          contentFlow(node)(contentType) = -currentPrecedenceContent
        } else {
          contentFlow(node)(contentType) =
            if (isPickupStep) randomContent(maxContentPerContentType(contentType) - currentPrecedenceContent) + 1
            else -randomContent(currentPrecedenceContent)
          currentPrecedenceContent += contentFlow(node)(contentType)
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
  def generateVehiclesSize(v: Int, maxVehicleSize: Int, minVehicleSize: Int = 1): Array[Long] ={
    Array.fill(v)(Math.max(random.nextInt(maxVehicleSize)+1,minVehicleSize).toLong)
  }

  def generateMultipleContentVehicleSize(v: Int, c: Int, minVehicleSize: Long, maxVehicleSizePerContentType: Array[Long], contentTypePerVehicle: Int): Array[Array[Long]] = {
    val contentTypeOfVehicle: Array[Set[Int]] = Array.tabulate(v)(vehicle => HashSet(vehicle % c) ++ Set.fill(contentTypePerVehicle - 1)(random.nextInt(c)))
    Array.tabulate(v)(vehicle => {
      val res = Array.fill(c)(0L)
      for (ct <- contentTypeOfVehicle(vehicle)) res(ct) = Math.max(minVehicleSize, random.nextLong(maxVehicleSizePerContentType(ct)))
      res
    })
  }
}
