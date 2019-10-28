package oscar.cbls.benchmarks
/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

import oscar.cbls._
import oscar.cbls.lib.search.combinators.Profile
import oscar.cbls.lib.search.neighborhoods.WideningFlipNeighborhood
import oscar.cbls.modeling.CBLSModel

import scala.collection.immutable.SortedMap
import scala.language.postfixOps
import scala.util.Random

/**
 * Created by rdl on 29L-01L-1L6.
 */
object CarSequencerBench extends CBLSModel with App {

  val orderedCarsByType: SortedMap[Long,Long] = SortedMap(0L -> 110L, 1L -> 60L, 2L -> 110L , 3L -> 120L, 4L -> 40L, 5L -> 30L)

  val nbCarTypes = 6
  val carTypes: Domain = (0,nbCarTypes-1)

  println("carSequencing")
  println("orderedCarTypes:" + orderedCarsByType)

  //option types
  //   A   G   D   E  (airConditionner, automaticGearBox, diesel, esp)
  //0L  T   T   T   F
  //1L  F   T   T   F
  //2L  T   F   T   F
  //3L  F   F   F   T
  //4L  T   T   F   T
  //5L  F   T   F   T

  def makeBoolArray(values:Long*):Array[Boolean] = {
    val toReturn = Array.fill(nbCarTypes)(false)
    values.foreach(toReturn(_) = true)
    toReturn
  }

  val airCoCarTypes = makeBoolArray(0L,2L,4L)
  val automaticGearBoxCarTypes = makeBoolArray(0L,1L,4L,5L)
  val dieselCarTypes = makeBoolArray(0L,1L,2L)
  val espCarTypes = makeBoolArray(3L,4L,5L)

  def prependItems(acc:List[Long],n:Long,item:Long):List[Long] = if(n == 0L) acc else prependItems(item :: acc,n-1L,item)
  val orderedCarTypes:List[Long] = orderedCarsByType.foldLeft(List.empty[Long])({case (accList,(carType,nbItems)) => prependItems(accList,nbItems,carType)})
  val nbCars = orderedCarTypes.size

  println("totalNumberOfCars:" + nbCars)

  //initializes the car sequence in a random way
  val orderedCarTypesIterator = Random.shuffle(orderedCarTypes).toIterator
  val carSequence:Array[CBLSIntVar] = Array.tabulate(nbCars)(p => CBLSIntVar(orderedCarTypesIterator.next(),carTypes,"carClassAtPosition" + p))

  //airConditionner: max 2L out of 3L
  c.post(sequence(carSequence,3L,2L,airCoCarTypes))

  //automaticGearBox: max 3L out of 5L
  c.post(sequence(carSequence,5L,3L,automaticGearBoxCarTypes))

  //diesel: max 3L out of 5L
  c.post(sequence(carSequence,5L,3L,dieselCarTypes))

  //esp: max 2L ouf of 3L
  c.post(sequence(carSequence,3L,2L,espCarTypes))

  val varViolation = c.violations(carSequence)
  val violatedCars = filter(varViolation)
  val mostViolatedCars = argMax(varViolation)

  println("closing model")

  c.close()

  val obj:Objective = c.violation

  s.close()

  val search =
    (Profile(swapsNeighborhood(carSequence,"mostViolatedSwap", searchZone2 = () => (_,_) => mostViolatedCars.value, symmetryCanBeBrokenOnIndices = false))
      exhaust Profile(WideningFlipNeighborhood(carSequence)) //it seems useless to try swaps once flip is exhausted, so simple exhaust is used here
      onExhaustRestartAfter(Profile(shuffleNeighborhood(carSequence, mostViolatedCars, name = "shuffleMostViolatedCars")) guard(() => mostViolatedCars.value.size > 2L), 2L, obj)
      onExhaustRestartAfter(Profile(shuffleNeighborhood(carSequence, violatedCars, name = "shuffleSomeViolatedCars", numberOfShuffledPositions = () => 5L max (violatedCars.value.size/2L))), 2L, obj)
      onExhaustRestartAfter(Profile(shuffleNeighborhood(carSequence, name = "shuffleMostCars", numberOfShuffledPositions = () => nbCars/2L)), 2L, obj)
      orElse (Profile(shuffleNeighborhood(carSequence, name = "shuffleAllCars")) maxMoves 4L)
      saveBestAndRestoreOnExhaust obj) //in case we do not solve it, we want to restore the best solution anyway

  search.verbose = 1
  search.doAllMoves(_ => c.isTrue,obj)

  println(search.profilingStatistics)

  println("car sequence:" + carSequence.map(_.value).mkString(","))

  println(if (c.violation.value == 0L) "problem solved" else "PROBLEM COULD NOT BE SOLVED: " + c.violation)
}
